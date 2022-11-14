// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::AppErrorCode::MissingDefAndRef;
use crate::config::{AppErrorCode, ErrorConfig, WithErrorLevel};
use crate::core::{
    MagicComment, MagicCommentKindData, SourceRange, Span, SyntaxData, WithSpan, SPECIAL_COMMENT_RE,
};
use crate::utils::AdjustOffsets;
use crate::{utils, AppError, AppErrorData};
use miette::SourceCode;
use nom::{
    branch::alt,
    bytes::{complete::is_not, complete::tag},
    character::{complete::char as nom_char, complete::multispace0},
    combinator::map,
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, separated_pair},
    IResult,
};
use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::{
    collections::hash_map::Entry,
    path::PathBuf,
    sync::{mpsc, Arc},
};

//--------------------------------------------------------------------------------------------------
// Main logic

pub struct Options {
    pub root: PathBuf,
    pub error_config: ErrorConfig,
}

pub type FrontendError = WithErrorLevel<FrontendErrorData>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FrontendErrorData {
    MissingId { range: SourceRange },
}

impl AdjustOffsets for FrontendErrorData {
    fn adjust_offsets(self, base_offset: usize) -> FrontendErrorData {
        match self {
            FrontendErrorData::MissingId { range } => FrontendErrorData::MissingId {
                range: range.adjust_offsets(base_offset),
            },
        }
    }
}

impl Display for FrontendErrorData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            FrontendErrorData::MissingId { .. } =>
                f.write_str("missing both 'def:' and 'ref:' keys in magic comment; this will prevent cross-referencing"),
        }
    }
}

impl Error for FrontendErrorData {}

impl miette::Diagnostic for FrontendErrorData {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            FrontendErrorData::MissingId { .. } => Some(Box::new(MissingDefAndRef)),
        }
    }
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            FrontendErrorData::MissingId { .. } => Some(Box::new(
                "add 'def: unique-id' or a 'ref: unique-id' to define/reference a magic comment",
            )),
        }
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        match &self {
            FrontendErrorData::MissingId { range } => Some(Box::new(
                [miette::LabeledSpan::new_with_span(None, range.span)].into_iter(),
            )),
        }
    }
    fn source_code(&self) -> Option<&dyn SourceCode> {
        match &self {
            FrontendErrorData::MissingId { range } => Some(range),
        }
    }
}

pub fn gather(options: &Options) -> (SyntaxData, BTreeSet<AppError>) {
    let mut override_builder = ignore::overrides::OverrideBuilder::new(&options.root);
    override_builder
        .add("!**/.git")
        .expect("failed to build override");

    // Don't ignore hidden directories by default, because that will miss directories
    // like .buildkite and .github, which often have config information that is connected
    // to something else.
    let walker = ignore::WalkBuilder::new(options.root.clone())
        .hidden(false)
        .overrides(override_builder.build().expect("failed to build Override"))
        .build_parallel();
    // TODO: Clone the regex per thread?
    let mut builder = FileVisitorBuilder::new(options.error_config.clone());
    walker.visit(&mut builder);
    let (comment_data, errors) = builder.aggregate_results();
    return (SyntaxData::merge_all(comment_data), errors);
}

type FileVisitorOutput = (SyntaxData, Vec<FrontendError>);

struct FileVisitorBuilder {
    alive_workers_count: u32,
    error_config: Arc<ErrorConfig>,
    source: mpsc::Receiver<FileVisitorOutput>,
    sink: mpsc::Sender<FileVisitorOutput>,
}

impl FileVisitorBuilder {
    fn new(cfg: ErrorConfig) -> FileVisitorBuilder {
        let (tx, rx) = mpsc::channel::<FileVisitorOutput>();
        FileVisitorBuilder {
            alive_workers_count: 0,
            error_config: Arc::new(cfg),
            source: rx,
            sink: tx,
        }
    }
    fn aggregate_results(mut self) -> (Vec<SyntaxData>, BTreeSet<AppError>) {
        let mut out = (vec![], BTreeSet::default());
        loop {
            match self.source.recv() {
                Ok((data, err)) => {
                    out.0.push(data);
                    out.1.extend(
                        err.into_iter()
                            .map(|fe| fe.map(&AppErrorData::Frontend) as AppError),
                    );
                    self.alive_workers_count -= 1;
                    if self.alive_workers_count == 0 {
                        return out;
                    }
                }
                Err(_) => {}
            }
        }
    }
}

impl ignore::ParallelVisitorBuilder<'_> for FileVisitorBuilder {
    fn build(&mut self) -> Box<dyn ignore::ParallelVisitor> {
        self.alive_workers_count += 1;
        return Box::new(FileVisitor {
            error_config: self.error_config.clone(),
            data: SyntaxData::default(),
            errors: vec![],
            sink: self.sink.clone(),
        });
    }
}

struct FileVisitor {
    data: SyntaxData,
    errors: Vec<FrontendError>,
    error_config: Arc<ErrorConfig>,
    sink: mpsc::Sender<(SyntaxData, Vec<FrontendError>)>,
}

impl Drop for FileVisitor {
    fn drop(&mut self) {
        let mut data = SyntaxData::default();
        let mut errs = vec![];
        std::mem::swap(&mut data, &mut self.data);
        std::mem::swap(&mut errs, &mut self.errors);
        self.sink
            .send((data, errs))
            .expect("failed to write to channel");
    }
}

impl ignore::ParallelVisitor for FileVisitor {
    fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> ignore::WalkState {
        match entry {
            Err(_) => {
                return ignore::WalkState::Skip;
            }
            Ok(entry) => {
                match entry.file_type() {
                    None => return ignore::WalkState::Skip, // stdin
                    Some(file_type) if file_type.is_dir() => return ignore::WalkState::Continue,
                    _ => {}
                }
                if mime_guess::from_path(entry.path())
                    .first_or_text_plain()
                    .type_()
                    == "text"
                {
                    match std::fs::read_to_string(entry.path()) {
                        Err(_) => return ignore::WalkState::Skip,
                        Ok(contents) => {
                            if SPECIAL_COMMENT_RE.find(&contents).is_some() {
                                self.extract_magic_comments(entry.path().to_owned(), contents);
                            }
                            return ignore::WalkState::Continue;
                        }
                    }
                }
                return ignore::WalkState::Skip;
            }
        }
    }
}

impl FileVisitor {
    fn extract_magic_comments(&mut self, path: PathBuf, contents: String) {
        let path = Arc::new(path);
        let mut comments = vec![];
        let contents = Arc::new(contents);
        for re_match in SPECIAL_COMMENT_RE.find_iter(&contents) {
            let range = SourceRange {
                path: path.clone(),
                span: Span::new(re_match.start(), re_match.end()),
                contents: utils::AllEquivalent {
                    value: contents.clone(),
                },
            };
            match parse_magic_comment(re_match.as_str(), &self.error_config, &range) {
                Err(fe_errors) => {
                    self.errors.extend(fe_errors.into_iter());
                }
                Ok(magic_comment) => {
                    let span = range.span;
                    let comment = Arc::new(magic_comment);
                    match self.data.comment_to_range_map.entry(comment.clone()) {
                        Entry::Occupied(mut o) => o.get_mut().push(range),
                        Entry::Vacant(v) => {
                            v.insert(vec![range]);
                        }
                    }
                    comments.push(WithSpan {
                        span,
                        value: comment.clone(),
                    });
                }
            }
        }
        self.data.path_to_comment_map.insert(path.clone(), comments);
        self.data.path_to_content_map.insert(path.clone(), contents);
    }
}
//--------------------------------------------------------------------------------------------------
// Parsing logic

fn parse_magic_comment(
    input: &str,
    error_config: &Arc<ErrorConfig>,
    base_range: &SourceRange,
) -> Result<MagicComment, Vec<FrontendError>> {
    let _original_input_len = input.len();
    let (input, kind) = parse_kind_keyword(input).expect("Incorrect prefix matching in regex");
    let (input, kv_pairs) = delimited(nom_char('('), parse_attribute_list, nom_char(')'))(input)
        .expect("Mismatch between regex & parser combinator causing internal failure");
    parse_end(input).expect("Expected end with ) based on regex");
    let mut magic_comment = MagicComment::default();
    magic_comment.kind = kind;
    // TODO(def: missing-test): Add tests for different warnings/errors.
    // TODO(def: missing-warnings): Emit warnings/errors for different cases.
    let mut errs = vec![];
    for (k, v) in kv_pairs {
        use MagicCommentKindData as Kind;
        // TODO(def: issue-duplicate-error): Repeating an attribute is an error.
        if k == "def" {
            magic_comment.id = v.to_owned();
            magic_comment.is_def = true;
        } else if k == "ref" {
            magic_comment.id = v.to_owned();
            magic_comment.is_def = false;
        } else if k == "issue" {
            match &mut magic_comment.kind {
                Kind::Todo { issue } => *issue = Some(v.to_owned()),
                Kind::Fixme { issue } => *issue = Some(v.to_owned()),
                Kind::Note | Kind::Warning | Kind::Review { .. } => {}
            }
        } else if k == "from" {
            match &mut magic_comment.kind {
                Kind::Review { reviewer_id } => *reviewer_id = Some(v.to_owned()),
                Kind::Note | Kind::Warning | Kind::Todo { .. } | Kind::Fixme { .. } => {}
            }
        }
    }
    if magic_comment.id.is_empty() {
        if let Some(level) = error_config.get_level(AppErrorCode::MissingDefAndRef) {
            let err = WithErrorLevel {
                level,
                error: FrontendErrorData::MissingId {
                    range: base_range.clone(),
                },
            };
            errs.push(err);
        }
    }
    if errs.is_empty() {
        return Ok(magic_comment);
    }
    return Err(errs);
}

fn parse_kind_keyword(input: &str) -> IResult<&str, MagicCommentKindData> {
    alt((
        map(tag("NOTE"), |_| MagicCommentKindData::Note),
        map(tag("WARNING"), |_| MagicCommentKindData::Warning),
        map(tag("TODO"), |_| MagicCommentKindData::Todo { issue: None }),
        map(tag("FIXME"), |_| MagicCommentKindData::Fixme {
            issue: None,
        }),
        map(tag("REVIEW"), |_| MagicCommentKindData::Review {
            reviewer_id: None,
        }),
    ))(input)
}

#[cfg(test)]
mod tests {
    use crate::frontend::*;
    #[test]
    fn check_attr_parse() {
        assert!(parse_attribute("issue: https://xxx").is_ok());
        assert!(parse_attribute_list("a: bcd, e: https://").is_ok());
    }
}

pub(crate) fn parse_attribute_list(
    input: &str,
) -> IResult<&str, impl Iterator<Item = (&str, &str)>> {
    let (input, v) = separated_list0(
        nom_char(','),
        alt((map(parse_attribute, Ok), map(parse_attr_value, Err))),
    )(input)?;
    return Ok((input, v.into_iter().filter_map(|kv| kv.ok())));
}

fn parse_end(input: &str) -> IResult<&str, &str> {
    nom::combinator::eof(input)
}

fn parse_attribute(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(parse_attr_key, nom_char(':'), parse_attr_value)(input)
}

fn parse_attr_key(input: &str) -> IResult<&str, &str> {
    parse_surrounding_space(is_not(":,)"))(input)
}

fn parse_attr_value(input: &str) -> IResult<&str, &str> {
    parse_surrounding_space(is_not(",)"))(input)
}

fn parse_surrounding_space<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}
