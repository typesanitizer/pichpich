// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::core::{
    MagicComment, MagicCommentKindData, SourceRange, Span, SyntaxData, WithSpan, SPECIAL_COMMENT_RE,
};
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
use pichpich_config::AppErrorCode::{
    ConflictingKeys, EmptyAttributeKeyOrValue, MalformedAttribute, MissingDefAndRef,
    UnexpectedSpaces, UnknownAttributeKey,
};
use pichpich_config::{AppErrorCode, ErrorConfig, WithErrorLevel};
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
    pub respect_ignore_file: bool,
    pub error_config: ErrorConfig,
}

pub type FrontendError = WithErrorLevel<FrontendErrorData>;

// Sorting will group errors by kind instead of range... bad idea?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FrontendErrorData {
    MissingId {
        range: SourceRange,
    },
    // Invariant: at least one of empty_key or empty_value must be true
    EmptyAttributeKeyOrValue {
        empty_key: bool,
        empty_value: bool,
        range: SourceRange,
    },
    MalformedAttribute {
        range: SourceRange,
    },
    UnknownAttributeKey {
        leading_text: &'static str, // kinda' redundant but whatever
        comment_range: SourceRange,
        key_spans: Vec<Span>, // invariant: Vec is non-empty
    },
    ConflictingKeys {
        comment_range: SourceRange,
        key_spans: Vec<Span>, // invariant: Vec is non-empty
    },
    UnexpectedSpaces {
        comment_range: SourceRange,
        value_spans: Vec<Span>, // invariant: Vec is non-empty
    },
}

impl Display for FrontendErrorData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            FrontendErrorData::MissingId { .. } =>
                f.write_str("missing both 'def:' and 'ref:' keys in magic comment; this will prevent cross-referencing"),
            FrontendErrorData::EmptyAttributeKeyOrValue { empty_key, empty_value, range } => {
                match (empty_key, empty_value) {
                    (true, true) => f.write_str("missing key and value around ':'"),
                    (true, false) => f.write_str("missing key before ':'"),
                    (false, true) => f.write_fmt(format_args!("missing value after ':' for key {}",
                        range.slice().trim_end_matches(':').trim()
                    )),
                    (false, false) => unreachable!("either empty_key or empty_value should be true"),
                }
            }
            FrontendErrorData::MalformedAttribute { .. } =>
                f.write_str("found attribute not in 'key: value' format"),
            FrontendErrorData::UnknownAttributeKey { leading_text, comment_range, key_spans: spans } => {
                let key_text: Vec<_> = spans.iter().map(|span|
                    format!("'{}'", SourceRange { span: *span, .. comment_range.clone() }.slice())
                ).collect();
                f.write_fmt(format_args!("found unknown attribute key{} {} for {}",
                    if key_text.len() > 1 { "s" } else { "" },
                    crate::format_utils::format_list(&key_text, "and"),
                    leading_text))
            }
            FrontendErrorData::ConflictingKeys { comment_range, key_spans: spans } => {
                let range = SourceRange { span: spans[0], .. comment_range.clone() };
                let key = range.slice();
                let all_same = spans[1..].iter().all(|span|
                    SourceRange { span: *span, .. comment_range.clone() }.slice() == key
                );
                if all_same {
                    f.write_fmt(format_args!("key '{}' is repeated {} times", key, spans.len()))
                } else {
                    let keys = spans.iter().map(|s| format!("'{}'", SourceRange { span: *s, .. comment_range.clone() }.slice()))
                        .collect::<Vec<_>>();
                    assert!(keys.len() >= 2);
                    f.write_fmt(format_args!("keys {} cannot be used simultaneously",
                        crate::format_utils::format_list(&keys, "and")
                    ))
                }
            }
            FrontendErrorData::UnexpectedSpaces { comment_range, value_spans } => {
                let value_text : Vec<_> = value_spans.iter().map(|span|
                    format!("'{}'", SourceRange { span: *span, .. comment_range.clone() }.slice())
                ).collect();
                f.write_fmt(format_args!("found unquoted value{} {} with whitespace",
                     if value_text.len() > 1 { "s" } else { "" },
                     crate::format_utils::format_list(&value_text, "and")))
            }
        }
    }
}

impl Error for FrontendErrorData {}

impl miette::Diagnostic for FrontendErrorData {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            FrontendErrorData::MissingId { .. } => Some(Box::new(MissingDefAndRef)),
            FrontendErrorData::EmptyAttributeKeyOrValue { .. } => {
                Some(Box::new(EmptyAttributeKeyOrValue))
            }
            FrontendErrorData::MalformedAttribute { .. } => Some(Box::new(MalformedAttribute)),
            FrontendErrorData::UnknownAttributeKey { .. } => Some(Box::new(UnknownAttributeKey)),
            FrontendErrorData::ConflictingKeys { .. } => Some(Box::new(ConflictingKeys)),
            FrontendErrorData::UnexpectedSpaces { .. } => Some(Box::new(UnexpectedSpaces)),
        }
    }
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            FrontendErrorData::MissingId { .. } => Some(Box::new(
                "add 'def: unique-id' or a 'ref: unique-id' to define/reference a magic comment",
            )),
            FrontendErrorData::EmptyAttributeKeyOrValue { .. } => None,
            FrontendErrorData::MalformedAttribute { .. } => None,
            FrontendErrorData::UnknownAttributeKey { .. } => None,
            // FIXME(def: provide-attr-hint): We should at least list out common attributes here.
            FrontendErrorData::ConflictingKeys { .. } => {
                Some(Box::new("keys can only be specified once per comment"))
            }
            FrontendErrorData::UnexpectedSpaces { .. } => Some(Box::new(
                "add explicit surrounding quotes \"...\" or remove whitespace",
            )),
        }
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        match &self {
            FrontendErrorData::MissingId { range } => Some(Box::new(
                [miette::LabeledSpan::new_with_span(None, range.span)].into_iter(),
            )),
            FrontendErrorData::EmptyAttributeKeyOrValue { range, .. } => Some(Box::new(
                [miette::LabeledSpan::new_with_span(None, range.span)].into_iter(),
            )),
            FrontendErrorData::MalformedAttribute { range } => Some(Box::new(
                [miette::LabeledSpan::new_with_span(None, range.span)].into_iter(),
            )),
            FrontendErrorData::UnknownAttributeKey {
                key_spans: spans, ..
            } => Some(Box::new(
                spans
                    .iter()
                    .map(|span| miette::LabeledSpan::new_with_span(None, *span)),
            )),
            FrontendErrorData::ConflictingKeys {
                key_spans: spans, ..
            } => Some(Box::new(
                spans
                    .iter()
                    .map(|span| miette::LabeledSpan::new_with_span(None, *span)),
            )),
            FrontendErrorData::UnexpectedSpaces {
                value_spans: spans, ..
            } => Some(Box::new(
                spans
                    .iter()
                    .map(|span| miette::LabeledSpan::new_with_span(None, *span)),
            )),
        }
    }
    fn source_code(&self) -> Option<&dyn SourceCode> {
        match &self {
            FrontendErrorData::MissingId { range } => Some(range),
            FrontendErrorData::EmptyAttributeKeyOrValue { range, .. } => Some(range),
            FrontendErrorData::MalformedAttribute { range } => Some(range),
            FrontendErrorData::UnknownAttributeKey { comment_range, .. } => Some(comment_range),
            FrontendErrorData::ConflictingKeys { comment_range, .. } => Some(comment_range),
            FrontendErrorData::UnexpectedSpaces { comment_range, .. } => Some(comment_range),
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
    let mut builder = ignore::WalkBuilder::new(options.root.clone());
    let walker = builder
        .hidden(false)
        .overrides(override_builder.build().expect("failed to build Override"));
    let walker = if options.respect_ignore_file {
        walker.add_custom_ignore_filename(".pichpich-ignore")
    } else {
        walker
    };
    let walker = walker.build_parallel();
    // TODO(def: regex-clone): Does cloning the regex for each thread provide better
    // search performance?
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
        while self.alive_workers_count != 0 {
            match self.source.recv() {
                Ok((data, err)) => {
                    out.0.push(data);
                    out.1.extend(
                        err.into_iter()
                            .map(|fe| fe.map(&AppErrorData::Frontend) as AppError),
                    );
                    self.alive_workers_count -= 1;
                }
                Err(_) => {} // A receiver closed; that's OK.
            }
        }
        return out;
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
            match parse_magic_comment(&self.error_config, &range) {
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
        self.data.path_to_content_map.insert(path, contents);
    }
}
//--------------------------------------------------------------------------------------------------
// Parsing logic

fn parse_magic_comment(
    error_config: &Arc<ErrorConfig>,
    base_range: &SourceRange,
) -> Result<MagicComment, Vec<FrontendError>> {
    let input = base_range.slice();
    let (input, kind) = parse_kind_keyword(input).expect("Incorrect prefix matching in regex");
    let (input, kv_pairs) = delimited(nom_char('('), parse_attribute_list, nom_char(')'))(input)
        .expect("Mismatch between regex & parser combinator causing internal failure");
    parse_end(input).expect("Expected end with ) based on regex");
    let mut magic_comment = MagicComment::default();
    magic_comment.kind = kind;
    // TODO(def: batch-errors): Maybe we should create a Vec first, and then do iterations
    // over it for accumulating state. This would allow to easily batch errors of the same
    // kind together.
    let mut errs = vec![];
    let mut good_attrs = vec![];
    // Filter out bad attributes first
    for kv in kv_pairs {
        match kv {
            Ok((k, v)) => good_attrs.push((k, v)),
            Err(bad_attr) => {
                diagnose_bad_attribute_errors(error_config, base_range, bad_attr, &mut errs);
            }
        }
    }
    populate_attrs(
        &mut magic_comment,
        error_config,
        base_range,
        good_attrs,
        &mut errs,
    );
    // See NOTE(ref: magic-comment-lookalike)
    if magic_comment.id.is_empty() {
        if let Some(level) = error_config.get_level(AppErrorCode::MissingDefAndRef) {
            let err = level.attach_to(FrontendErrorData::MissingId {
                range: base_range.clone(),
            });
            errs.push(err);
        }
    }
    if errs.is_empty() {
        return Ok(magic_comment);
    }
    return Err(errs);
}

fn populate_attrs(
    comment: &mut MagicComment,
    error_config: &Arc<ErrorConfig>,
    base_range: &SourceRange,
    good_attrs: Vec<(&str, &str)>,
    errs: &mut Vec<FrontendError>,
) {
    let mut bad_key_spans = Vec::new();
    let mut key_span_map = Vec::<(&str, Vec<Span>)>::new();
    let mut value_spaces_spans = Vec::new();
    for (key, value) in good_attrs {
        use MagicCommentKindData as Kind;
        if let Some(_) = error_config.get_level(UnexpectedSpaces) {
            if !(value.starts_with('"') && value.ends_with('"'))
                && value.contains(|c: char| c.is_whitespace())
            {
                value_spaces_spans.push(Span::for_subslice(base_range, value));
            }
        }
        match key {
            "def" | "ref" => {
                comment.id = value.to_owned();
                comment.is_def = key == "def";
            }
            "issue" => match &mut comment.kind {
                Kind::Todo { issue } => *issue = Some(value.to_owned()),
                Kind::Fixme { issue } => *issue = Some(value.to_owned()),
                Kind::Note | Kind::Warning | Kind::Review { .. } => {
                    diagnose_unknown_attr_key(error_config, base_range, key, &mut bad_key_spans);
                }
            },
            "from" => match &mut comment.kind {
                Kind::Review { reviewer_id } => *reviewer_id = Some(value.to_owned()),
                Kind::Note | Kind::Warning | Kind::Todo { .. } | Kind::Fixme { .. } => {
                    diagnose_unknown_attr_key(error_config, base_range, key, &mut bad_key_spans);
                }
            },
            _ => diagnose_unknown_attr_key(error_config, base_range, key, &mut bad_key_spans),
        }
        if let Some(_) = error_config.get_level(ConflictingKeys) {
            let span = Span::for_subslice(base_range, key);
            // Technically quadratic, but we should only have a few attributes.
            if let Some(spans) = key_span_map.iter_mut().find_map(|(k, vs)| {
                if k == &key || (key == "def" && k == &"ref") || (key == "ref" && k == &"def") {
                    Some(vs)
                } else {
                    None
                }
            }) {
                spans.push(span);
            } else {
                key_span_map.push((key, vec![span]));
            }
        }
    }
    if !bad_key_spans.is_empty() {
        let level = error_config
            .get_level(UnknownAttributeKey)
            .expect("filled bad_key_spans even though UnknownAttributeKey is set to ignore");
        errs.push(level.attach_to(FrontendErrorData::UnknownAttributeKey {
            leading_text: comment.kind.leading_text(),
            comment_range: base_range.clone(),
            key_spans: bad_key_spans,
        }));
    }
    if !key_span_map.is_empty() {
        let level = error_config
            .get_level(ConflictingKeys)
            .expect("filled key_span_map even though RepeatedKey is set to ignore");
        for (_, spans) in key_span_map.into_iter() {
            assert!(!spans.is_empty());
            if spans.len() == 1 {
                continue;
            }
            errs.push(level.attach_to(FrontendErrorData::ConflictingKeys {
                comment_range: base_range.clone(),
                key_spans: spans,
            }));
        }
    }
    if !value_spaces_spans.is_empty() {
        let level = error_config
            .get_level(UnexpectedSpaces)
            .expect("filled value_spaces_span even though UnexpectedSpaces is set to ignore");
        errs.push(level.attach_to(FrontendErrorData::UnexpectedSpaces {
            comment_range: base_range.clone(),
            value_spans: value_spaces_spans,
        }))
    }
}

fn diagnose_unknown_attr_key(
    error_config: &Arc<ErrorConfig>,
    base_range: &SourceRange,
    key: &str,
    bad_key_spans: &mut Vec<Span>,
) {
    // TODO(def: special-case-urls): If you add a URL field directly, you'll get a not-so-great
    // error, because (e.g.) 'https' will be interpreted as the key. We should emit a special
    // diagnostic in that situation.
    if let Some(_) = error_config.get_level(UnknownAttributeKey) {
        bad_key_spans.push(Span::for_subslice(base_range, key));
    }
}

fn diagnose_bad_attribute_errors(
    error_config: &Arc<ErrorConfig>,
    base_range: &SourceRange,
    bad_attr: &str,
    errs: &mut Vec<FrontendError>,
) {
    let empty_key = bad_attr.starts_with(':');
    let empty_value = bad_attr.ends_with(':');
    let span = Span::for_subslice(base_range, bad_attr);
    if empty_key || empty_value {
        if let Some(level) = error_config.get_level(EmptyAttributeKeyOrValue) {
            errs.push(
                level.attach_to(FrontendErrorData::EmptyAttributeKeyOrValue {
                    empty_key,
                    empty_value,
                    range: SourceRange {
                        span,
                        ..base_range.clone()
                    },
                }),
            );
        }
    } else if let Some(level) = error_config.get_level(MalformedAttribute) {
        errs.push(level.attach_to(FrontendErrorData::MalformedAttribute {
            range: SourceRange {
                span,
                ..base_range.clone()
            },
        }));
    }
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
) -> IResult<&str, impl Iterator<Item = Result<(&str, &str), &str>>> {
    let (input, v) = separated_list0(
        nom_char(','),
        alt((map(parse_attribute, Ok), map(parse_attr_value, Err))),
    )(input)?;
    return Ok((input, v.into_iter()));
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
