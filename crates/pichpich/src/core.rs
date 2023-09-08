// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::utils::AdjustOffsets;
use crate::{
    format_utils,
    miette_utils::SpanContentsWithPathAdapter,
    utils::{merge_map_vec, AllEquivalent},
};
use lazy_static::lazy_static;
use miette::MietteError;
use regex::Regex;
use serde::ser::SerializeStruct;
use std::ops::Range;
use std::{collections::HashMap, hash::Hash, path::PathBuf, sync::Arc};

lazy_static! {
    // TODO(def: closing-paren-separate-line, issue: https://github.com/typesanitizer/pichpich/issues/27)
    // The regex matching here requires that a magic comment's key-value
    // pairs do not contain ), and that the ) be on the same line as the
    // opening (. (Not 100% sure about multi-line...). However, for more
    // complex magic comments, we could have something like:
    // BLAH(def: my-long-id,
    //      author: l33t_hackerman_42069,
    //      issue: https://github.com/myproject/foobar/issues/1)

    // TODO(def: sync-support): Add support for SYNC comments with blocks
    pub(crate) static ref SPECIAL_COMMENT_RE: Regex =
        Regex::new(r"(NOTE|WARNING|TODO|FIXME|REVIEW)\([^\)]*\)").unwrap();
}

/// A special type of comment that is meaningful to pichpich.
///
/// For proper magic comments, the 'id' field must be non-empty.
///
/// However, some codebases use the NOTE + author in parens pattern,
/// so the parsing stage may potentially create MagicComment values
/// with an empty id field, but they are later discarded.
/// See NOTE(ref: magic-comment-lookalike).
#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct MagicComment {
    /// Does this comment have a 'def:' field? Otherwise it is a reference.
    pub(crate) is_def: bool,
    /// What is the (project-wide) unique id for this magic comment?
    pub(crate) id: String,
    /// Who is the author for the comment?
    author: Option<String>,
    /// Data specific to the magic comment in question
    pub(crate) kind: MagicCommentKindData,
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) enum MagicCommentKindData {
    #[default]
    Note,
    Warning,
    Todo {
        /// Tracking issue for this specific TODO comment.
        issue: Option<String>,
    },
    Fixme {
        /// Tracking issue for this specific FIXME comment.
        issue: Option<String>,
    },
    Review {
        /// Reviewer to tag if this comment is still around.
        reviewer_id: Option<String>,
    },
}

impl MagicCommentKindData {
    pub(crate) fn leading_text(&self) -> &'static str {
        use MagicCommentKindData as K;
        match &self {
            K::Note => "NOTE",
            K::Warning => "WARNING",
            K::Todo { .. } => "TODO",
            K::Fixme { .. } => "FIXME",
            K::Review { .. } => "REVIEW",
        }
    }
}

/// A source span [start, end) within a file, tracking byte offsets from the
/// start of the file.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    _start: usize,
    _end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        assert!(end >= start);
        Span {
            _start: start,
            _end: end,
        }
    }
    pub fn start(&self) -> usize {
        self._start
    }
    pub fn end(&self) -> usize {
        self._end
    }
    pub fn len(&self) -> usize {
        self._end - self._start
    }
    pub fn contains(&self, other: usize) -> bool {
        self._start <= other && other < self._end
    }
    pub fn contains_span(&self, other: Span) -> bool {
        self._start <= other._start && other._end <= self._end
    }
    pub(crate) fn slice_relative(base: &str, inner: &str) -> Span {
        let base_mem_range = base.as_ptr() as usize..base.as_ptr() as usize + base.len();
        let inner_start = inner.as_ptr() as usize;
        let inner_end = inner.as_ptr() as usize + inner.len();
        assert!(
            base_mem_range.contains(&inner_start),
            "inner str {inner} not contained in base {base}"
        );
        assert!(
            base_mem_range.contains(&(inner_end - 1).max(inner_start)),
            "inner str {inner} ends after base {base}"
        );
        Span::new(
            inner_start - base_mem_range.start,
            inner_end - base_mem_range.start,
        )
    }
    pub(crate) fn for_subslice(base: &SourceRange, inner: &str) -> Span {
        Span::slice_relative(base.slice(), inner).adjust_offsets(base.span.start())
    }
    pub fn into_range(self) -> Range<usize> {
        self._start..self._end
    }
    pub fn attach_to<T>(self, value: T) -> WithSpan<T> {
        WithSpan { span: self, value }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        span.into_range().into()
    }
}

impl AdjustOffsets for Span {
    fn adjust_offsets(self, base_offset: usize) -> Self {
        Span {
            _start: base_offset + self._start,
            _end: base_offset + self._end,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithSpan<T> {
    pub span: Span,
    pub value: T,
}

impl<T> WithSpan<T> {
    pub fn map<U>(self, f: &dyn Fn(T) -> U) -> WithSpan<U> {
        self.span.attach_to(f(self.value))
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRange {
    pub path: Arc<PathBuf>,
    pub(crate) span: Span,
    pub(crate) contents: AllEquivalent<Arc<String>>,
    // The order of fields is deliberate so that sorting causes errors in the same
    // file to be grouped together, and errors get reported from the top to the bottom
    // of the file.
}

impl AdjustOffsets for SourceRange {
    fn adjust_offsets(self, base_offset: usize) -> SourceRange {
        SourceRange {
            span: self.span.adjust_offsets(base_offset),
            ..self
        }
    }
}

impl SourceRange {
    // E.g. for 'e' in
    // a b c \n d e f
    // 0 1 2  3 4 5 6
    // <-------->
    //    bytes
    // start_offset = 5, line = 1, column = 1

    // 0-based line
    pub(crate) fn start_line(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.span.start())
            .expect("invalid start offset");
        return bytes.iter().filter(|c| **c == b'\n').count();
    }
    // 0-based column value
    pub(crate) fn start_column(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.span.start())
            .expect("invalid start offset");
        return bytes.iter().rev().take_while(|c| **c != b'\n').count();
    }
    pub(crate) fn end_line(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.span.end())
            .expect("invalid end offset");
        return bytes.iter().filter(|c| **c == b'\n').count();
    }
    pub(crate) fn end_column(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.span.end())
            .expect("invalid end offset");
        return bytes.iter().rev().take_while(|c| **c != b'\n').count();
    }
    pub(crate) fn slice(&self) -> &str {
        self.contents
            .get(self.span.into_range())
            .expect("stored invalid offsets")
    }
}

impl miette::SourceCode for SourceRange {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, MietteError> {
        match self.contents.value.as_str().read_span(
            span,
            context_lines_before,
            context_lines_after,
        ) {
            Ok(a) => Ok(Box::new(SpanContentsWithPathAdapter {
                path: self.path.as_path(),
                contents: a,
            })),
            Err(e) => Err(e),
        }
    }
}

impl serde::Serialize for SourceRange {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("SourceRange", 2)?;
        state.serialize_field(
            "range",
            &(
                self.start_line(),
                self.start_column(),
                self.end_line(),
                self.end_column(),
            ),
        )?;
        state.serialize_field("path", &self.path.to_string_lossy())?;
        state.end()
    }
}

#[derive(Default, Debug)]
pub struct SyntaxData {
    /// Tracks the file contents for files containing at least one magic comment
    /// (or look-alike).
    pub path_to_content_map: HashMap<Arc<PathBuf>, Arc<String>>,
    /// Tracks the magic comments (or look-alikes) found in a particular file
    /// along with associated spans from keyword start to the end of the closing ).
    pub path_to_comment_map: HashMap<Arc<PathBuf>, Vec<WithSpan<Arc<MagicComment>>>>,
    /// For each magic comment (or look-alike), tracks the ranges from the keyword
    /// start to end of the closing ). In typical usage, a definition may have
    /// multiple references to it (which correspond to identical MagicComment
    /// values but different source ranges), so the value type of the map is a Vec,
    /// not a single SourceRange.
    pub comment_to_range_map: HashMap<Arc<MagicComment>, Vec<SourceRange>>,
}

impl SyntaxData {
    pub(crate) fn merge(&mut self, other: SyntaxData) {
        self.path_to_content_map
            .extend(other.path_to_content_map.into_iter());
        merge_map_vec(&mut self.path_to_comment_map, other.path_to_comment_map);
        merge_map_vec(&mut self.comment_to_range_map, other.comment_to_range_map);
    }
    pub(crate) fn merge_all(data: Vec<SyntaxData>) -> SyntaxData {
        let mut res = SyntaxData::default();
        for data in data.into_iter() {
            res.merge(data);
        }
        return res;
    }
    fn format_snapshot_file(
        content: &Arc<String>,
        comments: Vec<WithSpan<Arc<MagicComment>>>,
    ) -> String {
        format_utils::format_snapshot_file(
            content.as_str(),
            comments
                .into_iter()
                .map(|wsc| wsc.map(&|mc| format!("{:?}", mc.as_ref())))
                .collect(),
        )
    }
    pub fn format_snapshot(&self) -> String {
        let mut files = vec![];
        for (path, content) in self.path_to_content_map.iter() {
            let comments = self
                .path_to_comment_map
                .get(path)
                .unwrap_or_else(|| {
                    panic!(
                        "missing comments for recorded path {}",
                        path.to_string_lossy()
                    )
                })
                .clone();
            let file_snapshot = Self::format_snapshot_file(content, comments);
            files.push((path.clone(), file_snapshot));
        }
        files.sort();
        let mut out = String::new();
        for (_, s) in files.into_iter() {
            out.push_str(&s);
            out.push('\n');
        }
        return out;
    }
}
