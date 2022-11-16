use crate::core::{Span, WithSpan};
use crate::format_utils;
use nom::Slice;
use std::fmt::{Debug, Write};
use std::sync::Arc;

pub struct FlatDocument {
    contents: Arc<String>,
    elements: Vec<FlatDocumentElement>,
}

impl FlatDocument {
    pub fn new(contents: Arc<String>) -> FlatDocument {
        let elements = FlatDocumentParser::new(contents.as_str()).run();
        FlatDocument { contents, elements }
    }
    pub fn format_snapshot(&self) -> String {
        let mut buf = String::new();
        for element in self.elements.iter() {
            let text = self.contents.as_str().slice(element.span.into_range());
            buf.write_fmt(format_args!("- {:?}: {:?}\n", element.value, text))
                .unwrap();
        }
        return buf;
    }

    pub fn format_snapshot_with_enclosing_spans(
        &self,
        magic_comments: &mut dyn Iterator<Item = &str>,
    ) -> String {
        let mut span_marks = vec![];
        for magic_comment in magic_comments {
            let (enclosing_comment_span, enclosing_group_span) =
                self.find_enclosing_spans(magic_comment);
            if let Some(enclosing_comment_span) = enclosing_comment_span {
                span_marks.push(
                    enclosing_comment_span
                        .attach_to(format!("enclosing paragraph for {}", magic_comment)),
                );
            }
            if let Some(enclosing_group_span) = enclosing_group_span {
                span_marks.push(
                    enclosing_group_span
                        .attach_to(format!("enclosing group for {}", magic_comment)),
                );
            }
        }
        return format_utils::format_snapshot_file(self.contents.as_str(), span_marks);
    }

    // The enclosing span is defined as follows.
    // - If the span is within the text of a line comment, then the returned span
    //   includes the contiguous paragraph containing the argument slice.
    // - If the span is within the text of a block comment, then the returned
    //   span is the extend of the surrounding block comment.
    //
    // Additionally, if the span is within a group (e.g. { ... }), then the span
    // for the innermost group containing the span is also returned.
    pub fn find_enclosing_spans(&self, s: &str) -> (Option<Span>, Option<Span>) {
        assert!(!s.is_empty());
        let span = Span::slice_relative(self.contents.as_str(), s);
        let element_index = self.binary_search(span.start());
        assert!(
            element_index == self.binary_search(span.end() - 1),
            "input span is unexpectedly spread out over multiple elements"
        );
        return (
            self.enclosing_comment_span(element_index),
            self.enclosing_group_span(element_index),
        );
    }

    fn enclosing_group_span(&self, central_index: usize) -> Option<Span> {
        let mut start_offset = None;
        let mut balance = 0;
        for i in (0..central_index + 1).rev() {
            match &self.elements[i].value {
                FlatDocumentElementKind::GroupStart => {
                    balance -= 1;
                    if balance == -1 {
                        start_offset = Some(self.elements[i].span.end());
                        break;
                    }
                }
                FlatDocumentElementKind::GroupEnd => {
                    balance += 1;
                }
                _ => {}
            }
        }
        if let Some(start_offset) = start_offset {
            let mut balance = 0;
            for i in central_index..self.elements.len() {
                match &self.elements[i].value {
                    FlatDocumentElementKind::GroupStart => {
                        balance -= 1;
                    }
                    FlatDocumentElementKind::GroupEnd => {
                        balance += 1;
                        if balance == 1 {
                            let end_offset = self.elements[i].span.start();
                            return Some(Span::new(start_offset, end_offset));
                        }
                    }
                    _ => {}
                }
            }
        }
        return None;
    }

    fn find_matching_comment(
        &self,
        start: usize,
        forward: bool,
        need: FlatDocumentElementKind,
        dual: FlatDocumentElementKind,
    ) -> usize {
        let mut best = start;
        let shift = (forward as isize * 2) - 1;
        let mut cursor = (start as isize).wrapping_add(shift);
        // See also NOTE(ref: regular-flat-doc-structure)
        let mut balance = 0;
        while (0..self.elements.len()).contains(&(cursor as usize)) {
            let contents = self.contents.as_str();
            match &self.elements[cursor as usize].value {
                FlatDocumentElementKind::Content => {
                    if balance == 0 {
                        let newlines = contents
                            .slice(self.elements[cursor as usize].span.into_range())
                            .as_bytes()
                            .iter()
                            .filter(|c| **c == b'\n')
                            .count();
                        if newlines > 1 {
                            break;
                        }
                    }
                }
                k if *k == need => {
                    balance += 1;
                    if balance == 0 {
                        let prev_cursor = cursor.wrapping_add(-shift);
                        let prev_content = self.elements[prev_cursor as usize];
                        assert_eq!(prev_content.value, FlatDocumentElementKind::Content);
                        let is_para_cont = contents
                            .slice(prev_content.span.into_range())
                            .contains(|c: char| !c.is_whitespace());
                        if is_para_cont {
                            best = cursor as usize;
                        } else {
                            break;
                        }
                    }
                }
                k if *k == dual => {
                    balance -= 1;
                }
                _ => break,
            }
            cursor = cursor.wrapping_add(shift);
        }
        return best;
    }

    fn enclosing_comment_span(&self, central_index: usize) -> Option<Span> {
        use FlatDocumentElementKind as Kind;
        assert!(self.elements[central_index].value == Kind::Content);
        match (
            self.elements.get(central_index - 1),
            self.elements.get(central_index + 1),
        ) {
            (Some(pre), Some(post))
                if pre.value == Kind::LineCommentStart && post.value == Kind::LineCommentEnd =>
            {
                let para_start_index = self.find_matching_comment(
                    central_index - 1,
                    false,
                    Kind::LineCommentStart,
                    Kind::LineCommentEnd,
                );
                let para_end_index = self.find_matching_comment(
                    central_index + 1,
                    true,
                    Kind::LineCommentEnd,
                    Kind::LineCommentStart,
                );
                let pre = &self.elements[para_start_index];
                let post = &self.elements[para_end_index];
                return Some(Span::new(pre.span.end(), post.span.start()));
            }
            (Some(pre), Some(post))
                if pre.value == Kind::BlockCommentStart && post.value == Kind::BlockCommentEnd =>
            {
                return Some(Span::new(pre.span.end(), post.span.start()))
            }
            (_pre, _post) => {
                return None;
            }
        }
    }
    fn binary_search(&self, offset: usize) -> usize {
        assert!(offset < self.contents.len());
        assert!(!self.elements.is_empty());
        let mut lo = 0;
        let mut hi = self.elements.len();
        while lo < hi - 1 {
            let mid = (lo + hi) / 2;
            let mid_span = self.elements[mid].span;
            if mid_span.contains(offset) {
                return mid;
            } else if offset < mid_span.start() {
                assert!(mid != lo, "offset is below minimum");
                hi = mid;
                continue;
            } else if offset >= mid_span.end() {
                assert!(mid + 1 != hi, "offset is outside maximum");
                lo = mid + 1;
            }
        }
        assert!(
            self.elements[lo].span.contains(offset),
            "offset not in middle range"
        );
        return lo;
    }
}

type FlatDocumentElement = WithSpan<FlatDocumentElementKind>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FlatDocumentElementKind {
    Content,
    BlockCommentStart,
    BlockCommentEnd,
    GroupStart,
    GroupEnd,
    LineCommentStart,
    LineCommentEnd,
}

// NOTE(def: regular-flat-doc-structure): Comments always come as three elements
//   XCommentStart Content XCommentEnd
// The corresponding spans for Content and XCommentEnd may be empty,
// but using a regular pattern simplifies trying to interpret the output
// when inferring surrounding spans.

struct FlatDocumentParser<'a> {
    state: FlatDocumentParserState,
    original: &'a str,
    rest: &'a str,
    elements: Vec<FlatDocumentElement>,
}

#[allow(clippy::enum_variant_names)]
#[derive(Copy, Clone)]
enum FlatDocumentParserState {
    InBlockComment,
    InLineComment,
    NotInComment,
}

impl<'a> FlatDocumentParser<'a> {
    fn new<'b>(input: &'b str) -> FlatDocumentParser<'b> {
        FlatDocumentParser {
            state: FlatDocumentParserState::NotInComment,
            original: input,
            rest: input,
            elements: vec![],
        }
    }

    fn run(mut self) -> Vec<FlatDocumentElement> {
        while !self.rest.is_empty() {
            match self.state {
                FlatDocumentParserState::NotInComment => {
                    let mut end_index = 0;
                    let mut resume_index = 0;
                    let mut state_change = None;
                    let mut extra = None;
                    for i in 0..self.rest.len() {
                        let b = self.rest.as_bytes()[i];
                        if b == b'{' {
                            (end_index, resume_index) = (i, i + 1);
                            extra = Some(FlatDocumentElementKind::GroupStart);
                            break;
                        }
                        if b == b'}' {
                            (end_index, resume_index) = (i, i + 1);
                            extra = Some(FlatDocumentElementKind::GroupEnd);
                            break;
                        }
                        if b == b'/' {
                            if let Some(&next) = self.rest.as_bytes().get(i + 1) {
                                if next == b'*' {
                                    (end_index, resume_index) = (i, i + 2);
                                    extra = Some(FlatDocumentElementKind::BlockCommentStart);
                                    state_change = Some(FlatDocumentParserState::InBlockComment);
                                    break;
                                }
                                if next == b'/' {
                                    (end_index, resume_index) = (i, i + 2);
                                    extra = Some(FlatDocumentElementKind::LineCommentStart);
                                    state_change = Some(FlatDocumentParserState::InLineComment);
                                    break;
                                }
                            }
                        }
                        (end_index, resume_index) = (i, i + 1);
                    }
                    let content =
                        Span::slice_relative(self.original, self.rest.slice(0..end_index))
                            .attach_to(FlatDocumentElementKind::Content);
                    self.elements.push(content);
                    if let Some(next_element) = extra {
                        let element = Span::slice_relative(
                            self.original,
                            self.rest.slice(end_index..resume_index),
                        )
                        .attach_to(next_element);
                        self.elements.push(element);
                    }
                    self.rest = self.rest.slice(resume_index..);
                    if let Some(state) = state_change {
                        self.state = state;
                    }
                }
                FlatDocumentParserState::InBlockComment => {
                    let (end_index, resume_index) = {
                        if let Some(end_index) = self.rest.find("*/") {
                            (end_index, end_index + 2)
                        } else {
                            (self.rest.len(), self.rest.len())
                        }
                    };
                    // See also NOTE(ref: regular-flat-doc-structure)
                    let content =
                        Span::slice_relative(self.original, self.rest.slice(0..end_index))
                            .attach_to(FlatDocumentElementKind::Content);
                    let end = Span::slice_relative(
                        self.original,
                        self.rest.slice(end_index..resume_index),
                    )
                    .attach_to(FlatDocumentElementKind::BlockCommentEnd);
                    self.elements.push(content);
                    self.elements.push(end);
                    self.state = FlatDocumentParserState::NotInComment;
                    self.rest = self.rest.slice(resume_index..);
                }
                FlatDocumentParserState::InLineComment => {
                    let end_index = self.rest.find('\n').unwrap_or(self.rest.len());
                    // See also NOTE(ref: regular-flat-doc-structure)
                    let content =
                        Span::slice_relative(self.original, self.rest.slice(0..end_index))
                            .attach_to(FlatDocumentElementKind::Content);
                    let end =
                        Span::slice_relative(self.original, self.rest.slice(end_index..end_index))
                            .attach_to(FlatDocumentElementKind::LineCommentEnd);
                    self.elements.push(content);
                    self.elements.push(end);
                    self.state = FlatDocumentParserState::NotInComment;
                    self.rest = self.rest.slice(end_index..);
                }
            }
        }
        return self.elements;
    }
}
