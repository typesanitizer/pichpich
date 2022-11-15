use crate::core::{Span, WithSpan};
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
}

type FlatDocumentElement = WithSpan<FlatDocumentElementKind>;

#[derive(Debug)]
pub enum FlatDocumentElementKind {
    Content,
    BlockCommentStart,
    BlockCommentEnd,
    GroupStart,
    GroupEnd,
    LineCommentStart,
    LineCommentEnd,
}

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
