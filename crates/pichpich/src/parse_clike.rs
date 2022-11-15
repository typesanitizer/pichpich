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

// pub struct Document<'a> {
//     elements: Vec<DocumentElement<'a>>,
//     linebreak_offsets: Vec<usize>,
// }
//
// impl<'a> Debug for Document<'a> {
//     #[allow(dead_code)]
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         #[derive(Debug)]
//         struct Doc<'a> {
//             elements: &'a Vec<DocumentElement<'a>>,
//         }
//         Doc {
//             elements: &self.elements,
//         }
//         .fmt(f)
//     }
// }
//
// #[derive(Debug)]
// pub enum DocumentElement<'a> {
//     Content(&'a str),
//     BlockComment(&'a str),
//     LineComment(&'a str),
//     Group {
//         start: &'a str,
//         inner: Vec<DocumentElement<'a>>,
//         end: &'a str,
//     },
// }
//
// pub fn parse_document<'a>(
//     input: &'a str,
// ) -> Result<Document<'a>, nom::Err<nom::error::Error<&str>>> {
//     match many_till(parse_document_element, eof)(input) {
//         Ok((_, (elements, _))) => Ok(Document {
//             elements,
//             linebreak_offsets: input
//                 .chars()
//                 .enumerate()
//                 .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
//                 .collect(), // TODO
//         }),
//         Err(e) => Err(e),
//     }
// }
//
// fn parse_document_element<'a>(input: &'a str) -> IResult<&'a str, DocumentElement<'a>> {
//     alt((
//         map(parse_group, |s| {
//             println!("group = {s:?}");
//             s
//         }),
//         map(parse_line_comment, |s| {
//             println!("lc = {s:?}");
//             s
//         }),
//         map(parse_block_comment, |s| {
//             println!("bc = {s:?}");
//             s
//         }),
//         map(parse_content, |s| {
//             println!("content = {s:?}");
//             s
//         }),
//     ))(input)
// }
//
// fn parse_line_comment<'a>(input: &'a str) -> IResult<&'a str, DocumentElement<'a>> {
//     let (rest, _) = preceded(tag("//"), take_while(|c| c != '\n'))(input)?;
//     Ok((
//         rest,
//         DocumentElement::LineComment(input.slice(0..input.len() - rest.len())),
//     ))
// }
//
// fn parse_group<'a>(input: &'a str) -> IResult<&'a str, DocumentElement<'a>> {
//     let (rest, (start, (inner, end))) =
//         tuple((tag("{"), many_till(parse_document_element, tag("}"))))(input)?;
//     Ok((rest, DocumentElement::Group { start, inner, end }))
// }
//
// fn parse_block_comment<'a>(input: &'a str) -> IResult<&'a str, DocumentElement<'a>> {
//     let (rest, _) = delimited(tag("/*"), take_until("*/"), tag("*/"))(input)?;
//     Ok((
//         rest,
//         DocumentElement::BlockComment(input.slice(0..input.len() - rest.len())),
//     ))
// }
//
// fn parse_content<'a>(input: &'a str) -> IResult<&'a str, DocumentElement<'a>> {
//     let (mut rest, _) = opt(alt((tag("/*"), tag("{"))))(input)?;
//     while !rest.is_empty() {
//         rest = take_while(|c| c != '/' && c != '{' && c != '}')(rest)?.0;
//         if rest.len() >= 1 {
//             let first_char = rest.chars().nth(0);
//             let second_char = rest.chars().nth(1);
//             if first_char == Some('{')
//                 || first_char == Some('}')
//                 || second_char == Some('*')
//                 || second_char == Some('/')
//             {
//                 break;
//             } else {
//                 rest = rest.slice(1..);
//             }
//         }
//     }
//     return Ok((
//         rest,
//         DocumentElement::Content(input.slice(0..input.len() - rest.len())),
//     ));
// }