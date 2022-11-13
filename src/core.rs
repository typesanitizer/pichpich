use crate::utils::AdjustOffsets;
use crate::{
    miette_utils::SpanContentsWithPathAdapter,
    utils::{merge_map_vec, AllEquivalent},
};
use lazy_static::lazy_static;
use miette::{MietteError, SourceSpan};
use regex::Regex;
use serde::ser::SerializeStruct;
use serde_derive::Serialize;
use std::{collections::HashMap, hash::Hash, path::PathBuf, sync::Arc};

lazy_static! {
    // TODO(def: sync-support): Add support for SYNC comments with blocks
    pub(crate) static ref SPECIAL_COMMENT_RE: Regex =
        Regex::new(r"(NOTE|WARNING|TODO|FIXME|REVIEW)\([^\)]*\)").unwrap();
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub struct MagicComment {
    pub(crate) is_def: bool,
    pub(crate) id: String,
    author: Option<String>,
    pub(crate) kind: MagicCommentKindData,
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub(crate) enum MagicCommentKindData {
    #[default]
    Note,
    Warning,
    Todo {
        issue: Option<String>,
    },
    Fixme {
        issue: Option<String>,
    },
    Review {
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

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRange {
    pub path: Arc<PathBuf>,
    pub(crate) start_offset: usize,
    pub(crate) end_offset: usize,
    pub(crate) contents: AllEquivalent<Arc<String>>,
    // The order of fields is deliberate so that sorting causes errors in the same
    // file to be grouped together, and errors get reported from the top to the bottom
    // of the file.
}

impl AdjustOffsets for SourceRange {
    fn adjust_offsets(self, base_offset: usize) -> SourceRange {
        SourceRange {
            start_offset: base_offset + self.start_offset,
            end_offset: base_offset + self.end_offset,
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
            .get(..self.start_offset)
            .expect("invalid start offset");
        return bytes.iter().filter(|c| **c == b'\n').count();
    }
    // 0-based column value
    pub(crate) fn start_column(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.start_offset)
            .expect("invalid start offset");
        return bytes.iter().rev().take_while(|c| **c != b'\n').count();
    }
    pub(crate) fn end_line(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.end_offset)
            .expect("invalid end offset");
        return bytes.iter().filter(|c| **c == b'\n').count();
    }
    pub(crate) fn end_column(&self) -> usize {
        let bytes = self
            .contents
            .as_bytes()
            .get(..self.end_offset)
            .expect("invalid end offset");
        return bytes.iter().rev().take_while(|c| **c != b'\n').count();
    }
}

impl miette::SourceCode for SourceRange {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
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

#[derive(Default, Debug, Serialize)]
pub struct SyntaxData {
    pub path_to_comment_map: HashMap<Arc<PathBuf>, Vec<Arc<MagicComment>>>,
    pub comment_to_range_map: HashMap<Arc<MagicComment>, Vec<SourceRange>>,
}

impl SyntaxData {
    pub(crate) fn merge(&mut self, other: SyntaxData) {
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
}
