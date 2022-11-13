use crate::utils::AdjustOffsets;
use std::path::Path;

/// Adapter type because miette's NamedSource type seemed insufficiently flexible.
pub(crate) struct SpanContentsWithPathAdapter<'a> {
    pub(crate) path: &'a Path,
    pub(crate) contents: Box<dyn miette::SpanContents<'a> + 'a>,
}

impl<'a> miette::SpanContents<'a> for SpanContentsWithPathAdapter<'a> {
    fn data(&self) -> &'a [u8] {
        self.contents.data()
    }

    fn span(&self) -> &miette::SourceSpan {
        self.contents.span()
    }

    fn name(&self) -> Option<&str> {
        self.path.to_str()
    }

    fn line(&self) -> usize {
        self.contents.line()
    }

    fn column(&self) -> usize {
        self.contents.column()
    }

    fn line_count(&self) -> usize {
        self.contents.line_count()
    }
}

impl AdjustOffsets for miette::SourceSpan {
    fn adjust_offsets(self, base_offset: usize) -> miette::SourceSpan {
        miette::SourceSpan::new((self.offset() + base_offset).into(), self.len().into())
    }
}

pub trait ErrorCount {
    fn error_count(&self) -> usize;
}

impl<T: ?Sized + miette::Diagnostic> ErrorCount for T {
    fn error_count(&self) -> usize {
        1 + self
            .related()
            .map_or(0, |b| b.map(|e| e.error_count()).sum())
    }
}
