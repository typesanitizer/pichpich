// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::core::{MagicComment, SourceRange, Span, SyntaxData};
use crate::frontend::{parse_attribute_list, Options};
use crate::utils::AdjustOffsets;
use crate::{AppError, AppErrorData};
use miette::Diagnostic;
use pichpich_config::{AppErrorCode, ErrorConfig};
use std::{
    collections::hash_map::Entry,
    collections::{BTreeSet, HashMap},
    error::Error,
    fmt::{Display, Formatter},
    sync::Arc,
};

#[allow(clippy::type_complexity)]
struct SymbolTable {
    id_to_comments_map: HashMap<String, Vec<(Arc<MagicComment>, Vec<SourceRange>)>>,
}

impl SymbolTable {
    fn build(data: SyntaxData) -> SymbolTable {
        let mut id_to_comments_map: HashMap<String, Vec<_>> = HashMap::default();
        for (comment, range) in data.comment_to_range_map.into_iter() {
            assert!(
                !range.is_empty(),
                "doesn't make sense to store empty ranges"
            );
            if comment.id.is_empty() {
                continue;
            }
            match id_to_comments_map.entry(comment.id.clone()) {
                Entry::Occupied(mut o) => o.get_mut().push((comment, range)),
                Entry::Vacant(v) => {
                    v.insert(vec![(comment, range)]);
                }
            }
        }
        return SymbolTable { id_to_comments_map };
    }

    fn analyse(&self, error_config: &ErrorConfig) -> BTreeSet<AppError> {
        let mut out = vec![];

        if let Some(level) = error_config.get_level(AppErrorCode::InconsistentIdKind) {
            for (_, v) in self.id_to_comments_map.iter() {
                assert!(!v.is_empty());
                if v.len() == 1 {
                    continue;
                }
                let n_unique = v
                    .iter()
                    .map(|(mc, _)| mc.kind.leading_text())
                    .collect::<BTreeSet<_>>()
                    .len();
                if n_unique == 1 {
                    continue;
                }
                let mut data = v.clone();
                data.sort();
                out.push(level.attach_to(AnalysisErrorData::new_inconsistent_kind(data)));
            }
        }

        if let Some(level) = error_config.get_level(AppErrorCode::UndefinedRef) {
            for (_, v) in self.id_to_comments_map.iter() {
                if v.iter().any(|(mc, _)| mc.is_def) {
                    continue;
                }
                let mut data = v.clone();
                data.sort();
                out.push(level.attach_to(AnalysisErrorData::new_undefined_ref(data)));
            }
        }

        out.into_iter()
            .map(|err_with_level| err_with_level.map(&AppErrorData::Analysis))
            .collect()
    }
}

pub fn run(options: &Options, data: SyntaxData) -> BTreeSet<AppError> {
    let table = SymbolTable::build(data);
    return table.analyse(&options.error_config);
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnalysisErrorData {
    UndefinedRef {
        main_comment: Arc<MagicComment>,
        main_range: SourceRange,
        // Create this vector ahead-of-time, instead of during error emission, because miette's
        // 'fn related' demands &dyn Diagnostics where the lifetime is the same as the original
        // error. So just storing the data in a flat vector and iterating over it later
        // (to make and emit diagnostics on the fly) is not possible because that will entail
        // constructing temporaries which will have a shorter lifetime than the error.
        related: Vec<AnalysisErrorData>,
    },
    InconsistentKind {
        main_comment: Arc<MagicComment>,
        main_range: SourceRange,
        // Invariant: The Vec only contains InconsistentKind elements
        related: Vec<AnalysisErrorData>,
    },
}

impl AnalysisErrorData {
    fn new_from_data(
        data: Vec<(Arc<MagicComment>, Vec<SourceRange>)>,
        f: &dyn Fn(Arc<MagicComment>, SourceRange, Vec<AnalysisErrorData>) -> AnalysisErrorData,
    ) -> AnalysisErrorData {
        let main_comment = data[0].0.clone();
        let main_range = data[0].1[0].clone();
        let first_ranges = data[0].1.clone();
        let related = first_ranges
            .into_iter()
            .skip(1)
            .map(|r| (main_comment.clone(), r))
            .chain(
                data.into_iter()
                    .skip(1)
                    .flat_map(|(mc, rv)| rv.into_iter().map(move |r| (mc.clone(), r))),
            )
            .map(|(c, r)| f(c, r, vec![]))
            .collect();
        return f(main_comment, main_range, related);
    }
    fn new_undefined_ref(data: Vec<(Arc<MagicComment>, Vec<SourceRange>)>) -> AnalysisErrorData {
        AnalysisErrorData::new_from_data(data, &|main_comment, main_range, related| {
            AnalysisErrorData::UndefinedRef {
                main_comment,
                main_range,
                related,
            }
        })
    }
    fn new_inconsistent_kind(
        data: Vec<(Arc<MagicComment>, Vec<SourceRange>)>,
    ) -> AnalysisErrorData {
        AnalysisErrorData::new_from_data(data, &|main_comment, main_range, related| {
            AnalysisErrorData::InconsistentKind {
                main_comment,
                main_range,
                related,
            }
        })
    }
}

impl Display for AnalysisErrorData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AnalysisErrorData::UndefinedRef { main_comment, .. } => f.write_fmt(format_args!(
                "missing definition for reference to {}",
                main_comment.id
            )),
            AnalysisErrorData::InconsistentKind { main_comment, .. } => f.write_fmt(format_args!(
                "used in {} {} here",
                article_for(main_comment.kind.leading_text()),
                main_comment.kind.leading_text(),
            )),
        }
    }
}

impl Error for AnalysisErrorData {}

fn article_for(s: &str) -> &'static str {
    if s.starts_with(|c: char| "AEIOU".contains(c.to_ascii_uppercase())) {
        return "an";
    }
    return "a";
}

impl miette::Diagnostic for AnalysisErrorData {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            AnalysisErrorData::UndefinedRef { .. } => Some(Box::new(AppErrorCode::UndefinedRef)),
            AnalysisErrorData::InconsistentKind { .. } => {
                Some(Box::new(AppErrorCode::InconsistentIdKind))
            }
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            AnalysisErrorData::UndefinedRef { main_comment, .. } => Some(Box::new(format!(
                "did you delete or forget to add a {}(def: {}) somewhere?",
                main_comment.kind.leading_text(),
                main_comment.id,
            ))),
            AnalysisErrorData::InconsistentKind {
                main_comment,
                related,
                ..
            } => {
                if !related.is_empty() {
                    let kinds: Vec<String> = [main_comment.kind.leading_text()]
                        .into_iter()
                        .chain(related.iter().map(|aed| match aed {
                            AnalysisErrorData::InconsistentKind {
                                main_comment: mc, ..
                            } => mc.kind.leading_text(),
                            _ => unreachable!("InconsistentKind error contains some other child"),
                        }))
                        .collect::<BTreeSet<_>>()
                        .into_iter()
                        .map(|s| format!("{} {}", article_for(s), s))
                        .collect();
                    Some(Box::new(format!(
                        "consistently use this id in {}",
                        crate::format_utils::format_list(&kinds, "or")
                    )))
                } else {
                    None
                }
            }
        }
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        match &self {
            AnalysisErrorData::UndefinedRef { main_range, .. } => Some(main_range),
            AnalysisErrorData::InconsistentKind { main_range, .. } => Some(main_range),
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        match &self {
            AnalysisErrorData::UndefinedRef { main_range, .. } => {
                let map = parse_attr_list_w_spans(
                    main_range
                        .contents
                        .value
                        .as_ref()
                        .get(main_range.span.into_range())
                        .expect("Out-of-bounds offsets stored in range"),
                );
                let (_, _, value_span) = map.get("ref").unwrap_or_else(|| {
                    panic!("missing ref key in comment {:?}", main_range.slice())
                });
                Some(Box::new(
                    [miette::LabeledSpan::new_with_span(
                        None,
                        value_span.adjust_offsets(main_range.span.start()),
                    )]
                    .into_iter(),
                ))
            }
            AnalysisErrorData::InconsistentKind { main_range, .. } => {
                let map = parse_attr_list_w_spans(
                    main_range
                        .contents
                        .value
                        .as_ref()
                        .get(main_range.span.into_range())
                        .expect("Out-of-bounds offsets stored in range"),
                );
                let (_, _, value_span) = map.get("ref").unwrap_or_else(|| {
                    map.get("def").unwrap_or_else(|| {
                        panic!("missing def or ref key in comment {:?}", main_range.slice())
                    })
                });
                Some(Box::new(
                    [miette::LabeledSpan::new_with_span(
                        None,
                        value_span.adjust_offsets(main_range.span.start()),
                    )]
                    .into_iter(),
                ))
            }
        }
    }

    fn related(&self) -> Option<Box<dyn Iterator<Item = &dyn Diagnostic> + '_>> {
        match &self {
            AnalysisErrorData::UndefinedRef { related, .. } => {
                if !related.is_empty() {
                    return Some(Box::new(related.iter().map(|v| v as &dyn Diagnostic)));
                }
                return None;
            }
            AnalysisErrorData::InconsistentKind { related, .. } => {
                if !related.is_empty() {
                    return Some(Box::new(related.iter().map(|v| v as &dyn Diagnostic)));
                }
                return None;
            }
        }
    }
}

// Helper function to attribute list with source spans.
fn parse_attr_list_w_spans(base: &str) -> HashMap<&str, (&str, Span, Span)> {
    let start = base
        .find('(')
        .expect("unexpected missing opening brace after parsing");
    let attr_list_text = base
        .get(start + 1..base.len() - 1)
        .expect("unexpected failed to read starting from find result");
    let mut out = HashMap::default();
    for (k, v) in parse_attribute_list(attr_list_text)
        .expect("failed to re-parse attr list")
        .1
        .filter_map(|res| res.ok())
    {
        let present = out.insert(
            k,
            (
                v,
                Span::slice_relative(base, k),
                Span::slice_relative(base, v),
            ),
        );
        assert!(
            present.is_none(),
            "duplicate attr keys should've been diagnosed earlier"
        );
    }
    return out;
}
