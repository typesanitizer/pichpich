// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::{AppErrorCode, ErrorConfig, WithErrorLevel};
use crate::core::{MagicComment, SourceRange, SyntaxData};
use crate::frontend::{parse_attribute_list, Options};
use crate::utils::AdjustOffsets;
use crate::{AppError, AppErrorData};
use miette::Diagnostic;
use std::{
    collections::hash_map::Entry,
    collections::{BTreeSet, HashMap},
    error::Error,
    fmt::{Display, Formatter},
    sync::Arc,
};

struct SymbolTable {
    id_to_comments_map: HashMap<String, Vec<(Arc<MagicComment>, Vec<SourceRange>)>>,
}

impl SymbolTable {
    fn build(data: SyntaxData) -> SymbolTable {
        let mut id_to_comments_map: HashMap<String, Vec<_>> = HashMap::default();
        for (comment, range) in data.comment_to_range_map.into_iter() {
            assert!(range.len() > 0, "doesn't make sense to store empty ranges");
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

        if let Some(_level) = error_config.get_level(AppErrorCode::InconsistentIdKind) {
            // TODO(def: diagnose-inconsistent-kinds)
        }

        if let Some(level) = error_config.get_level(AppErrorCode::UndefinedRef) {
            for (_id, v) in self.id_to_comments_map.iter() {
                if v.iter().any(|(mc, _)| mc.is_def) {
                    continue;
                }
                let mut data = v.clone();
                data.sort();
                out.push(WithErrorLevel {
                    level,
                    error: AnalysisErrorData::new_undefined_ref(data),
                });
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
        // data: Vec<(Arc<MagicComment>, Vec<SourceRange>)>,
        // hack_storage: Option<Rc<RefCell<AnalysisErrorData>>>,
        main_comment: Arc<MagicComment>,
        main_range: SourceRange,
        // Create this vector ahead-of-time, instead of during error emission, because miette's
        // 'fn related' demands &dyn Diagnostics where the lifetime is the same as the original
        // error. So just storing the data in a flat vector and iterating over it later
        // (to make and emit diagnostics on the fly) is not possible because that will entail
        // constructing temporaries which will have a shorter lifetime than the error.
        related: Option<Box<Vec<AnalysisErrorData>>>,
    },
}

impl AnalysisErrorData {
    fn new_undefined_ref(data: Vec<(Arc<MagicComment>, Vec<SourceRange>)>) -> AnalysisErrorData {
        let main_comment = data[0].0.clone();
        let main_range = data[0].1[0].clone();
        let first_ranges = data[0].1.clone();
        let related = Some(Box::new(
            first_ranges
                .into_iter()
                .skip(1)
                .map(|r| (main_comment.clone(), r))
                .chain(
                    data.into_iter()
                        .skip(1)
                        .map(|(mc, rv)| rv.into_iter().map(move |r| (mc.clone(), r)))
                        .flatten(),
                )
                .map(|(c, r)| AnalysisErrorData::UndefinedRef {
                    main_comment: c,
                    main_range: r,
                    related: None,
                })
                .collect(),
        ));
        return AnalysisErrorData::UndefinedRef {
            main_comment,
            main_range,
            related,
        };
    }
}

impl Display for AnalysisErrorData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            AnalysisErrorData::UndefinedRef { main_comment, .. } => f.write_fmt(format_args!(
                "missing definition for reference to {}",
                main_comment.id
            )),
        }
    }
}

impl Error for AnalysisErrorData {}

impl miette::Diagnostic for AnalysisErrorData {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            AnalysisErrorData::UndefinedRef { .. } => Some(Box::new(AppErrorCode::UndefinedRef)),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        match &self {
            AnalysisErrorData::UndefinedRef { main_comment, .. } => Some(Box::new(format!(
                "did you delete or forget to add a {}(def: {}) somewhere?",
                main_comment.kind.leading_text(),
                main_comment.id,
            ))),
        }
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        match &self {
            AnalysisErrorData::UndefinedRef { main_range, .. } => Some(main_range),
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
                        .get(main_range.start_offset..main_range.end_offset)
                        .expect("Out-of-bounds offsets stored in range"),
                );
                let (_, _, value_span) = map.get("ref").expect("missing ref key in comment");
                Some(Box::new(
                    [miette::LabeledSpan::new_with_span(
                        None,
                        value_span.clone().adjust_offsets(main_range.start_offset),
                    )]
                    .into_iter(),
                ))
            }
        }
    }

    fn related(&self) -> Option<Box<dyn Iterator<Item = &dyn Diagnostic> + '_>> {
        match &self {
            AnalysisErrorData::UndefinedRef { related, .. } => {
                if let Some(p) = related {
                    return Some(Box::new(p.iter().map(|v| v as &dyn Diagnostic)));
                }
                return None;
            }
        }
    }
}

fn relative_span(base: &str, inner: &str) -> miette::SourceSpan {
    let base_mem_range = base.as_ptr() as usize..base.as_ptr() as usize + base.len();
    let inner_start = inner.as_ptr() as usize;
    let inner_end = inner.as_ptr() as usize + inner.len();
    assert!(
        base_mem_range.contains(&inner_start),
        "inner str {inner} not contained in base {base}"
    );
    assert!(
        base_mem_range.contains(&inner_end),
        "inner str {inner} ends after base {base}"
    );
    ((inner_start - base_mem_range.start)..(inner_end - base_mem_range.start)).into()
}

// Helper function to attribute list with source spans.
fn parse_attr_list_w_spans(
    base: &str,
) -> HashMap<&str, (&str, miette::SourceSpan, miette::SourceSpan)> {
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
    {
        let present = out.insert(k, (v, relative_span(base, k), relative_span(base, v)));
        assert!(
            present.is_none(),
            "duplicate attr keys should've been diagnosed earlier"
        );
    }
    return out;
}
