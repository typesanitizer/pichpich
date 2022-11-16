// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::core::{Span, WithSpan};
use std::collections::BTreeSet;
use std::fmt::Write;

// Format a list as 'v[0], v[1], ..., v[n - 2] final_word v[n - 1]'
pub(crate) fn format_list(vs: &[String], final_word: &str) -> String {
    let mut buf = String::new();
    assert!(!vs.is_empty());
    if vs.len() == 1 {
        buf.push_str(&vs[0]);
        return buf;
    }
    #[allow(clippy::needless_range_loop)]
    for i in 0..vs.len() - 2 {
        buf.push_str(&vs[i]);
        buf.push_str(", ")
    }
    buf.push_str(&vs[vs.len() - 2]);
    buf.push(' ');
    buf.push_str(final_word);
    buf.push(' ');
    buf.push_str(&vs[vs.len() - 1]);
    return buf;
}

pub(crate) fn format_snapshot_file(content: &str, mut marks: Vec<WithSpan<String>>) -> String {
    let mut worklist = BTreeSet::<(usize, WithSpan<_>)>::new();
    marks.sort();
    let mut mark_index = 0;
    let mut buf = String::new();
    for line in content.lines() {
        buf.push_str(line);
        buf.push('\n');
        let line_span = Span::slice_relative(content, line);
        while let Some((_, mark)) = worklist.pop_first() {
            assert!(mark.span.end() as isize >= line_span.start() as isize - 1);
            if mark.span.end() + 1 == line_span.start() {
                buf.write_fmt(format_args!("> end {}\n", mark.value))
                    .unwrap();
            } else if mark.span.end() <= line_span.end() {
                buf.write_fmt(format_args!(
                    "{}^ end {}\n",
                    &" ".repeat(mark.span.end() - line_span.start()),
                    mark.value
                ))
                .unwrap();
            } else if mark.span.end() == line_span.end() + 1 {
                buf.write_fmt(format_args!(
                    "{}> end {}\n",
                    &" ".repeat(mark.span.end() - line_span.start()),
                    mark.value,
                ))
                .unwrap();
            } else {
                assert!(mark.span.end() > line_span.end() + 1);
                worklist.insert((mark.span.end(), mark));
                break;
            }
        }
        while mark_index != marks.len() {
            let mark = &marks[mark_index];
            assert!(mark.span.start() as isize >= line_span.start() as isize - 1);
            if mark.span.start() + 1 == line_span.start() {
                if mark.span.end() <= line_span.end() {
                    buf.write_fmt(format_args!(
                        "<{} {}\n",
                        &"^".repeat(mark.span.end().saturating_sub(line_span.start() + 1)),
                        mark.value
                    ))
                    .unwrap();
                } else if mark.span.end() == line_span.end() + 1 {
                    buf.write_fmt(format_args!(
                        "<{}> {}\n",
                        &"^".repeat(line_span.len() - 1),
                        mark.value
                    ))
                    .unwrap();
                } else {
                    assert!(mark.span.end() > line_span.end() + 1);
                    buf.write_fmt(format_args!("< start {}\n", mark.value))
                        .unwrap();
                    worklist.insert((mark.span.end(), mark.clone()));
                }
                mark_index += 1;
            } else if line_span.contains(mark.span.start()) {
                let num_space = mark.span.start() - line_span.start();
                buf.push_str(&" ".repeat(num_space));
                if mark.span.end() <= line_span.end() {
                    buf.write_fmt(format_args!(
                        "{} {}\n",
                        &"^".repeat(mark.span.len()),
                        mark.value
                    ))
                    .unwrap();
                } else if mark.span.end() == line_span.end() + 1 {
                    buf.write_fmt(format_args!(
                        "{}> {}\n",
                        &"^".repeat(mark.span.len()),
                        mark.value
                    ))
                    .unwrap();
                } else {
                    assert!(mark.span.end() > line_span.end() + 1);
                    buf.write_fmt(format_args!("^ start {}\n", mark.value))
                        .unwrap();
                    worklist.insert((mark.span.end(), mark.clone()));
                }
                mark_index += 1;
            } else {
                assert!(mark.span.start() >= line_span.end());
                break;
            }
        }
    }
    assert!(worklist.is_empty(), "worklist = {:?}", worklist);
    return buf;
}
