// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use miette::{GraphicalReportHandler, GraphicalTheme};
use pichpich::frontend::Options;
use pichpich::parse_clike::FlatDocument;
use pichpich::{frontend, main_impl, parse_clike};
use pichpich_config::ErrorConfig;
use std::path::PathBuf;
use std::sync::Arc;

fn for_each_path(pattern: &str, test_path: &dyn Fn(PathBuf) -> ()) {
    for entry in glob::glob(pattern).expect("failed to read pattern") {
        let path = match entry {
            Ok(p) => p,
            Err(e) => {
                panic!("ill-formed result from glob {}", e);
            }
        };
        test_path(path)
    }
}

#[test]
fn enclosing_spans_snapshots() {
    let opts = Options {
        root: "tests/snapshots/enclosing-spans".parse().unwrap(),
        error_config: ErrorConfig::default(),
    };
    let syntax_data = frontend::gather(&opts).0;
    assert!(!syntax_data.path_to_comment_map.is_empty());
    let mut path_list: Vec<_> = syntax_data
        .path_to_content_map
        .iter()
        .map(|(p, _)| p.clone())
        .collect();
    path_list.sort();
    assert!(path_list.len() == 2);
    let mut documents = vec![];
    for path in path_list.iter() {
        let contents = syntax_data.path_to_content_map.get(path).unwrap().clone();
        documents.push(FlatDocument::new(contents));
    }
    for (i, path) in path_list.iter().enumerate() {
        let contents = syntax_data.path_to_content_map.get(path).unwrap();
        let comments = syntax_data.path_to_comment_map.get(path).unwrap();
        let mut comment_texts = comments.iter().map(|comment| {
            contents
                .as_str()
                .get(comment.span.into_range())
                .expect("stored out-of-bounds indexes in comment span")
        });
        insta::assert_snapshot!(
            documents[i].format_snapshot_with_enclosing_spans(&mut comment_texts)
        );
    }
}

#[test]
fn doc_parsing_snapshots() {
    for_each_path("tests/snapshots/parse-*", &|path| {
        let file_contents = std::fs::read_to_string(&path).unwrap();
        let document = parse_clike::FlatDocument::new(Arc::new(file_contents));
        insta::assert_snapshot!(document.format_snapshot())
    })
}

#[test]
fn internal_state_snapshots() {
    for_each_path("tests/snapshots/internal-state-*", &|path| {
        let opts = Options {
            root: path.to_owned(),
            error_config: ErrorConfig::default(),
        };
        let result = frontend::gather(&opts);
        insta::assert_snapshot!(result.0.format_snapshot());
    });
}

#[test]
fn error_snapshots() {
    // Avoid colors in output to make snapshot files readable in a standalone way.
    let reporter = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor());
    for_each_path("tests/snapshots/error-*", &|path| {
        let mut error_config = ErrorConfig::default();
        assert!(error_config.populate(vec!["error:all".to_string()]).is_ok());
        let opts = Options {
            root: path.to_owned(),
            error_config,
        };
        let result = main_impl(opts);
        match result {
            Ok(()) => panic!(
                "expected errors when processing {:?}",
                path.to_string_lossy()
            ),
            Err(e) => {
                let mut buf = String::new();
                assert!(
                    reporter.render_report(&mut buf, e.as_ref()).is_ok(),
                    "rendering failed"
                );
                insta::assert_snapshot!(buf);
            }
        }
    });
}
