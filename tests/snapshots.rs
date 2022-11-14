// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use miette::{GraphicalReportHandler, GraphicalTheme};
use pichpich::config::ErrorConfig;
use pichpich::frontend::Options;
use pichpich::main_impl;

#[test]
fn error_snapshots() {
    // Avoid colors in output to make snapshot files readable in a standalone way.
    let reporter = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor());
    for entry in glob::glob("tests/snapshots/error-*").expect("failed to read pattern") {
        let path = match entry {
            Ok(p) => p,
            Err(e) => {
                assert!(false, "ill-formed result from glob {}", e);
                unreachable!()
            }
        };
        let mut error_config = ErrorConfig::default();
        assert!(error_config.populate(vec!["error:all".to_string()]).is_ok());
        let opts = Options {
            root: path.to_owned(),
            error_config,
        };
        let result = main_impl(opts);
        match result {
            Ok(()) => assert!(
                false,
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
    }
}
