// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use pichpich::frontend::Options;
use pichpich::main_impl;
use pichpich_config::ErrorConfig;

fn main() -> miette::Result<()> {
    let matches = clap::command!()
        .arg(clap::arg!(--check <pattern> "A pattern in the form of (ignore|warn|error):(all|check-name) to control severity levels for different checks"))
        .get_matches();

    let checks = matches
        .get_many::<String>("check")
        .unwrap_or_default()
        .cloned()
        .collect::<Vec<_>>();

    let mut error_config = ErrorConfig::default();
    // FIXME: Issue error for unknown arguments here.
    let _unknown_checks = error_config.populate(checks);

    let opts = Options {
        root: std::env::current_dir()
            .expect("failed to get the current directory; running in a sandbox?"),
        error_config,
    };

    main_impl(opts)?;
    Ok(())
}

// What kinds of things do we want to test?
// 1. Different kinds of errors -- are we flagging them in the right place.
// 2. Optionally, internal state -- is that correct.
//
// For dumping internal state, we attach ^^^ to the inputs
// For dumping errors, for Miette, we can include the snapshot output.
