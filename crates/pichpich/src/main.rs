// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

fn main() -> miette::Result<()> {
    pichpich::main_impl(std::env::args_os())
}

// What kinds of things do we want to test?
// 1. Different kinds of errors -- are we flagging them in the right place.
// 2. Optionally, internal state -- is that correct.
//
// For dumping internal state, we attach ^^^ to the inputs
// For dumping errors, for Miette, we can include the snapshot output.
