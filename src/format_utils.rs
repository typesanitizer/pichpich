// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
