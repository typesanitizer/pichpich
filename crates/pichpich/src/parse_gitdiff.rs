// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

struct GitDiff {

}

// There are 2 different things here:
// 1. Using diffs optionally for improved hints (in case there is an error).
// 2. Using diffs for diagnosing specific things. E.g SYNC comments.
//    - For this use case, one thing we could do is:
//      - look at the surrounding context of the SYNC comment before
//      - look at the surrounding context of the SYNC comment after
//        Q: How do we correlate comments before and after?
//        E.g. if a SYNC comment is deleted in one place,
//        and added in another place, then how do we know that
//        it is not the surrounding context which changed, but the comment
//        itself changed?
//
//

// For non-pull requests,

// Get contents from current ref
// If there are errors,
//   Check GITHUB_HEAD_REF && GITHUB_BASE_REF <- only set when pull_request is present
//     emit warning if only one is set.
//   Get contents from merge-base (create a new worktree)
//     Error if there are multiple merge bases.
//   Throw away errors from merge-base
//   Perform a semantic diff between comment contents to emit a better error.

//   If set, then read contents for this. This is the tip.

// GITHUB_BASE_REF

// Questions we want to ask.
// For every note, get the span for the surrounding paragraph and/or braces.
// In the latest version of the repo, as well as the older version.
//
// We want a logical comment/documentation diff.
// A magic comment's identity is a combination of:
// 1. The whitespace-insensitive content of the surrounding paragraph.
// 2. The whitespace-insensitive content of the surrounding group.
//    This is particularly true for SYNC blocks.

// Perform a correlation before and after.
// For comments which didn't have any changes, discard.
// For comments which did have changes, flag as potentially needing changes.
//
// For errors, try to see if we can parse the diff and provide a better error.
// E.g. if a def goes missing, check