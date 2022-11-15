// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{
    cmp::Ordering,
    collections::hash_map::Entry,
    collections::HashMap,
    fmt,
    fmt::{Debug, Formatter},
    hash::{Hash, Hasher},
    ops::Deref,
};

pub fn merge_map_vec<K: Eq + Hash, V>(m1: &mut HashMap<K, Vec<V>>, mut m2: HashMap<K, Vec<V>>) {
    if m1.len() < m2.len() {
        std::mem::swap(m1, &mut m2);
    }
    for (k, mut vs) in m2.into_iter() {
        match m1.entry(k) {
            Entry::Occupied(mut o) => o.get_mut().append(&mut vs),
            Entry::Vacant(v) => {
                v.insert(vs);
            }
        }
    }
}

/// Newtype wrapper to ignore a type for comparison.
#[derive(Copy, Clone, Default)]
pub struct AllEquivalent<T> {
    pub value: T,
}

impl<T> Deref for AllEquivalent<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T: Debug> Debug for AllEquivalent<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T> PartialEq for AllEquivalent<T> {
    fn eq(&self, _: &Self) -> bool {
        return true;
    }
}
impl<T> Eq for AllEquivalent<T> {}

impl<T> PartialOrd for AllEquivalent<T> {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        return Some(Ordering::Equal);
    }
}
impl<T> Ord for AllEquivalent<T> {
    fn cmp(&self, _: &Self) -> Ordering {
        return Ordering::Equal;
    }
}

impl<T> Hash for AllEquivalent<T> {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {} // For consistency
}

pub(crate) trait AdjustOffsets {
    fn adjust_offsets(self, base_offset: usize) -> Self;
}
