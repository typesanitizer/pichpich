#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorLevel {
    Ignore,
    Warn,
    Error,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithErrorLevel<T> {
    pub(crate) error: T,          // deliberately first for proper sorting
    pub(crate) level: ErrorLevel, // Only Warn or Error
}

impl<T> WithErrorLevel<T> {
    pub fn map<U>(self, f: &dyn Fn(T) -> U) -> WithErrorLevel<U> {
        WithErrorLevel {
            level: self.level,
            error: f(self.error),
        }
    }
}

impl<T: Display> Display for WithErrorLevel<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(if self.level == Warn {
            "warning: "
        } else {
            "error: "
        })?;
        (&self.error as &dyn Display).fmt(f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AppErrorCode {
    MissingDefAndRef = 0,
    EmptyAttributeKeyOrValue = 1,
    UnexpectedSpaces = 2,
    UnknownAttributeKey = 3,
    UndefinedRef = 4,
    InconsistentIdKind = 5,
}

impl Display for AppErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(DEFAULT_ERROR_INFO[*self as usize].2)
    }
}

use std::fmt::{Debug, Display, Formatter};
use AppErrorCode::*;
use ErrorLevel::*;

pub const DEFAULT_ERROR_INFO: [(AppErrorCode, ErrorLevel, &'static str, &'static str); 6] = [
    (MissingDefAndRef, Ignore, "missing-def-and-ref",
    "The `attribute list` is missing both 'def:' and 'ref:' keys, preventing cross-referencing."),
    (EmptyAttributeKeyOrValue, Error, "empty-attr-key-or-value",
    "The `attribute list` is keys and/or values which are empty."),
    (UnexpectedSpaces, Warn, "spaces-in-attr-key-or-value",
     "The `attribute list` includes keys and/or values which use whitespace, without explicit \
     surrounding quotes."),
    (UnknownAttributeKey, Ignore, "unknown-attr-key",
    "The `attribute list` includes a key that is not recognized by pichpich."),
    (UndefinedRef, Error, "undefined-ref",
    "There is no definition for the unique ID specified in the 'ref:' attribute."),
    (InconsistentIdKind, Error, "inconsistent-id-kind",
     "The same id is used for different kinds of magic comments, e.g. NOTE(def: my-id) and TODO(ref: my-id).")
];

#[derive(Debug, Clone)]
pub struct ErrorConfig {
    levels: Vec<ErrorLevel>,
}

impl Default for ErrorConfig {
    fn default() -> Self {
        ErrorConfig {
            levels: DEFAULT_ERROR_INFO.iter().map(|tup| tup.1).collect(),
        }
    }
}

impl ErrorConfig {
    pub fn populate(&mut self, args: Vec<String>) -> Result<(), Vec<String>> {
        let mut unknown = vec![];
        for arg in args.into_iter() {
            self.populate_one(arg, &mut unknown);
        }
        if unknown.is_empty() {
            return Ok(());
        }
        return Err(unknown);
    }
    fn populate_one(&mut self, arg: String, err: &mut Vec<String>) {
        if let Some((lhs, rhs)) = arg.split_once(':') {
            match lhs {
                "ignore" | "warn" | "error" => {
                    let new_level = if lhs == "ignore" {
                        Ignore
                    } else if lhs == "warn" {
                        Warn
                    } else {
                        Error
                    };
                    if rhs == "all" {
                        for level in self.levels.iter_mut() {
                            *level = new_level;
                        }
                        return;
                    }
                    if let Some(tup) = DEFAULT_ERROR_INFO.iter().find(|tup| tup.2 == rhs) {
                        self.levels[tup.0 as usize] = new_level;
                        return;
                    }
                }
                _ => {}
            }
        }
        err.push(arg);
    }
    pub fn set_warnings_as_errors(&mut self) {
        for level in self.levels.iter_mut() {
            if *level == Warn {
                *level = Error;
            }
        }
    }
    pub(crate) fn get_level(&self, code: AppErrorCode) -> Option<ErrorLevel> {
        let level = self.levels[code as usize];
        match level {
            Ignore => None,
            Warn | Error => Some(level),
        }
    }
}
