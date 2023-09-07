// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(clippy::needless_return)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::redundant_pattern_matching)]
#![allow(clippy::redundant_static_lifetimes)]
#![allow(clippy::single_match)]

mod analysis;
mod backend;
mod core;
mod format_utils;
pub mod frontend;
mod miette_utils;
pub mod parse_clike;
mod utils;

use crate::frontend::Options;
use crate::{analysis::AnalysisErrorData, frontend::FrontendErrorData, miette_utils::ErrorCount};
use miette::{Diagnostic, LabeledSpan, Severity, SourceCode};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};

use pichpich_config::{ErrorConfig, ErrorLevel, WithErrorLevel};

pub fn main_command() -> clap::Command {
    clap::command!()
    .propagate_version(true)
    .subcommand_required(true)
    .arg_required_else_help(true)
    .subcommand(
        clap::Command::new("lint")
        .arg(clap::arg!(--check <pattern> "A pattern in the form of (ignore|warn|error):(all|check-name) to control severity levels for different checks"))
    )
}

pub fn main_impl(argv: impl IntoIterator<Item = std::ffi::OsString>) -> miette::Result<()> {
    let matches = main_command().get_matches_from(argv);

    match matches.subcommand() {
        Some(("lint", sub_matches)) => {
            return lint_main(sub_matches);
        }
        _ => unreachable!("Exhausted list of subcommands and subcommand_required prevents `None`"),
    }
}

fn lint_main(matches: &clap::ArgMatches) -> miette::Result<()> {
    let checks = matches
        .get_many::<String>("check")
        .unwrap_or_default()
        .cloned()
        .collect::<Vec<_>>();

    let mut error_config = ErrorConfig::default();
    // FIXME: Issue error for unknown arguments here.
    // Also add some way of doing snapshot tests at the CLI level...
    let _unknown_checks = error_config.populate(checks);

    let opts = Options {
        root: std::env::current_dir()
            .expect("failed to get the current directory; running in a sandbox?"),
        respect_ignore_file: true,
        error_config,
    };

    lint_main_impl(opts)?;
    Ok(())
}

pub fn lint_main_impl(opts: Options) -> miette::Result<()> {
    let (data, mut errors) = frontend::gather(&opts);
    let sem_errors = analysis::run(&opts, data);

    for err in sem_errors.into_iter() {
        errors.insert(err);
    }

    // FIXME: We should continue with analysis instead of exiting early.

    if !errors.is_empty() {
        Err(AllErrors {
            unrelated: errors.into_iter().map(AppErrorNewtype).collect(),
        })?
    }
    return Ok(());
}

#[derive(Debug)]
struct AllErrors {
    // Not sure why this property is called 'related', it just seems
    // to be a generic error grouping mechanism.
    unrelated: Vec<AppErrorNewtype>,
}

impl Error for AllErrors {}

impl Display for AllErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{} errors", self.error_count() - 1))
    }
}

impl Diagnostic for AllErrors {
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        Some(Box::new(
            self.unrelated.iter().map(|b| b as &dyn Diagnostic),
        ))
    }
}

pub type AppError = WithErrorLevel<AppErrorData>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AppErrorData {
    Frontend(FrontendErrorData),
    Analysis(AnalysisErrorData),
}

impl Display for AppErrorData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AppErrorData::Frontend(fed) => (fed as &dyn Display).fmt(f),
            AppErrorData::Analysis(aed) => (aed as &dyn Display).fmt(f),
        }
    }
}

pub struct AppErrorNewtype(pub AppError);

impl Error for AppErrorNewtype {}

impl Debug for AppErrorNewtype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (&self.0 as &dyn Debug).fmt(f)
    }
}

impl Display for AppErrorNewtype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (&self.0 as &dyn Display).fmt(f)
    }
}

macro_rules! delegate {
    ($selfid:ident, $id:ident) => {
        match &$selfid.0.error {
            AppErrorData::Frontend(fed) => fed.$id(),
            AppErrorData::Analysis(aed) => aed.$id(),
        }
    };
}

impl miette::Diagnostic for AppErrorNewtype {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        delegate!(self, code)
    }
    fn severity(&self) -> Option<Severity> {
        match self.0.level {
            ErrorLevel::Ignore => unreachable!("errors shouldn't be emitted with Ignore level"),
            ErrorLevel::Warn => Some(Severity::Warning),
            ErrorLevel::Error => Some(Severity::Error),
        }
    }
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        delegate!(self, help)
    }
    fn source_code(&self) -> Option<&dyn SourceCode> {
        delegate!(self, source_code)
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        delegate!(self, labels)
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        delegate!(self, related)
    }
}
