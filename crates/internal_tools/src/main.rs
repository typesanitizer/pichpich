#![allow(clippy::expect_fun_call)]
#![allow(clippy::needless_return)]

use std::fmt::Write;

use pichpich_config::DEFAULT_ERROR_INFO;

fn main() {
    let matches = clap::command!()
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(clap::Command::new("create-docs"))
        .get_matches();

    let doc_contents = vec![("docs/CLI.md", cli_reference)];

    match matches.subcommand() {
        Some(("create-docs", _sub_matches)) => {
            let lockfile_path = std::env::current_dir()
                .expect("failed to get current dir")
                .join("Cargo.lock");
            match lockfile_path.try_exists() {
                Ok(true) => {}
                _ => panic!("Expected pichpich-dev to be invoked from project root"),
            };
            for (path, make_doc) in doc_contents {
                let path = std::path::Path::new(path);
                if let Some(parent) = path.parent() {
                    std::fs::create_dir_all(parent).expect(&format!(
                        "failed to create directory {}",
                        parent.to_string_lossy()
                    ));
                }
                std::fs::write(path, make_doc()).expect(&format!(
                    "failed to write to file {}",
                    path.to_string_lossy()
                ));
            }
        }
        _ => unreachable!("Exhausted list of subcommands and subcommand_required prevents `None`"),
    }
}

fn cli_reference() -> String {
    let template = "# pichpich CLI reference

```
{{lint-help-text}}
```

## Checks

The list of check names supported by `pichpich lint --check <check-name>`
are described below, along with the default level (ignore/warn/error).

{{check-list}}
";
    let lint_help_text = pichpich::main_command().render_help().to_string();
    let mut check_list = String::new();
    for tup in DEFAULT_ERROR_INFO {
        let (check_name, default_level, help_text) = (tup.2, tup.1, tup.3);
        writeln!(
            &mut check_list,
            "* `{check_name}` (default level: {default_level}): {help_text}"
        )
        .unwrap();
    }
    return template
        .replace("{{lint-help-text}}", &lint_help_text)
        .replace("{{check-list}}", &check_list);
}
