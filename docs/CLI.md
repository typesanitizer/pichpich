# pichpich CLI reference

```
Usage: pichpich <COMMAND>

Commands:
  lint  
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help information
  -V, --version  Print version information

```

## Checks

The list of check names supported by `pichpich lint --check <check-name>`
are described below, along with the default level (ignore/warn/error).

* `missing-def-and-ref` (default level: ignore): The `attribute list` is missing both 'def:' and 'ref:' keys, preventing cross-referencing.
* `empty-attr-key-or-value` (default level: error): The `attribute list` contains keys and/or values which are empty.
* `spaces-in-attr-key-or-value` (default level: warn): The `attribute list` includes keys and/or values which use whitespace, without explicit surrounding quotes.
* `malformed-attr` (default level: ignore): The `attribute list` includes a field that couldn't be parsed as key: value
* `unknown-attr-key` (default level: ignore): The `attribute list` includes a key that is not recognized by pichpich.
* `conflicting-keys` (default level: error): The `attribute list` either includes a key repeated multiple times, or includes both 'def' and 'ref'.
* `undefined-ref` (default level: error): There is no definition for the unique ID specified in the 'ref:' attribute.
* `inconsistent-id-kind` (default level: error): The same id is used for different kinds of magic comments, e.g. NOTE(def: my-id) and TODO(ref: my-id).

