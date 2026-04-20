# progutils 0.0.6

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.1.0` to `0.5.0`.
  This increases the minimum version of `R` to `4.1.0` but removes the
  dependency on `vctrs`.
- `create_dir()`: `dir` ending in `\.` now is, as was documented, an error
  instead of silently denoting the working directory. Hardcoding newlines instead of
  using `wrap_text()` makes it easier to test warnings.

### Miscellaneous
- Run tests when checking the package. Adjusted tests and example to accommodate
  bugfix to tools::file_path_sans_ext() in R 4.6.0.
- `check_case()` and `replace_vals()`: outcomment failing tests that seem to
  fail on sort order for uppercase vs. lowercase characters (differences caused
  by locale settings (see `Sys.getlocale()`)?
- Make the location of newlines more predictable by hardcoding newlines using
 `\n` instead of using `wrap_text()` in warnings.
- Combine elements of workflows `check-standard.yaml` and `check-no-suggests.yaml`
  to check if the package functions correctly without the dependencies listed in
  'Suggests' which I use for documentation.


# progutils 0.0.5

### Breaking changes
- `replace_vals()`: gained argument `signal_old_ignore_case`. Bugfix for, and
  more consistent handling of, `NA`s in factors. Values in messages are no
  longer sorted alphabetically.

### Miscellaneous
- No need to import `osVersion` from `utils` because `utils` itself is imported.
- GitHub action `check-standard` now also runs on R 4.1.0 on ubuntu and Windows,
  is triggered every Saturday on 04:23 UTC, and can be triggered manually
  (trigger it once manually on the main branch to be able to trigger it manually
  on other branches).


# progutils 0.0.4

### Breaking changes
- `vect_to_char()` gained argument `ignore_newlines` to pass to `wrap_text()`.

### Added functions
- `check_case`: check for values that differ only in their case.
- `replace_vals`: to replace character values or factor levels.
- `signal_text`: to signal text to a user through an error, warning, or message.
- `unpaste_unquote`: the opposite of `paste_quoted()`.

### Miscellaneous
- `progutils` now uses GitHub action `check-standard` on all branches.


# progutils 0.0.3

### Breaking changes
- Increased minimum version of `checkinput` to 0.1.0.
- `create_dir()` now shows the warnings if creating a directory fails.
- `create_dir()` now use `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
- `get_filename()` now uses `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
- `get_paths()`: deleted. Use `.libPaths()` instead.

### Added functions
- `create_path()` to create file paths.
- `file_path_sans_ext()`, replacing `tools::file_path_sans_ext()`, to also
  recognise the extension of file names that end in a dot.
- `reorder_cols` to reorder columns.
- `reorder_levels` to reorder factor levels.

### Updated documentation
- Added `stats` and `tools` as dependencies in the `Suggests` field because they
  are linked to in help pages.
- No longer import `methods::formalArgs()`.
- Replaced section title `Note` (created through @Note) by section title 'Notes'
  (created through @section Notes). Idem for 'Programming note'.
- `create_dir()`: move some documentation to `create_path()`; document format of
  date-time stamp with same case as `strftime()`.

### Miscellaneous ###
- `progutils` now uses GitHub action `check-standard` on all branches (see
  `?usethis::use_github_action()`).


# progutils 0.0.2

NEWS for this and earlier versions has not been tracked.
