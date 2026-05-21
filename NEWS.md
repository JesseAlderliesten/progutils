# progutils 0.0.11

### Breaking changes
- `create_dir()` now uses `is_filename()` to check if `filename` is valid.
- `create_path` now uses `is_path()` to check that `dir` is a valid path.
- `create_tempdir()` now only writes in subdirectories of `tempdir()` and uses
  `is_path()` to check that `subdir` is valid.
- `file_path_sans_ext()`: rename to `file_path_no_ext()` to distinguish it from
  `tools::file_path_sans_ext()` when linking to documentation; add argument
  `compression`.
- `wrap_text()` collapses leading and trailing whitespace and, if
  `ignore_newlines` is `TRUE`, leading and trailing newlines into a single blank
  character instead of removing them; and retains leading and trailing newlines
  if `ignore_newlines` is `FALSE`.
- Add functions `file_path_ext()`, `is_filename()` and `is_path()`.

### Miscellaneous
- `NEWS`: stylistic update.
- `README`: refer to website when appropriate. Stylistic update.


# progutils 0.0.10

### Miscellaneous
- Add pkgdown website: `https://jessealderliesten.github.io/progutils/`.


# progutils 0.0.9

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.5.0` to `0.6.0`,
  needed to use re-exported `paste_quoted()`.
- `progutils::paste_quoted()`: move to `checkinput` and re-export it to
  `progutils`.
- Add function `create_tempdir()` to create a temporary directory that can
  safely be removed.


# progutils 0.0.8

### Breaking changes
- Removed unused `check_os_is_windows()`.
- Added `head_tail()` to show the `head` and `tail` of an object.
- Use `roxygen2` version 8.0.0.
- `not_in()`: `x` and `table` have to be a vector or factor (as was always
  documented) to prevent returning `x` if `x` or `table` is a `list`.


# progutils 0.0.7

### Breaking changes
- `paste_quoted()`: return `""` as `'\"\"'` instead of `''`. Indicate the class
  of non-logical `NA`s. Do not allow list-type `x`. These changes also affect
  warnings and messages in other functions.
- `replace_vals()`: `new` is also processed through `paste_quoted()` in the
  message indicating the replacement.
- `vect_to_char()`: return `""` as `\"\"` instead of ` `. Indicate the class of
  non-logical `NA`s. This change also affects warnings and messages in other
  functions.


# progutils 0.0.6

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.1.0` to `0.5.0`.
  This increases the minimum version of `R` to `4.1.0` but removes the
  dependency on `vctrs`.
- Dependency `tinytest`: declare version `>= 1.4.1` because argument `strict`
  is used in `expect_message()` and `expect_warning()`.
- `create_dir()`: `dir` ending in `\.` now is, as was documented, an error
  instead of silently denoting the working directory. Hardcode newlines instead
  of using `wrap_text()` makes it easier to test warnings.

### Miscellaneous
- Run tests when checking the package. Adjusted tests and example to accommodate
  bugfix to `tools::file_path_sans_ext()` in `R 4.6.0`.
- `check_input()` and `replace_vals()`: update tests that failed because of
  different sort order for uppercase vs. lowercase characters caused by locale
  settings (see `Sys.getlocale()`). This also affects the order of factor levels
  and thus the output of `as.factor()`.
- Make the location of newlines more predictable by hardcoding newlines using
 `\n` instead of using `wrap_text()` in warnings.
- Combine elements of workflows `check-standard.yaml` and `check-no-suggests.yaml`
  to check if the package functions correctly without the dependencies listed in
  `Suggests` which I use for documentation.
- Note that `not_in()` does not consider names when matching.


# progutils 0.0.5

### Breaking changes
- `replace_vals()`: gained argument `signal_old_ignore_case`. Bugfix for, and
  more consistent handling of, `NA` in factors. Values in messages are no longer
  sorted alphabetically.

### Miscellaneous
- No need to import `osVersion` from `utils` because `utils` itself is imported.
- GitHub action `check-standard` now also runs on `R 4.1.0` on ubuntu and Windows,
  is triggered every Saturday on 04:23 UTC, and can be triggered manually
  (trigger it once manually on the main branch to be able to trigger it manually
  on other branches).


# progutils 0.0.4

### Breaking changes
- `vect_to_char()` gained argument `ignore_newlines` to pass to `wrap_text()`.

### Added functions
- `check_case`: check for values that differ only in their case.
- `replace_vals`: replace character values or factor levels.
- `signal_text`: signal text to a user through an error, warning, or message.
- `unpaste_unquote`: the opposite of `paste_quoted()`.

### Miscellaneous
- `progutils` now uses GitHub action `check-standard` on all branches.


# progutils 0.0.3

### Breaking changes
- `checkinput`: increased minimum version to `0.1.0`.
- `create_dir()`: show the warnings if creating a directory fails.
- `create_dir()`: use `winslash = "/"` instead of `winslash = "\\"` to normalise
  paths.
- `get_filename()`: use `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
- `get_paths()`: removed. Use `.libPaths()` instead.

### Added functions
- `create_path()`: create file paths.
- `file_path_sans_ext()`: replacing `tools::file_path_sans_ext()`, to also
  recognise the extension of file names that end in a dot.
- `reorder_cols()`: reorder columns.
- `reorder_levels()`: reorder factor levels.

### Updated documentation
- Add `stats` and `tools` as dependencies in the `Suggests` field because they
  are linked to in help pages.
- No longer import `methods::formalArgs()`.
- Replace section title `Note` (created through `@Note`) by section title
  `Notes` (created through `@section Notes`). Idem for `Programming note`.
- `create_dir()`: move some documentation to `create_path()`; document format of
  date-time stamp with same case as `strftime()`.

### Miscellaneous ###
- `progutils` now uses GitHub action `check-standard` on all branches (see
  `?usethis::use_github_action()`).


# progutils 0.0.2

NEWS for this and earlier versions has not been tracked.
