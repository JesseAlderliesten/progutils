# Changelog

## progutils 0.2.0

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.7.0` to
  `0.8.0` to update argument name `allow_zero` to `allow_zero_length`.

## progutils 0.1.0

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.6.0` to
  `0.7.0` to have the new default `all = FALSE` instead of `all = TRUE`
  in `make_natural()`.

## progutils 0.0.13

#### Breaking changes

- [`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md)
  gains argument `keep_integer` (default `TRUE`, in contrast to the
  implicit prior default `FALSE`), fixing issue
  [\#1](https://github.com/JesseAlderliesten/progutils/issues/1).

## progutils 0.0.12

#### Breaking changes

- `create_path`: rename to `create_file_path`. Replace non-alphanumeric
  characters other than dots and underscores by underscores instead of
  replacing non-alphanumeric characters other than underscores by dots.

#### Documentation

- [`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md):
  moved `Note` to `Details`. Condensed example section.
- [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md):
  add section `Usage in practice` which explains why using
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
  is preferable over using
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
- [`head_tail()`](https://jessealderliesten.github.io/progutils/reference/head_tail.md):
  no longer erroneously document that `n` can be zero. Removed
  uninformative example.
- [`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md):
  add section `Programming notes` about file separators.

## progutils 0.0.11

#### Breaking changes

- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  use
  [`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md)
  to check if `filename` is valid.
- `create_path`: use
  [`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)
  to check that `dir` is a valid path.
- [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md):
  only write in subdirectories of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and use
  [`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)
  to check that `subdir` is valid.
- `file_path_sans_ext()`: rename to
  [`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md)
  to distinguish it from
  [`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html)
  when linking to documentation; add argument `compression`.
- [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md):
  collapse leading and trailing whitespace and, if `ignore_newlines` is
  `TRUE`, leading and trailing newlines into a single blank character
  instead of removing them. Retains leading and trailing newlines if
  `ignore_newlines` is `FALSE`.

#### Added functions

- Add functions
  [`file_path_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
  [`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md)
  and
  [`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md).

## progutils 0.0.9

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.5.0` to
  `0.6.0`, needed to use re-exported
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html).
- [`progutils::paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html):
  move to `checkinput` and re-export it to `progutils`.

#### Added functions

- Add function
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
  to create a temporary directory that can safely be removed.

## progutils 0.0.8

#### Breaking changes

- Removed unused `check_os_is_windows()`.
- Use `roxygen2` version 8.0.0.
- [`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md):
  `x` and `table` have to be a vector or factor (as was always
  documented) to prevent returning `x` if `x` or `table` is a `list`.

#### Added functions

- Add
  [`head_tail()`](https://jessealderliesten.github.io/progutils/reference/head_tail.md)
  to show the first and last part of an object.

## progutils 0.0.7

#### Breaking changes

- [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html):
  return `""` as `'\"\"'` instead of `''`. Indicate the class of
  non-logical `NA`s. Do not allow list-type `x`. These changes also
  affect warnings and messages in other functions.
- [`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md):
  `new` is also processed through
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html)
  in the message indicating the replacement.
- [`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md):
  return `""` as `\"\"` instead of . Indicate the class of non-logical
  `NA`s. This change also affects warnings and messages in other
  functions.

## progutils 0.0.6

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.1.0` to
  `0.5.0`. This increases the minimum version of `R` to `4.1.0` but
  removes the dependency on `vctrs`.
- Dependency `tinytest`: declare version `>= 1.4.1` because argument
  `strict` is used in `expect_message()` and `expect_warning()`.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  `dir` ending in `\.` now is, as was documented, an error instead of
  silently denoting the working directory. Hardcode newlines instead of
  using
  [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)
  makes it easier to test warnings.

## progutils 0.0.5

#### Breaking changes

- [`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md):
  gained argument `signal_old_ignore_case`. Bugfix for, and more
  consistent handling of, `NA` in factors. Values in messages are no
  longer sorted alphabetically.

## progutils 0.0.4

#### Breaking changes

- [`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)
  gained argument `ignore_newlines` to pass to
  [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md).

#### Added functions

- `check_case`: check for values that differ only in their case.
- `replace_vals`: replace character values or factor levels.
- `signal_text`: signal text to a user through an error, warning, or
  message.
- `unpaste_unquote`: the opposite of
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html).

## progutils 0.0.3

#### Breaking changes

- `checkinput`: increased minimum version to `0.1.0`.
- Add `stats` and `tools` as dependencies in the `Suggests` field
  because they are linked to in help pages.
- No longer import
  [`methods::formalArgs()`](https://rdrr.io/r/methods/methodUtilities.html).
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  show the warnings if creating a directory fails.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  use `winslash = "/"` instead of `winslash = "\\"` to normalise paths.
- [`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md):
  use `winslash = "/"` instead of `winslash = "\\"` to normalise paths.
- `get_paths()`: removed. Use
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) instead.

#### Added functions

- `create_path()`: create file paths.
- `file_path_sans_ext()`: replacing
  [`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html),
  to also recognise the extension of file names that end in a dot.
- [`reorder_cols()`](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md):
  reorder columns.
- [`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md):
  reorder factor levels.

#### Documentation

- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  move some documentation to `create_path()`; document format of
  date-time stamp with same case as
  [`strftime()`](https://rdrr.io/r/base/strptime.html).

## progutils 0.0.2

NEWS for this and earlier versions has not been tracked.
