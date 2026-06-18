# Changelog

## progutils 0.10.1

#### Documentation

- [`signal_text()`](https://jessealderliesten.github.io/progutils/reference/signal_text.md):
  document and show example how to make `text` available for further
  queries.
- Update grouping of functions in the `See Also` sections and in the
  package index on the website, adding the re-exported function
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html).
- Apply a few more stylistic suggestions provided by various packages
  like `goodpractice` that check code sanity.

## progutils 0.10.0

#### Bugfixes

- Fix pattern used to detect backslashes in `pattern`.

#### Documentation

- Apply stylistic suggestions provided by various packages like
  `goodpractice` that check code sanity.

## progutils 0.9.0

#### Breaking changes

- Update
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md):
  always create a new temporary directory instead of throwing an error
  if `subdir` is re-used. Not allow creating recursive subdirectories:
  cleaning up such directories is unsafe. Rename argument `subdir` to
  `pattern`.

## progutils 0.8.0

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.12.0` to
  `1.0.0` to depend on a stable version.

## progutils 0.7.0

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.11.0` to
  `0.12.0` to use argument `require_sep` from `is_path()` in
  [`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
  and in
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md).

## progutils 0.6.0

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.10.0` to
  `0.11.0` to incorporate change of argument name `allow_zero_length` to
  `allow_zerolength`.

## progutils 0.5.1

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.9.0` to
  `0.10.0` to incorporate bugfix: `is_path("C:")` would warn that
  `filename` should not contain `:`.

#### Bugfixes

- `dir_create()` uses argument `recurse` instead of `recursive`, which
  is `TRUE` by default.
- [`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)
  would return directories matching `pattern`.

## progutils 0.5.0

#### Breaking changes

- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  throw an error instead of returning the working directory if creating
  the directory fails: returning the working directory is an unsafe
  fall-back because the reasonable assumption that the newly-created
  directory did not yet exist before
  [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
  was called then could lead to deleting the working directory.
- [`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
  and
  [`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md):
  no need to warn about an already-existing directory with a path equal
  to a file path or about an already-existing file with a path equal to
  a directory path.

## progutils 0.4.0

#### Breaking changes

- `is_path()`: move to package `checkinput`. Rename argument `path` to
  `x`. Adjusted to warn and return `FALSE` instead of throwing an error
  if `path` is not a valid path. Not allow filenames to start with a
  hyphen. Not warn about duplicated file separators or pointing to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## progutils 0.3.0

#### Breaking changes

- Added dependency `fs` to `Imports`.
- [`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md):
  no longer replace non-alphanumeric characters in the filename because
  reconstituting the filename is brittle.
- Replace `file_path_no_ext()` by
  [`fs::path_ext_remove()`](https://fs.r-lib.org/reference/path_file.html)
  and `file_path_ext()`  
  by [`fs::path_ext()`](https://fs.r-lib.org/reference/path_file.html).
- `get_filename()`: rename to
  [`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)
  and return the complete file path instead of only the file name.
- `is_filename()`: removed, use `is_path()`.
- `is_path()`: drop argument `as_file`. Instead, check that the filename
  is valid if a file extension is present.
- Replace `file.path(".", ...)` and `file.path(getwd(), ...)` with
  `fs::path_wd(...)`; replace other instances of
  [`file.path()`](https://rdrr.io/r/base/file.path.html) with
  [`fs::path()`](https://fs.r-lib.org/reference/path.html).

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

## progutils 0.0.11

#### Breaking changes

- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  use `is_filename()` to check if `filename` is valid.
- `create_path`: use `is_path()` to check that `dir` is a valid path.
- [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md):
  only write in subdirectories of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and use
  `is_path()` to check that `subdir` is valid.
- `file_path_sans_ext()`: rename to `file_path_no_ext()` to distinguish
  it from
  [`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html)
  when linking to documentation; add argument `compression`.
- [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md):
  collapse leading and trailing whitespace and, if `ignore_newlines` is
  `TRUE`, leading and trailing newlines into a single blank character
  instead of removing them. Retains leading and trailing newlines if
  `ignore_newlines` is `FALSE`.

#### Added functions

- Add functions `file_path_ext()`, `is_filename()` and `is_path()`.

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
- Add dependencies `stats` and `tools` to `Suggests` because they are
  linked to in help pages.
- No longer import
  [`methods::formalArgs()`](https://rdrr.io/r/methods/methodUtilities.html).
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  show the warnings if creating a directory fails.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  use `winslash = "/"` instead of `winslash = "\\"` to normalise paths.
- `get_filename()`: use `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
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
