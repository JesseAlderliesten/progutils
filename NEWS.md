# progutils 0.8.0

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.12.0` to `1.0.0` to
  depend on a stable version.


# progutils 0.7.0

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.11.0` to `0.12.0` to
  use argument `require_sep` from `is_path()` in `create_file_path()` and in
  `create_tempdir()`.


# progutils 0.6.0

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.10.0` to `0.11.0` to
  incorporate change of argument name `allow_zero_length` to `allow_zerolength`.


# progutils 0.5.1

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.9.0` to `0.10.0` to
  incorporate bugfix: `is_path("C:")` would warn that `filename` should not
  contain `:`.

### Bugfixes
- `dir_create()` uses argument `recurse` instead of `recursive`, which is `TRUE`
  by default.
- `get_file_path()` would return directories matching `pattern`.


# progutils 0.5.0

### Breaking changes
- `create_dir()`: throw an error instead of returning the working directory if
  creating the directory fails: returning the working directory is an unsafe
  fall-back because the reasonable assumption that the newly-created directory
  did not yet exist before `create_dir()` was called then could lead to deleting
  the working directory.
- `create_file_path()` and `get_file_path()`: no need to warn about an
  already-existing directory with a path equal to a file path or about an
  already-existing file with a path equal to a directory path.


# progutils 0.4.0

### Breaking changes
- `is_path()`: move to package `checkinput`. Rename argument `path` to `x`.
  Adjusted to warn and return `FALSE` instead of throwing an error if `path` is
  not a valid path. Not allow filenames to start with a hyphen. Not warn about
  duplicated file separators or pointing to `tempdir()`.


# progutils 0.3.0

### Breaking changes
- Added dependency `fs` to `Imports`.
- `create_file_path()`: no longer replace non-alphanumeric characters in the
  filename because reconstituting the filename is brittle.
- Replace `file_path_no_ext()` by `fs::path_ext_remove()` and `file_path_ext()`  
  by `fs::path_ext()`.
- `get_filename()`: rename to `get_file_path()` and return the complete file
  path instead of only the file name.
- `is_filename()`: removed, use `is_path()`.
- `is_path()`: drop argument `as_file`. Instead, check that the filename is
  valid if a file extension is present.
- Replace `file.path(".", ...)` and `file.path(getwd(), ...)` with
  `fs::path_wd(...)`; replace other instances of `file.path()` with `fs::path()`.


# progutils 0.2.0

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.7.0` to `0.8.0` to
  update argument name `allow_zero` to `allow_zero_length`.


# progutils 0.1.0

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.6.0` to `0.7.0` to
  have the new default `all = FALSE` instead of `all = TRUE` in `make_natural()`.


# progutils 0.0.13

### Breaking changes
- `as.numeric_safe()` gains argument `keep_integer` (default `TRUE`, in contrast
  to the implicit prior default `FALSE`), fixing issue #1.


# progutils 0.0.12

### Breaking changes
- `create_path`: rename to `create_file_path`. Replace non-alphanumeric
  characters other than dots and underscores by underscores instead of replacing
  non-alphanumeric characters other than underscores by dots.


# progutils 0.0.11

### Breaking changes
- `create_dir()`: use `is_filename()` to check if `filename` is valid.
- `create_path`: use `is_path()` to check that `dir` is a valid path.
- `create_tempdir()`: only write in subdirectories of `tempdir()` and use
  `is_path()` to check that `subdir` is valid.
- `file_path_sans_ext()`: rename to `file_path_no_ext()` to distinguish it from
  `tools::file_path_sans_ext()` when linking to documentation; add argument
  `compression`.
- `wrap_text()`: collapse leading and trailing whitespace and, if
  `ignore_newlines` is `TRUE`, leading and trailing newlines into a single blank
  character instead of removing them. Retains leading and trailing newlines if
  `ignore_newlines` is `FALSE`.

### Added functions
- Add functions `file_path_ext()`, `is_filename()` and `is_path()`.


# progutils 0.0.9

### Breaking changes
- Dependency `checkinput`: increase minimum version from `0.5.0` to `0.6.0`,
  needed to use re-exported `paste_quoted()`.
- `progutils::paste_quoted()`: move to `checkinput` and re-export it to
  `progutils`.

### Added functions
- Add function `create_tempdir()` to create a temporary directory that can
  safely be removed.


# progutils 0.0.8

### Breaking changes
- Removed unused `check_os_is_windows()`.
- Use `roxygen2` version 8.0.0.
- `not_in()`: `x` and `table` have to be a vector or factor (as was always
  documented) to prevent returning `x` if `x` or `table` is a `list`.

### Added functions
- Add `head_tail()` to show the first and last part of an object.


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


# progutils 0.0.5

### Breaking changes
- `replace_vals()`: gained argument `signal_old_ignore_case`. Bugfix for, and
  more consistent handling of, `NA` in factors. Values in messages are no longer
  sorted alphabetically.


# progutils 0.0.4

### Breaking changes
- `vect_to_char()` gained argument `ignore_newlines` to pass to `wrap_text()`.

### Added functions
- `check_case`: check for values that differ only in their case.
- `replace_vals`: replace character values or factor levels.
- `signal_text`: signal text to a user through an error, warning, or message.
- `unpaste_unquote`: the opposite of `paste_quoted()`.


# progutils 0.0.3

### Breaking changes
- `checkinput`: increased minimum version to `0.1.0`.
- Add dependencies `stats` and `tools` to `Suggests` because they are linked to
  in help pages.
- No longer import `methods::formalArgs()`.
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
