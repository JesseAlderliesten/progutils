# Changelog

## progutils 0.0.10

#### Miscellaneous

- Added pkgdown website:
  `https://jessealderliesten.github.io/progutils/`.

## progutils 0.0.9

#### Breaking changes

- Dependency `checkinput`: increase minimum version from `0.5.0` to
  `0.6.0`, needed to use re-exported
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html).
- [`progutils::paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html):
  move it to `checkinput` and re-export it to `progutils`.
- Add function
  [`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
  to create a temporary directory that can safely be removed.

## progutils 0.0.8

#### Breaking changes

- Removed unused `check_os_is_windows()`.
- Added
  [`head_tail()`](https://jessealderliesten.github.io/progutils/reference/head_tail.md)
  to show the `head` and `tail` of an object.
- Use `roxygen2` version 8.0.0.
- [`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md):
  `x` and `table` have to be a vector or factor (as was always
  documented) to prevent returning `x` if `x` or `table` is a list.

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
- Dependency `tinytest`: declare version `>= 1.4.1` because I use
  argument `strict` in `expect_message()` and `expect_warning()`.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  `dir` ending in `\.` now is, as was documented, an error instead of
  silently denoting the working directory. Hardcoding newlines instead
  of using
  [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)
  makes it easier to test warnings.

#### Miscellaneous

- Run tests when checking the package. Adjusted tests and example to
  accommodate bugfix to
  [`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html)
  in `R 4.6.0`.
- `check_input()` and
  [`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md):
  updated tests that failed because of different sort order for
  uppercase vs. lowercase characters caused by locale settings (see
  [`Sys.getlocale()`](https://rdrr.io/r/base/locales.html)). This also
  affects the order of factor levels and thus the output of
  [`as.factor()`](https://rdrr.io/r/base/factor.html).
- Make the location of newlines more predictable by hardcoding newlines
  using `\n` instead of using
  [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)
  in warnings.
- Combine elements of workflows `check-standard.yaml` and
  `check-no-suggests.yaml` to check if the package functions correctly
  without the dependencies listed in `Suggests` which I use for
  documentation.
- Note that
  [`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md)
  does not consider names when matching.

## progutils 0.0.5

#### Breaking changes

- [`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md):
  gained argument `signal_old_ignore_case`. Bugfix for, and more
  consistent handling of, `NA`s in factors. Values in messages are no
  longer sorted alphabetically.

#### Miscellaneous

- No need to import `osVersion` from `utils` because `utils` itself is
  imported.
- GitHub action `check-standard` now also runs on `R 4.1.0` on ubuntu
  and Windows, is triggered every Saturday on 04:23 UTC, and can be
  triggered manually (trigger it once manually on the main branch to be
  able to trigger it manually on other branches).

## progutils 0.0.4

#### Breaking changes

- [`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)
  gained argument `ignore_newlines` to pass to
  [`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md).

#### Added functions

- `check_case`: check for values that differ only in their case.
- `replace_vals`: to replace character values or factor levels.
- `signal_text`: to signal text to a user through an error, warning, or
  message.
- `unpaste_unquote`: the opposite of
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html).

#### Miscellaneous

- `progutils` now uses GitHub action `check-standard` on all branches.

## progutils 0.0.3

#### Breaking changes

- Increased minimum version of `checkinput` to `0.1.0`.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
  now shows the warnings if creating a directory fails.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
  now use `winslash = "/"` instead of `winslash = "\\"` to normalise
  paths.
- [`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)
  now uses `winslash = "/"` instead of `winslash = "\\"` to normalise
  paths.
- `get_paths()`: deleted. Use
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) instead.

#### Added functions

- [`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md)
  to create file paths.
- [`file_path_sans_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_sans_ext.md),
  replacing
  [`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html),
  to also recognise the extension of file names that end in a dot.
- `reorder_cols` to reorder columns.
- `reorder_levels` to reorder factor levels.

#### Updated documentation

- Added `stats` and `tools` as dependencies in the `Suggests` field
  because they are linked to in help pages.
- No longer import
  [`methods::formalArgs()`](https://rdrr.io/r/methods/methodUtilities.html).
- Replaced section title `Note` (created through `@Note`) by section
  title `Notes` (created through `@section Notes`). Idem for
  `Programming note`.
- [`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md):
  move some documentation to
  [`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md);
  document format of date-time stamp with same case as
  [`strftime()`](https://rdrr.io/r/base/strptime.html).

#### Miscellaneous

- `progutils` now uses GitHub action `check-standard` on all branches
  (see `?usethis::use_github_action()`).

## progutils 0.0.2

NEWS for this and earlier versions has not been tracked.
