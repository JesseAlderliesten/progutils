# devel
- Nothing yet.


# progutils 0.0.5

### Breaking changes
- `replace_vals()`: gained argument `signal_old_ignore_case`. Bugfix for, and
  more consistent handling of, `NA`s in factors. Values in messages are no
  longer sorted alphabetically.

### Miscellaneous
- No need to import `osVersion` from `utils` because `utils` itself is imported.


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
