# progutils 0.0.3

### Breaking changes
- Increased minimum version of `checkinput` to 0.1.0.
- `create_dir()` now shows the warnings if creating a directory fails.
- `create_dir()` now use `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
- `get_filename()` now uses `winslash = "/"` instead of `winslash = "\\"` to
  normalise paths.
- Deleted `get_paths()`: use `.libPaths()` instead.

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


# progutils 0.0.2

NEWS for this and earlier versions has not been tracked.
