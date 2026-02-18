# progutils (development version)
The development-branch of `progutils` is *work in progress*: see below for the
released versions.

### Breaking changes
- Increased minimum version of `checkinput` to 0.0.6.
- Deleted function `get_paths()`: use `.libPaths()` instead.

### Added functions
- `create_path()` to create file paths.

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
