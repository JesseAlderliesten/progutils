# Create a file path

Create a file path, creating the indicated directory if it does not yet
exist.

## Usage

``` r
create_file_path(
  filename,
  format_stamp = "%Y_%m_%d_%H_%M_%S",
  dir = fs::path(".", "output"),
  add_date = TRUE
)
```

## Arguments

- filename:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the file name, including the file extension like `.csv` or
  `.txt`. Should adhere to the restrictions described in
  [`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html).

- format_stamp:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  indicating the [format](https://rdrr.io/r/base/strptime.html) of the
  stamp to be added in front of the file name. No stamp is added if
  `format_stamp` is an empty string (i.e., `""`). The formatted stamp is
  treated as part of the filename, such that the same restrictions apply
  to `format_stamp` as to `filename`, see `Details`.

- dir:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  containing a [valid
  path](https://jessealderliesten.github.io/checkinput/reference/is_path.html),
  see `Details` and the section `Notes on paths` below.

- add_date:

  `TRUE` or `FALSE`: create a subdirectory in `dir` with the current
  date in the [format](https://rdrr.io/r/base/strptime.html)
  `YYYY_mm_dd`?

## Value

A character string with the created [absolute
normalized](https://fs.r-lib.org/reference/path_math.html) path,
returned [invisibly](https://rdrr.io/r/base/invisible.html), see the
section `Notes on paths` below.

An error is thrown if the attempt to create a file path fails. A warning
is issued if the file already
[exists](https://fs.r-lib.org/reference/is_file.html), see `Details`.

## Details

`filename` **should** contain a file extension (i.e., a dot followed by
any character until the end of the file name) and should **not** contain
slashes or backslashes: use `dir` to indicate subdirectories.

The path in `dir` is interpreted as a **relative** path to the directory
that should be created, starting from the [working
directory](https://rdrr.io/r/base/getwd.html), see the section
`Notes on paths` below.

A warning is issued if the **file** indicated by the returned path
already exists. To prevent this when creating files in quick succession,
use `"%OSn"` as part of `format_stamp` to create precise stamps by
truncating seconds to `0 <= n <= 6` decimal places, see
[`strftime()`](https://rdrr.io/r/base/strptime.html) for details.

`create_file_path()` creates the directory but does **not** create the
file: wrap the call to `create_file_path()` in
[`fs::file_create()`](https://fs.r-lib.org/reference/create.html) or in
[`file.create()`](https://rdrr.io/r/base/files.html) to create the file.

Calls where date **nor** time stamps are added (i.e, using
`format_stamp = ""` and `add_date = FALSE`) can be replaced by
`fs::path(fs::dir_create(path = "<dir>"), "<filename>")` to **not** get
a warning if the file already
[exists](https://fs.r-lib.org/reference/is_file.html).

## Side effects

The directory indicated by the returned path is
[created](https://fs.r-lib.org/reference/create.html) if it does not yet
[exist](https://fs.r-lib.org/reference/file_access.html).

## Notes on paths

The file separator is a backslash (`\`) on Windows but a forward slash
(`/`) on other operating systems: use
[`file.path()`](https://rdrr.io/r/base/file.path.html) or
[`fs::path()`](https://fs.r-lib.org/reference/path.html) to construct
paths in a platform-independent way and
[.Platform\$file.sep](https://rdrr.io/r/base/Platform.html) to get the
file separator used on the current platform.

Furthermore, the backslash is also used as [escape
character](https://rdrr.io/r/base/regex.html) in R, such that
backslashes need to be escaped in R code by doubling them. Use `cat(x)`
to omit the escape characters to see how `x` would be printed.

The [working directory](https://rdrr.io/r/base/getwd.html) can be
indicated by a single dot (`"."`) and the parent directory can be
indicated by two dots (`".."`).

**relative** paths start from the [working
directory](https://rdrr.io/r/base/getwd.html). In contrast, **absolute**
paths start from the root directory and thus give the complete path,
such that the path still points to the same directory if the [working
directory](https://rdrr.io/r/base/getwd.html) changes.

Paths can be normalised, e.g., through
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) or
[`fs::path_norm()`](https://fs.r-lib.org/reference/path_math.html)
(which is also called by
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html)). On
case-insensitive file systems, this also adjusts the case to match names
of directories that are already present.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, with extensive references about file paths
and directories;
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)
to check if a file exists and is a unique match to a pattern;
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
(used by this function) to create a directory if it does not yet exist

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
# Use a temporary directory to not write in the user's directory
my_tempdir <- create_tempdir(prefix = "examplecreatefilepath")

(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = my_tempdir, add_date = TRUE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/2026_10_16/abc.txt
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = TRUE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/2026_10_16/16_10_2026_abc.txt
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = my_tempdir, add_date = FALSE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/def.html
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = FALSE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/16_10_2026_def.html
(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/subdir/2026_10_16/abc.txt
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/subdir/2026_10_16/16_10_2026_abc.txt
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/subdir/def.html
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#> /tmp/RtmpgUrBhj/examplecreatefilepath19df5d4e4cbe/subdir/16_10_2026_def.html

# Cleaning up
unlink(x = my_tempdir, recursive = TRUE)
rm(my_tempdir)
```
