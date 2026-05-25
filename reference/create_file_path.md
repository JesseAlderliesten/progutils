# Create a file path

Create a file path, creating the indicated directory if it does not yet
exist.

## Usage

``` r
create_file_path(
  filename,
  format_stamp = "%Y_%m_%d_%H_%M_%S",
  dir = file.path(".", "output"),
  add_date = TRUE
)
```

## Arguments

- filename:

  A character string with the file name, including the file extension
  like `.csv` or `.txt`.

- format_stamp:

  A character string indicating the
  [format](https://rdrr.io/r/base/strptime.html) of the stamp to be
  added in front of the file name. No stamp is added if `format_stamp`
  is an empty string (i.e., `""`). The formatted stamp is treated as
  part of the filename, such that the same restrictions apply, see
  `Details`.

- dir:

  Non-empty [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  containing a [valid
  path](https://jessealderliesten.github.io/progutils/reference/is_path.md)
  to a directory that should be created if it does not yet exist. A dot
  (i.e., `"."`) indicates the current working directory.

- add_date:

  `TRUE` or `FALSE`: create a subdirectory in `dir` with the current
  date in the [format](https://rdrr.io/r/base/strptime.html)
  `YYYY_mm_dd`?

## Value

The created file path, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`filename` should contain a file extension (i.e., a dot followed by
alphanumeric characters until the end of the file name). It should not
contain slashes or backslashes: use `dir` to indicate (sub)directories.
Non-alphanumeric characters other than dots and underscores preceding
the file extension are replaced by underscores, with a warning.

The default `dir` is a subdirectory with the current date in the
[format](https://rdrr.io/r/base/strptime.html) `YYYY_mm_dd` in directory
`output` below the working directory.
[`file.path()`](https://rdrr.io/r/base/file.path.html) ensures the
correct ([platform](https://rdrr.io/r/base/Platform.html)-dependent)
file separator is used to indicate subdirectories, and `"."` indicates
the [working directory](https://rdrr.io/r/base/getwd.html).

`dir` should point to a [valid
path](https://jessealderliesten.github.io/progutils/reference/is_path.md).
The directory for the returned path is
[created](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
if it does not yet exist.

The absolute [normalised](https://rdrr.io/r/base/normalizePath.html)
path is returned such that the returned path still works if the [working
directory](https://rdrr.io/r/base/getwd.html) changes. `"/"` instead of
`"\\"` is used as argument
[winslash](https://rdrr.io/r/base/normalizePath.html) such that the
returned path can be used in Windows' file system.

A warning is issued if the **file** indicated by the returned path
already exists. To prevent this when creating files in quick succession,
use `"%OSn"` as part of `format_stamp` to create precise stamps by
truncating seconds to `0 <= n <= 6` decimal places, see
[`strftime()`](https://rdrr.io/r/base/strptime.html) for details.

## Side effects

The directory indicated by the returned file path is created if it does
not yet exist.

## See also

[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)
to check if a file exists and is a unique match to a pattern,
[`file.path()`](https://rdrr.io/r/base/file.path.html) to construct file
paths in a platform-independent way,
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) to create
absolute normalised paths,
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a directory if it does not yet exist

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

## Examples

``` r
# Use a temporary directory to not write in the user's directory
my_tempdir <- normalizePath(path = file.path(tempdir(), "subdir"),
                            winslash = "/", mustWork = FALSE)

(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = my_tempdir, add_date = TRUE))
#> [1] "/tmp/RtmpfMHouA/subdir/2026_05_25/abc.txt"
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = TRUE))
#> [1] "/tmp/RtmpfMHouA/subdir/2026_05_25/25_05_2026_abc.txt"
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = my_tempdir, add_date = FALSE))
#> [1] "/tmp/RtmpfMHouA/subdir/def.html"
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = FALSE))
#> [1] "/tmp/RtmpfMHouA/subdir/25_05_2026_def.html"
(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#> [1] "/tmp/RtmpfMHouA/subdir/subdir/2026_05_25/abc.txt"
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#> [1] "/tmp/RtmpfMHouA/subdir/subdir/2026_05_25/25_05_2026_abc.txt"
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#> [1] "/tmp/RtmpfMHouA/subdir/subdir/def.html"
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#> [1] "/tmp/RtmpfMHouA/subdir/subdir/25_05_2026_def.html"

# Cleaning up
unlink(x = my_tempdir, recursive = TRUE)
rm(my_tempdir)
```
