# Create a file path

Create a file path, creating the indicated directory if it does not yet
exist.

## Usage

``` r
create_file_path(
  filename,
  format_stamp = "%Y_%m_%d_%H_%M_%S",
  dir = fs::path_wd("output"),
  add_date = TRUE
)
```

## Arguments

- filename:

  A character string with the file name, including the file extension
  like `.csv` or `.txt`. Should adhere to the restrictions described in
  [`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html).

- format_stamp:

  A character string indicating the
  [format](https://rdrr.io/r/base/strptime.html) of the stamp to be
  added in front of the file name. No stamp is added if `format_stamp`
  is an empty string (i.e., `""`). The formatted stamp is treated as
  part of the filename, such that the same restrictions apply, see
  `Details`.

- dir:

  [Character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  containing a [valid
  path](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
  to a directory that should be created if it does not yet exist.
  [`fs::path()`](https://fs.r-lib.org/reference/path.html) adds file
  separators and the dot (`"."`) indicates the [working
  directory](https://rdrr.io/r/base/getwd.html), such that by default a
  subdirectory with the current date in the
  [format](https://rdrr.io/r/base/strptime.html) `YYYY_mm_dd` in
  directory `output` below the working directory is created.

- add_date:

  `TRUE` or `FALSE`: create a subdirectory in `dir` with the current
  date in the [format](https://rdrr.io/r/base/strptime.html)
  `YYYY_mm_dd`?

## Value

The created file path, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`filename` **should** contain a file extension (i.e., a dot followed by
any character until the end of the file name) and should **not** contain
slashes or backslashes: use `dir` to indicate subdirectories.

The [absolute normalised](https://fs.r-lib.org/reference/path_math.html)
path is returned such that the returned path still works if the [working
directory](https://rdrr.io/r/base/getwd.html) changes.

A warning is issued if the **file** indicated by the returned path
already exists. To prevent this when creating files in quick succession,
use `"%OSn"` as part of `format_stamp` to create precise stamps by
truncating seconds to `0 <= n <= 6` decimal places, see
[`strftime()`](https://rdrr.io/r/base/strptime.html) for details.

## Side effects

The directory indicated by the returned file path is
[created](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
if it does not yet exist.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, and the 'Note on paths' in its
documentation;
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)
to check if a file exists and is a unique match to a pattern,
[`fs::path()`](https://fs.r-lib.org/reference/path.html) to construct
file paths in a platform-independent way,
[`fs::path_abs()`](https://fs.r-lib.org/reference/path_math.html) to
create absolute normalised paths,
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a directory if it does not yet exist

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
# Use a temporary directory to not write in the user's directory
my_tempdir <- fs::path_abs(path = fs::path(tempdir(), "subdir"))

(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = my_tempdir, add_date = TRUE))
#> /tmp/Rtmph9T1TP/subdir/2026_06_03/abc.txt
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = TRUE))
#> /tmp/Rtmph9T1TP/subdir/2026_06_03/03_06_2026_abc.txt
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = my_tempdir, add_date = FALSE))
#> /tmp/Rtmph9T1TP/subdir/def.html
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = my_tempdir, add_date = FALSE))
#> /tmp/Rtmph9T1TP/subdir/03_06_2026_def.html
(create_file_path(filename = "abc.txt", format_stamp = "",
                  dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#> /tmp/Rtmph9T1TP/subdir/subdir/2026_06_03/abc.txt
(create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
                  dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#> /tmp/Rtmph9T1TP/subdir/subdir/2026_06_03/03_06_2026_abc.txt
(create_file_path(filename = "def.html", format_stamp = "",
                  dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#> /tmp/Rtmph9T1TP/subdir/subdir/def.html
(create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
                  dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#> /tmp/Rtmph9T1TP/subdir/subdir/03_06_2026_def.html

# Cleaning up
unlink(x = my_tempdir, recursive = TRUE)
rm(my_tempdir)
```
