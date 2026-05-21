# Create a directory

Create a directory if it does not yet exist.

## Usage

``` r
create_dir(dir = file.path(".", "output"), add_date = TRUE)
```

## Arguments

- dir:

  Non-empty character string containing the path to a directory that
  should be created if it does not yet exist. A dot (i.e., ".")
  indicates the current working directory.

- add_date:

  `TRUE` or `FALSE`: create a subdirectory with the current date in the
  [format](https://rdrr.io/r/base/strptime.html) `YYYY_mm_dd`?

## Value

A character string with the absolute
[normalized](https://rdrr.io/r/base/normalizePath.html) path to the
requested directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html). The [working
directory](https://rdrr.io/r/base/getwd.html) is returned if an attempt
to create a directory fails, with a warning.

## Details

The default `dir` is a subdirectory with the current date in the
[format](https://rdrr.io/r/base/strptime.html) `YYYY_mm_dd` in directory
`output` below the working directory.
[`file.path()`](https://rdrr.io/r/base/file.path.html) ensures the
correct ([platform](https://rdrr.io/r/base/Platform.html)-dependent)
file separator is used to indicate subdirectories, and `"."` indicates
the [working directory](https://rdrr.io/r/base/getwd.html).

Several limitations are imposed on `dir`, see
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md).

If creating the directory fails, the working directory is returned
instead. This happens if `dir` points to an existing file instead of an
directory.

The absolute [normalised](https://rdrr.io/r/base/normalizePath.html)
path is returned such that the returned path still works if the [working
directory](https://rdrr.io/r/base/getwd.html) changes. On
case-insensitive file systems (e.g., Windows and macOS), normalization
adjusts the case to match case-insensitive names of directories that are
already present (see the `Examples`). `"/"` instead of `"\\"` is used as
[winslash](https://rdrr.io/r/base/normalizePath.html) during
normalisation, such that the returned path can be used in Windows' file
system.

## Side effects

The requested directory is created if does not yet exist.

## Programming notes

See also:

- [`utils::file_test()`](https://rdrr.io/r/utils/filetest.html)

- [`fs::is_dir()`](https://fs.r-lib.org/reference/is_file.html)

- [`fs::is_dir_empty()`](https://fs.r-lib.org/reference/is_dir_empty.html).

## See also

[`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md)
to create a path,
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
for a safe way to create temporary directories,
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)
and references there about file paths and directories,
[`dir.exists()`](https://rdrr.io/r/base/files2.html) and
[`dir.create()`](https://rdrr.io/r/base/files2.html) used by this
function,
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)
to check if a file exists and is a unique match to a pattern

Other functions to handle paths and directories:
[`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

## Examples

``` r
# Use a temporary subdirectory to not write in the user's directory
my_tempdir <- file.path(tempdir(), "testcreatedir")

# Create directory 'dir_one' inside this temporary directory
res_dir_one <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                          add_date = FALSE)
dir.exists(res_dir_one) # TRUE
#> [1] TRUE

# An attempt to create a directory that already exists does not change any
# directory and the same directory is returned.
res_dir_one_v2 <- create_dir(dir = file.path(my_tempdir, "dir_one"),
                             add_date = FALSE)
identical(res_dir_one, res_dir_one_v2) # TRUE
#> [1] TRUE

# On case-insensitive file systems such as Windows and macOS, adding
# 'dir_ONE' to the directory gives the same result as adding 'dir_one' as
# done above for 'res_dir_one'
res_dir_one_v3 <- create_dir(dir = file.path(my_tempdir, "dir_ONE"),
                             add_date = FALSE)
# TRUE on Windows and macOS, FALSE on Ubuntu
identical(res_dir_one, res_dir_one_v3)
#> [1] FALSE

# Create directory 'dir_two' with a subdirectory containing the current date
res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
                          add_date = TRUE)
dir.exists(res_dir_two) # TRUE
#> [1] TRUE

# Cleaning up
unlink(dirname(res_dir_one), recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two, res_dir_one_v3)
```
