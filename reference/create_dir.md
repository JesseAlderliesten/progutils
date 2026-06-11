# Create a directory

Create a directory if it does not yet exist.

## Usage

``` r
create_dir(dir = fs::path(".", "output"), add_date = TRUE)
```

## Arguments

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

A character string with the [absolute
normalized](https://fs.r-lib.org/reference/path_math.html) path to the
requested directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html). An error is thrown
if the attempt to create a directory fails. This happens if `dir` points
to an existing file instead of an directory.

## Details

The [absolute normalised](https://fs.r-lib.org/reference/path_math.html)
path is returned such that the returned path still works if the [working
directory](https://rdrr.io/r/base/getwd.html) changes. On
case-insensitive file systems (e.g., Windows and macOS), normalization
adjusts the case to match case-insensitive names of directories that are
already present (see the `Examples`).

## Side effects

The directory indicated by the returned path is created if it does not
yet exist.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, and the 'Note on paths' in its
documentation;
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
to create a file path and creating the indicated directory if it does
not yet exist;
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
for a safe way to create temporary directories;
[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
and references there about file paths and directories;
[`fs::dir_exists()`](https://fs.r-lib.org/reference/file_access.html)
and [`fs::dir_create()`](https://fs.r-lib.org/reference/create.html)
used by this function (and the base-equivalent
[`dir.create()`](https://rdrr.io/r/base/files2.html) of the latter);
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)
to check if a file exists and is a unique match to a pattern.

Other functions to handle paths and directories:
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
# Use a temporary subdirectory to not write in the user's directory
my_tempdir <- create_tempdir(pattern = "examplecreatedir")

# Create directory 'dir_one' inside this temporary directory
res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                          add_date = FALSE)
fs::dir_exists(res_dir_one) # TRUE
#> /tmp/RtmptJZ0Kd/examplecreatedir1a1ed9c556d/dir_one 
#>                                                TRUE 

# An attempt to create a directory that already exists does not change any
# directory and the same directory is returned.
res_dir_one_v2 <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                             add_date = FALSE)
identical(res_dir_one, res_dir_one_v2) # TRUE
#> [1] TRUE

# On case-insensitive file systems, the directory 'res_dir_ONE' is the same as
# 'res_dir_one'. On case-sensitive file systems it differs in case from
# 'res_dir_one'.
res_dir_ONE <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
                          add_date = FALSE)
identical(res_dir_one, res_dir_ONE)
#> [1] FALSE

# Create directory 'dir_two' with a subdirectory containing the current date
res_dir_date <- create_dir(dir = fs::path(my_tempdir, "dir_date"),
                           add_date = TRUE)
fs::dir_exists(res_dir_date) # TRUE
#> /tmp/RtmptJZ0Kd/examplecreatedir1a1ed9c556d/dir_date/2026_06_11 
#>                                                            TRUE 

# Cleaning up
unlink(my_tempdir, recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_date, res_dir_ONE)
```
