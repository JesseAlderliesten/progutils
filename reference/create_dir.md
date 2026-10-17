# Create a directory

Create a directory if it does not yet exist.

## Usage

``` r
create_dir(dir = fs::path(".", "output"), add_date = TRUE)
```

## Arguments

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

A character string with the [absolute
normalised](https://fs.r-lib.org/reference/path_math.html) path to the
requested directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html), see the section
`Notes on paths` and the `Examples` below.

By default a subdirectory with the current date in the
[format](https://rdrr.io/r/base/strptime.html) `YYYY_mm_dd` is created
in directory `output` below the [working
directory](https://rdrr.io/r/base/getwd.html).

An error is thrown if the attempt to create a directory fails, for
example because `dir` points to an existing file instead of to an
directory.

## Details

The path in `dir` is interpreted as a **relative** path to the directory
that should be created, starting from the [working
directory](https://rdrr.io/r/base/getwd.html), see the section
`Notes on paths` below.

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
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md)
for a safe way to create temporary directories;
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md)
to create a file path and creating the indicated directory if it does
not yet exist;
[`fs::dir_exists()`](https://fs.r-lib.org/reference/file_access.html)
and [`fs::dir_create()`](https://fs.r-lib.org/reference/create.html)
(with its base-equivalent
[`dir.create()`](https://rdrr.io/r/base/files2.html)) used by this
function.

Other functions to handle paths and directories:
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
# Use a temporary subdirectory to not write in the user's directory
my_tempdir <- create_tempdir(prefix = "examplecreatedir")

# Create directory 'dir_one' inside this temporary directory
res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                          add_date = FALSE)
fs::dir_exists(res_dir_one) # TRUE
#> /tmp/RtmpErmCfJ/examplecreatedir187d6ac390de/dir_one 
#>                                                 TRUE 

# An attempt to create a directory that already exists does not change any
# directory and the same directory is returned.
res_dir_one_v2 <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
                             add_date = FALSE)
identical(res_dir_one, res_dir_one_v2) # TRUE
#> [1] TRUE

# On case-insensitive file systems, the directories 'res_dir_ONE' and
# 'res_dir_one' are the same directory. On case-sensitive file systems they
# are different directories.
res_dir_ONE <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
                          add_date = FALSE)
identical(res_dir_one, res_dir_ONE)
#> [1] FALSE

# Create directory 'dir_two' with a subdirectory containing the current date
res_dir_date <- create_dir(dir = fs::path(my_tempdir, "dir_date"),
                           add_date = TRUE)
fs::dir_exists(res_dir_date) # TRUE
#> /tmp/RtmpErmCfJ/examplecreatedir187d6ac390de/dir_date/2026_10_17 
#>                                                             TRUE 

# Cleaning up
unlink(my_tempdir, recursive = TRUE)
rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_date, res_dir_ONE)
```
