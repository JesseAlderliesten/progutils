# Create a Temporary Directory

Create a temporary directory that can safely be removed.

## Usage

``` r
create_tempdir(prefix = "tempdir")
```

## Arguments

- prefix:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the initial part of the name of the new temporary directory, only
  containing characters that are [valid in a
  path](https://jessealderliesten.github.io/checkinput/reference/is_path.html).

## Value

A character string with the [absolute
normalised](https://fs.r-lib.org/reference/path_math.html) path to the
created temporary directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html), see the section
`Notes on paths` below.

An error is thrown when attempting to create subdirectories (see
`Details`) and if creating the directory fails.

## Details

The new directory is [created](https://rdrr.io/r/base/files2.html)
inside the directory returned by
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). Its
[name](https://rdrr.io/r/base/tempfile.html) starts with the string
given by `prefix` and is followed by a random string. This random string
**might** contain a dot (`.`) such that the directory appears to have a
file extension, see the section `Source` in
[`help("tempdir")`](https://rdrr.io/r/base/tempfile.html).

It is **not** possible to create recursive subdirectories (e.g.,
`tempdir/subtempdir/otherdir`, see the `Examples`) because
[removing](https://rdrr.io/r/base/unlink.html) such directories might
remove files that are still needed by other processes.

Although the temporary directory given by
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) will normally be
automatically [removed](https://rdrr.io/r/base/unlink.html) when R
[quits](https://rdrr.io/r/base/quit.html) (and operating systems might
[periodically](https://cran.r-project.org/doc/manuals/R-admin.html#Running-R)
empty the temporary directory), the created subdirectories should be
[removed](https://rdrr.io/r/base/unlink.html) once they are not needed
anymore, see the section `Usage in practice` below.

## Usage in Practice

[Examples](https://rdrr.io/r/utils/example.html) and
[tests](https://r-pkgs.org/testing-basics.html) should write to a
temporary directory that is cleaned up afterwards (otherwise
[`R cmd check`](https://r-pkgs.org/R-CMD-check.html) will issue a `Note`
about
[`detritus in the temp directory`](https://contributor.r-project.org/cran-cookbook/code_issues.html#leaving-files-in-the-temporary-directory)
and CRAN will not accept your package, see section `Source packages`
from the [CRAN
policies](https://cran.r-project.org/web/packages/policies.html)).
Although [`tempdir()`](https://rdrr.io/r/base/tempfile.html) points to a
temporary directory, that directory should **not** be removed because
other processes in R and
[RStudio](https://posit.co/products/open-source/rstudio) also use it.
Instead, create a temporary subdirectory in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and afterwards clean
up by [removing](https://rdrr.io/r/base/unlink.html) that subdirectory:

    my_tempdir <- create_tempdir(prefix = "subtempdir")
    < do stuff >
    unlink(my_tempdir, recursive = TRUE)

It is good practice to create the complete temporary directory with all
required files and folders before running any test for the presence or
absence of particular files or folders: this ensures no spurious matches
occur if examples or tests are removed or added.

## Side Effects

The directory indicated by the returned path is
[created](https://fs.r-lib.org/reference/create.html) if it does not yet
[exist](https://fs.r-lib.org/reference/file_access.html).

## Notes on Paths

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

## Programming Notes

The output of [`tempdir()`](https://rdrr.io/r/base/tempfile.html) during
[`R CMD checks`](https://r-pkgs.org/R-CMD-check.html) on MacOS contains
successive forward slashes (e.g.,
`/var/<...>/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`) which in earlier
versions of
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
(then in package
[progutils](https://jessealderliesten.github.io/progutils/)) led to
spurious warnings about duplicated file separators.

## See also

[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, with extensive references about file paths
and directories;
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create a (non-temporary) directory if it does not yet exist;
[`local()`](https://rdrr.io/r/base/eval.html) and
[withr::local_tempdir()](https://withr.r-lib.org/reference/with_tempfile.html)
for automated deletion of temporary directories;
[withr::local_dir()](https://withr.r-lib.org/reference/with_dir.html)
and
[usethis::local_project()](https://usethis.r-lib.org/reference/proj_utils.html)
to change the working directory to a temporary directory (use
[`withr::deferred_run()`](https://withr.r-lib.org/reference/defer.html)
to change back to the normal project in interactive use);
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) used in this
function to create the paths for the temporary directory

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
tempdir(check = TRUE)
#> [1] "/tmp/Rtmpo1KNmc"
# Create a directory inside the directory returned by 'tempdir()'
(tempdir_ex1 <- create_tempdir(prefix = "subtempdir"))
#> [1] "/tmp/Rtmpo1KNmc/subtempdir1aaa80141c7"

# Using the same 'prefix' again creates another directory
(tempdir_ex2 <- create_tempdir(prefix = "subtempdir"))
#> [1] "/tmp/Rtmpo1KNmc/subtempdir1aaa1ae0962d"

# It is not possible to create recursive subdirectories
try(no_subtempdir <- create_tempdir(
    prefix = "subtempdir/otherdir"))
#> Error in create_tempdir(prefix = "subtempdir/otherdir") : 
#>   'prefix' should not include file separators
try(no_subtempdir <- create_tempdir(
    prefix = "subtempdir\\otherdir"))
#> Error in create_tempdir(prefix = "subtempdir\\otherdir") : 
#>   'prefix' should not include file separators

# Clean up
unlink(c(tempdir_ex1, tempdir_ex2), recursive = TRUE)
rm(tempdir_ex1, tempdir_ex2)
```
