# Create a new temporary directory

Create a new temporary directory that can safely be removed.

## Usage

``` r
create_tempdir(pattern = "tempdir")
```

## Arguments

- pattern:

  A [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the initial part of the name of the new temporary directory, only
  containing characters that are valid in a
  [path](https://jessealderliesten.github.io/checkinput/reference/is_path.html).

## Value

The [absolute normalized](https://fs.r-lib.org/reference/path_math.html)
path to the created temporary directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

The new directory is [created](https://rdrr.io/r/base/files2.html)
inside [`tempdir()`](https://rdrr.io/r/base/tempfile.html). Its
[name](https://rdrr.io/r/base/tempfile.html) starts with the string
given by `pattern` and is followed by a random string in hex. This
random string **might** contain a dot (`.`) such that the directory
might appear to have a file extension, see the section `Source` in
[`help("tempdir")`](https://rdrr.io/r/base/tempfile.html). An error is
thrown if creating the directory fails.

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
removed once they are not needed anymore, see the section
`Usage in practice` below.

## Side effects

The temporary directory indicated by the returned path is
[created](https://fs.r-lib.org/reference/create.html) inside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Usage in practice

`Examples` and `tests` should write to a temporary directory that is
cleaned up afterwards (otherwise R cmd check will issue a `Note` about
'[detritus in the temp
directory](https://contributor.r-project.org/cran-cookbook/code_issues.html#leaving-files-in-the-temporary-directory)'
and
[CRAN](https://cran.r-project.org/web/packages/policies.html#Source-packages)
will not accept your package). Although
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) points to a
temporary directory, that directory should **not** be removed because
other R processes and
[RStudio](https://posit.co/products/open-source/rstudio) might use it.
Instead, create a temporary subdirectory in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and afterwards clean
up by [removing](https://rdrr.io/r/base/unlink.html) that subdirectory:

    my_tempdir <- create_tempdir(pattern = "subtempdir")
    < do stuff >
    unlink(my_tempdir, recursive = TRUE)

It is good practice to create the complete temporary directory with all
required files and folders before running any test for the presence or
absence of particular files or folders: this ensures no spurious matches
occur if examples or tests are removed or added.

## Programming notes

The output of [`tempdir()`](https://rdrr.io/r/base/tempfile.html) during
[R CMD checks](https://r-pkgs.org/R-CMD-check.html) on MacOS contains
successive forward slashes (e.g.,
`/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`) which in earlier
versions of `is_path()` (then in package `progutils`) led to spurious
warnings about duplicated file separators.

## See also

[`tempfile()`](https://rdrr.io/r/base/tempfile.html) used in this
function to create the paths for the temporary directory;
[`local()`](https://rdrr.io/r/base/eval.html) and
[withr::local_tempdir()](https://withr.r-lib.org/reference/with_tempfile.html)
for automated deletion of temporary directories;
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create (non-temporary) directories;
[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
to check if a path is valid, with the `Note on paths` in its
documentation.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md)

## Examples

``` r
tempdir(check = TRUE)
#> [1] "/tmp/RtmpxR3dLJ"
# Create a directory inside the directory returned by 'tempdir()'
(my_subtempdir_ex1 <- create_tempdir(pattern = "subtempdir"))
#> [1] "/tmp/RtmpxR3dLJ/subtempdir1a716aa2989f"

# Using the same 'pattern' again creates another directory
(my_subtempdir_ex2 <- create_tempdir(pattern = "subtempdir"))
#> [1] "/tmp/RtmpxR3dLJ/subtempdir1a71633ad333"

# It is not possible to create recursive subdirectories
try(no_subtempdir <- create_tempdir(pattern = "subtempdir/otherdir"))
#> Error in create_tempdir(pattern = "subtempdir/otherdir") : 
#>   'pattern' should not include file separators
try(no_subtempdir <- create_tempdir(pattern = "subtempdir\\otherdir"))
#> Error in create_tempdir(pattern = "subtempdir\\otherdir") : 
#>   'pattern' should not include file separators

# Clean up
unlink(c(my_subtempdir_ex1, my_subtempdir_ex2), recursive = TRUE)
rm(my_subtempdir_ex1, my_subtempdir_ex2)
```
