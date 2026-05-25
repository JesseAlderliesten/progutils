# Create a temporary directory

Create a temporary directory that can safely be removed.

## Usage

``` r
create_tempdir(subdir = "subdir")
```

## Arguments

- subdir:

  A [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the name of a **not-yet** existing temporary subdirectory to be
  created inside [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
  `subdir` should meet the requirements for a valid
  [path](https://jessealderliesten.github.io/progutils/reference/is_path.md).

## Value

The [normalized](https://rdrr.io/r/base/normalizePath.html) path to the
created temporary directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`subdir` is created inside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and an error is
thrown if it already exists. This ensures that
[removing](https://rdrr.io/r/base/unlink.html) the created directory
does not remove files that are still needed by other processes (see
`Usage` below).

It is possible to create subdirectories inside a not-yet existing
directory (e.g., to create `<tempdir>/output/outputsub` if
`<tempdir>/output` does not yet exist.

## Side effects

The requested temporary directory is created if does not yet exist. An
error is thrown if the directory already exists or creating the
directory fails.

## Usage in practice

Examples and tests should **not** write to the [working
directory](https://rdrr.io/r/base/getwd.html) but to a temporary
directory that is cleaned up afterwards. Although
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) points to a
temporary directory, that directory should **not** be removed because
[RStudio](https://posit.co/products/open-source/rstudio) also uses it.
Instead, store the paths to temporary files written to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and
[unlink](https://rdrr.io/r/base/unlink.html) those paths when cleaning
up, or create a temporary subdirectory in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) which can be
completely be removed when cleaning up. `use_tempdir()` follows the
latter approach.

## See also

[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create (non-temporary) directories.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

## Examples

``` r
tempdir()
#> [1] "/tmp/RtmpiyEJAY"
# Create a directory inside the directory returned by 'tempdir()'
(tempdir_std <- create_tempdir(subdir = "examplesubtempdir"))
#> [1] "/tmp/RtmpiyEJAY/examplesubtempdir"

# Error if the directory already exists
try(create_tempdir(subdir = "examplesubtempdir"))
#> Error in create_tempdir(subdir = "examplesubtempdir") : 
#>   Temporary directory already exists: change 'subdir' ('examplesubtempdir'):
#> /tmp/RtmpiyEJAY/examplesubtempdir

# It is possible to create recursive directories
(tempdir_recursive <- create_tempdir(subdir = file.path("abc", "def")))
#> [1] "/tmp/RtmpiyEJAY/abc/def"

# Clean up
unlink(c(tempdir_std, dirname(tempdir_recursive)), recursive = TRUE)
rm(tempdir_recursive, tempdir_std)
```
