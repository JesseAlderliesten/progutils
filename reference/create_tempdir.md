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
  with the name of the temporary subdirectory to be created inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). `subdir` should
  be a valid
  [path](https://jessealderliesten.github.io/progutils/reference/is_path.md)
  pointing to a directory that **not yet** exists, see `Details`.

## Value

The [normalized](https://rdrr.io/r/base/normalizePath.html) path to the
created temporary directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`subdir` is created inside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and an error is
thrown if it already exists. This ensures that programmatically
[removing](https://rdrr.io/r/base/unlink.html) the created directory
later on does not remove files that are still needed by other processes
(which could happen when removing the directory returned by
[`tempdir()`](https://rdrr.io/r/base/tempfile.html), for example because
`RStudio` also uses that directory).

It is possible to create subdirectories inside a not-yet existing
directory (e.g., to create `<tempdir>/output/outputsub` if
`<tempdir>/output` does not yet exist.

## Side effects

The requested temporary directory is created if does not yet exist. An
error is thrown if the directory already exists or creating the
directory failed.

## See also

[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md)
to create (non-temporary) directories and the notes in its test-file
`progutils\inst\tinytest\test_create_dir.R`.

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

## Examples

``` r
tempdir()
#> [1] "/tmp/RtmpVewme3"
# Create a directory inside the directory returned by 'tempdir()'
(tempdir_std <- create_tempdir(subdir = "examplesubtempdir"))
#> [1] "/tmp/RtmpVewme3/examplesubtempdir"

# Error if the directory already exists
try(create_tempdir(subdir = "examplesubtempdir"))
#> Error in create_tempdir(subdir = "examplesubtempdir") : 
#>   Temporary directory already exists: change 'subdir' ('examplesubtempdir'): /tmp/RtmpVewme3/examplesubtempdir

# It is possible to create recursive directories
(tempdir_recursive <- create_tempdir(subdir = file.path("abc", "def")))
#> [1] "/tmp/RtmpVewme3/abc/def"

# Clean up
unlink(c(tempdir_std, dirname(tempdir_recursive)), recursive = TRUE)
rm(tempdir_recursive, tempdir_std)
```
