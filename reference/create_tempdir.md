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
  with the subdirectory to create inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). This subdirectory
  should not yet exist, see `Details`.

## Value

The [normalized](https://rdrr.io/r/base/normalizePath.html) path to the
created temporary directory, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

The temporary directory is created as subdirectory `subdir` inside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and an error is
thrown if it already exists. This ensures that programmatically
[removing](https://rdrr.io/r/base/unlink.html) the created directory
later on does not remove files that are still needed by other processes
(which would happen when removing the directory returned by
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) because `RStudio`
also uses that directory).

It is possible to create subdirectories inside a not-yet existing
directory (e.g., to create `<tempdir>/output/<date>` if
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
[`file_path_sans_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_sans_ext.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)

## Examples

``` r
tempdir()
#> [1] "/tmp/RtmpU9aiax"
# Create a directory inside the directory returned by 'tempdir()'
(create_tempdir(subdir = "subdir"))
#> [1] "/tmp/RtmpU9aiax/subdir"

# Error if the directory already exists
try(create_tempdir(subdir = "subdir"))
#> Error in create_tempdir(subdir = "subdir") : 
#>   You need to change 'subdir' ('subdir'): temporary directory already exists: /tmp/RtmpU9aiax/subdir

# It is possible to create recursive directories
(create_tempdir(subdir = "abc/def"))
#> [1] "/tmp/RtmpU9aiax/abc/def"
```
