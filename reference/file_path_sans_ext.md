# Remove extension from file paths

A drop-in replacement for tools::file_path_sans_ext, see `Details`.
Returns the file path without the extensions (and the leading dot). Only
purely alphanumeric extensions are recognized.

## Usage

``` r
file_path_sans_ext(x, compression = FALSE)
```

## Arguments

- x:

  character vector giving file paths.

- compression:

  logical: should compression extension `.gz`, `.bz2` or `.xz` be
  removed first?

## Details

[`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html)
did *not* recognise the extension of file names that end in a dot prior
to R 4.6.0, whereas
[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html) *did*
recognise such extensions.

Using `file_path_sans_ext()` from `progutils` ensures that `filename` is
recreated by
`paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename))`,
such that, e.g.,
[create_path](https://jessealderliesten.github.io/progutils/reference/create_path.md)`("ab..txt")`
produces the correct result ending in `"ab..txt"` instead of the
nonsense result ending in `"ab..txt.txt"`, see the `Examples`.

## See also

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_path()`](https://jessealderliesten.github.io/progutils/reference/create_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md)

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`reorder_cols()`](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

## Examples

``` r
filename <- "ab..txt"
# Should be "ab." but was "ab..txt" prior to R 4.6.0.
tools::file_path_sans_ext(filename)
#> [1] "ab."
tools::file_ext(filename) # "txt"
#> [1] "txt"
# So the next line produces the nonsense-result "ab..txt.txt"
paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#> [1] "ab..txt"

# The updated version recreates filename 'ab..txt':
paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#> [1] "ab..txt"
```
