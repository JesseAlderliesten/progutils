# Distinguish filenames and extensions

Distinguish filenames and extensions, correctly handling filenames that
end in a dot.

## Usage

``` r
file_path_no_ext(x, compression = FALSE)

file_path_ext(x, compression = FALSE)
```

## Arguments

- x:

  character vector giving file paths.

- compression:

  logical: should compression extension `.gz`, `.bz2` or `.xz` be
  removed first?

## Details

`file_path_no_ext()` returns the file paths without extensions and
leading dot; `file_path_ext()` returns the file extensions without
compression extension and leading dot. Only purely alphanumeric
extensions are recognized.

`file_path_no_ext()` replaces
[`tools::file_path_sans_ext()`](https://rdrr.io/r/tools/fileutils.html)
because prior to R 4.6.0 the latter did **not** recognise the extension
of file names ending in a dot, whereas
[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html) **did**
recognise such extensions. This discrepancy led to `filename` **not**
being re-created by
`paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))`
for filenames like `"ab..txt"`, instead producing the nonsense result
`"ab..txt.txt"`, see the `Examples`.

`file_path_ext()` replaces
[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html) because
the latter does not have argument `compression`.

## See also

Other functions to handle paths and directories:
[`create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.md),
[`create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.md),
[`create_tempdir()`](https://jessealderliesten.github.io/progutils/reference/create_tempdir.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`is_filename()`](https://jessealderliesten.github.io/progutils/reference/is_filename.md),
[`is_path()`](https://jessealderliesten.github.io/progutils/reference/is_path.md)

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
# The next line produced the nonsense-result "ab..txt.txt" prior to R 4.6.0.
paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#> [1] "ab..txt"

# The updated version recreates filename 'ab..txt':
paste0(file_path_no_ext(filename), ".", file_path_ext(filename))
#> [1] "ab..txt"
```
