# Reorder columns

Reorder columns

## Usage

``` r
reorder_cols(x, new_order)
```

## Arguments

- x:

  Object with at least one column. Columns should have unique,
  syntactically valid names, see
  [`checkinput::all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.html).

- new_order:

  Character vector with a length larger than zero containing unique
  names in the new order of the columns.

## Value

`x` with reordered columns.

## Details

Column names of `x` that are missing from `new_order` are appended to
`new_order`, with a warning.

Values in `new_order` that are missing from column names of `x` are
dropped, with a warning.

## See also

[`order()`](https://rdrr.io/r/base/order.html),
[reorder_levels](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`sort()`](https://rdrr.io/r/base/sort.html)

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

## Examples

``` r
test_df <- data.frame(a = 1:2, b = 11:12, c = 21:22)
reorder_cols(x = test_df, new_order = c("b", "a", "c"))
#>    b a  c
#> 1 11 1 21
#> 2 12 2 22

# reorder_cols() appends column names that are missing from 'new_order' and
# drops values from 'new_order' that are missing from column names, both with
# a warning.
reorder_cols(x = test_df, new_order = c("b", "a", "d"))
#> Warning: Appended columns that are present in 'x' but missing from 'new_order':
#> 'c'
#> Warning: Dropped values of 'new_order' that are not present in column names of 'x':
#> 'd'
#>    b a  c
#> 1 11 1 21
#> 2 12 2 22
```
