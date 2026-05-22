# Reorder factor levels

Reorder factor levels

## Usage

``` r
reorder_levels(x, new_order, warn_drop_order = TRUE)
```

## Arguments

- x:

  Factor with levels to be reordered to `new_order`, or character vector
  to be converted to a factor with levels ordered as `new_order`. Should
  have length larger than zero.

- new_order:

  Unique character vector with a length larger than zero indicating the
  new order of the factor levels.

- warn_drop_order:

  `TRUE` or `FALSE`: warn if values of `new_order` are dropped because
  they are not present in `x`?

## Value

`x` after converting it to a
[factor](https://rdrr.io/r/base/factor.html) with its levels reordered
to `new_order`, dropping values in `new_order` not present in `x`.

## Details

Character input to `x` is silently converted to a
[factor](https://rdrr.io/r/base/factor.html) before reordering the
factor levels.

Values of factor `x` that are not present in its levels are added to its
levels, with a warning.

Levels of factor `x` that are not present in its values are dropped,
with a warning.

Levels of `x` that are missing from `new_order` are appended to
`new_order`, with a warning.

Values in `new_order` that are missing from levels of `x` are dropped,
with a warning if `warn_drop_order` is `TRUE`.

## Notes

Reordering levels of factor `f` by replacing levels through code like
`levels(f) <- levels(f)[<some order>]` does *not* work, see the last
`Example`.

## Programming notes

Could also use an approach like
`levels(x) <- list("Yes" = c("Y", "Yes"), "No" = c("N", "No"))`, see
[`levels()`](https://rdrr.io/r/base/levels.html).

## See also

[reorder_cols](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md),
[`stats::relevel()`](https://rdrr.io/r/stats/relevel.html) to assign one
reference level to a factor

Other functions to modify factors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
orig <- factor(letters[c(12:13, 13:11)], levels = letters[13:11])
orig
#> [1] l m m l k
#> Levels: m l k
reorder_levels(x = orig, new_order = letters[11:13])
#> [1] l m m l k
#> Levels: k l m

# Changing the levels directly does *not* work because it changes the values
levels(orig) <- letters[11:13]
orig
#> [1] l k k l m
#> Levels: k l m
```
