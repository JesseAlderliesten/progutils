# Round levels

Round numeric values of factor levels, e.g., to create facet labels that
fit better.

## Usage

``` r
round_levels(
  x,
  level_order = NULL,
  digits = 4L,
  type = c("selective", "expanded")
)
```

## Arguments

- x:

  [factor](https://rdrr.io/r/base/factor.html) or
  [character](https://rdrr.io/r/base/character.html) vector containing
  [numerish
  values](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md)
  to be rounded, see `Details`.

- level_order:

  `NULL` to sort levels on increasing numerical value, or a a vector
  indicating the desired order of the factor
  [levels](https://rdrr.io/r/base/levels.html).

- digits:

  the **minimum** number of [significant
  digits](https://rdrr.io/r/base/Round.html) to round to (see `Details`)
  or `Inf` to not round values.

- type:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  `"selective"` or `"expanded"` to indicate the type of rounding to be
  used, see `Details`.

## Value

`x` with rounded factor levels.

## Details

Arguments `digits` and `type` are passed to
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md).

Levels of `x` that are not present in its values will be silently
dropped.

Values in `x` that do not occur in `level_order` will be added to
`level_order`, with a warning. Values in `level_order` that do not occur
in `x` will be dropped, with a warning.

## See also

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_nonalnum()`](https://jessealderliesten.github.io/progutils/reference/replace_nonalnum.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md)

Other functions to modify factors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
`reexports`,
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
round_levels(x = factor(c(2, 2.2, 2.8)), level_order = c("2.2", "2.8", "2"))
#> [1] 2   2.2 2.8
#> Levels: 2.2 2.8 2
round_levels(x = factor(c(2, 2.2, 2.8)), level_order = c("2.2", "2.8", "2.0"))
#> [1] 2   2.2 2.8
#> Levels: 2.2 2.8 2
round_levels(x = factor(c(2, 2.2, 2.8)), level_order = NULL)
#> [1] 2   2.2 2.8
#> Levels: 2 2.2 2.8
```
