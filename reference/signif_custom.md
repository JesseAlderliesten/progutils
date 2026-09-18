# Round numbers

Round numbers to a specified minimum number of significant digits while
ensuring that the integer part is not rounded.

## Usage

``` r
signif_custom(x, digits = 3L, type = c("selective", "expanded"))
```

## Arguments

- x:

  [numeric
  vector](https://jessealderliesten.github.io/checkinput/reference/is_number.html),
  [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html), see `Details`.

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

`x` with rounded values, see `Details`.

## Details

[Rounding](https://rdrr.io/r/base/Round.html) numbers to a specified
number of significant digits can lead to values ending in rounded zeros,
e.g., `"14300"` instead of `"14286"` when rounding `(1e5)/7` to 3
significant digits. To prevent this, `signif_custom()` treats the value
in argument `digits` as a **minimum** value and rounds to more
significant digits if that is needed to not round the integer part of
numbers.

If `type` is `"selective"`, the required value of `digits` is determined
for each value individually, such that the number of significant digits
might differ if `x` has length larger than one. If type is `"expanded"`,
the value of `digits` is increased to the same value for all numbers in
a vector or column.

[Infinite values](https://rdrr.io/r/base/is.finite.html) are is returned
unchanged. `NA_integer_` and `NA_real_` are both returned as
[NA_real\_](https://rdrr.io/r/base/NA.html). `signif_custom()` also
handles [matrices](https://rdrr.io/r/base/matrix.html) and
[dataframes](https://rdrr.io/r/base/data.frame.html), rounding the
numeric columns with rounding of type `expanded` type on a per-column
basis. It does **not** handle
[factors](https://rdrr.io/r/base/factor.html), use
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md)
on `x` or use
`round_levels(x = x, level_order = levels(x), digits = digits, type = type)`.

## See also

[`round()`](https://rdrr.io/r/base/Round.html) to round to a specified
number of decimal places;
[`signif()`](https://rdrr.io/r/base/Round.html) to round to a specified
number of significant digits;
[`zapsmall()`](https://rdrr.io/r/base/zapsmall.html) to put small values
to zero; [`formatC()`](https://rdrr.io/r/base/formatc.html) for other
ways to format numbers;
[`round_levels()`](https://jessealderliesten.github.io/progutils/reference/round_levels.md)
to round [factor](https://rdrr.io/r/base/factor.html) levels.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_nonalnum()`](https://jessealderliesten.github.io/progutils/reference/replace_nonalnum.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`round_levels()`](https://jessealderliesten.github.io/progutils/reference/round_levels.md)

## Examples

``` r
x1 <- (1e5)/7
signif(x = x1, digits = 3) # returns 14300
#> [1] 14300
signif_custom(x1, digits = 3) # returns 14286 to not round the integer part
#> [1] 14286

x2 <- c(10^c(-1, 5)/7)
signif(x = x2, digits = 3) # returns c(1.43e-02, 1.43e+04)
#> [1] 1.43e-02 1.43e+04
# Increase the number of digits only for the
# second number, returning c(0.0143, 14286.0000)
signif_custom(x2, digits = 3, type = "selective")
#> [1]     0.0143 14286.0000
# Increase the number of digits for both
# numbers, returning c(0.014286, 14286.0000)
signif_custom(x2, digits = 3, type = "expanded")
#> [1] 1.4286e-02 1.4286e+04

x4 <- c(-0.1, 1, -1e4, 1e5) / 7
signif(x = x4, digits = 3)
#> [1] -1.43e-02  1.43e-01 -1.43e+03  1.43e+04
# returns c(-0.0143,   0.143,   -1430,   14300)
signif_custom(x = x4, digits = 3, type = "selective")
#> [1]    -0.0143     0.1430 -1429.0000 14286.0000
# returns c(-0.0143,   0.1430,  -1429.0, 14286)
signif_custom(x = x4, digits = 3, type = "expanded")
#> [1] -1.4286e-02  1.4286e-01 -1.4286e+03  1.4286e+04
# returns c(-0.014286, 0.14286, -1428.6, 14286)

x_df <- data.frame(a = 1:3, b = letters[11:13], c = c(1e5, 1e3, 1)/7, d = pi)
signif_custom(x_df, type = "selective")
#>   a b         c    d
#> 1 1 k 14286.000 3.14
#> 2 2 l   143.000 3.14
#> 3 3 m     0.143 3.14
signif_custom(x_df, type = "expanded")
#>   a b          c    d
#> 1 1 k 1.4286e+04 3.14
#> 2 2 l 1.4286e+02 3.14
#> 3 3 m 1.4286e-01 3.14
```
