# Are numeric values nearly equal

Test element-wise near-equality of numeric vectors by allowing for small
numeric errors to make `are_equal()` safer than
[==](https://rdrr.io/r/base/Comparison.html).

## Usage

``` r
are_equal(x, y, tol = sqrt(.Machine$double.eps))
```

## Arguments

- x, y:

  Numeric vectors to compare for equality.

- tol:

  A small
  [positive](https://jessealderliesten.github.io/checkinput/reference/is_number.html)
  number. Numbers that differ less than `tol` are considered to be
  equal.

## Value

A vector with logical values (`TRUE`, `FALSE` or `NA`) indicating if
elements in `x` and `y` are equal to each other.
[NA](https://rdrr.io/r/base/NA.html) is returned for comparisons
involving numeric `NA`s (i.e., `NA_integer_` and `NA_real_`),
[NaN](https://rdrr.io/r/base/is.finite.html)s, or [infinite
values](https://rdrr.io/r/base/is.finite.html) with the same sign.

## Acknowledgement

Code `abs(x - y) < tol` was taken from `dplyr::near()`.

## See also

[`checkinput::is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.html)
to check for element-wise near-equality to natural numbers;
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) to check more
generally for near-equality;
[`identical()`](https://rdrr.io/r/base/identical.html) to check for
exact equality and [Comparison](https://rdrr.io/r/base/Comparison.html)
to do so using binary operators;
[`match()`](https://rdrr.io/r/base/match.html) and
[`progutils::not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md)
to compare character vectors; [R FAQ
7.31](https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
for background on numerical equality; the vignette *Type coercion in
vectors* in package `checkinput`:
`vignette("Type_Coercion", package = "checkinput")`.

Other functions to check equality:
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
x <- sqrt(2)^2
x == 2 # FALSE
#> [1] FALSE
x - 2 # about 4.44e-16
#> [1] 4.440892e-16
are_equal(x = x, y = 2) # TRUE
#> [1] TRUE

are_equal(x = c(2, 3, 3,        NA, Inf),
          y = c(2, 3, 3 + 1e-8, NA, Inf))
#> [1] TRUE TRUE TRUE   NA   NA
are_equal(x = 3, y = c(2, 3, 3 + 1e-8, NA, Inf))
#> [1] FALSE  TRUE  TRUE    NA FALSE
```
