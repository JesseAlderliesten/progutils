# Convert x to numeric

Convert `x` to a vector with
[numeric](https://rdrr.io/r/base/numeric.html)
[mode](https://rdrr.io/r/base/mode.html) in a way that works if `x` is a
[factor](https://rdrr.io/r/base/factor.html), a character vector, or
already a numeric vector.

## Usage

``` r
as.numeric_safe(x, keep_integer = TRUE)
```

## Arguments

- x:

  A factor, or a character or numeric vector to be converted to a
  numeric vector.

- keep_integer:

  `TRUE` or `FALSE`: return input of
  [type](https://rdrr.io/r/base/typeof.html) integer without converting
  it to double.

## Value

A numeric vector, possibly `numeric(0)` or containing `NA_real_`.

## Details

The [type](https://rdrr.io/r/base/typeof.html) of
[integers](https://rdrr.io/r/base/integer.html) is kept `integer` if
`keep_integer` is `TRUE` and is changed to `double` if `keep_integer` is
`FALSE`.

`NULL` and zero-length vectors are converted to `numeric(0)`. Logical
vectors of length larger than zero are converted to a vector of
`NA_real_`, with a warning.

`as.numeric_safe()` uses a suggestion from
[`factor()`](https://rdrr.io/r/base/factor.html) and [R FAQ
7.10](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f)
that works if `x` is a factor, a character vector or already is a
numeric vector (even though the last example of
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) comments that this
approach is less efficient for long vectors).

In contrast, the alternative conversions `as.numeric(levels(x))[x]`
(suggested in the `Warning` of
[`factor()`](https://rdrr.io/r/base/factor.html)) and
`as.numeric(levels(x))[as.integer(x)]` (suggested in R `FAQ 7.10` linked
to above) **only** work if `x` is factor, see the `Examples`.

Furthermore, [`as.numeric()`](https://rdrr.io/r/base/numeric.html)
**cannot** be used to convert a factor to numeric, because it returns
their underlying numeric integer representation (i.e., the indices of
the factor levels), see the `Warnings` in
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) and
[`factor()`](https://rdrr.io/r/base/factor.html).

## See also

[`formatC()`](https://rdrr.io/r/base/formatc.html) and
[option](https://rdrr.io/r/base/options.html) `scipen` on formatting
numbers,
[`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)

Other functions to convert types: `reexports`,
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

Other functions to modify character vectors: `reexports`,
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signal_text()`](https://jessealderliesten.github.io/progutils/reference/signal_text.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

Other functions to modify factors: `reexports`,
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
x_int <- 5:7
x_num <- as.numeric(x_int)
x_char <- as.character(x_int)
x_fact <- as.factor(x_int)

str(as.numeric_safe(x_int, keep_integer = TRUE))
#>  int [1:3] 5 6 7
str(as.numeric_safe(x_int, keep_integer = FALSE))
#>  num [1:3] 5 6 7

str_as_num_safe <- function(x) {str(as.numeric_safe(x))}
str_as_num_fact <- function(x) {str(as.numeric(levels(x))[x])}
str_as_num_7.10 <- function(x) {str(as.numeric(levels(x))[as.integer(x)])}
str_as_num_base <- function(x) {str(as.numeric(x))}

# as.numeric_safe() works irrespective the type of x
str_as_num_safe(x_int)  # correct
#>  int [1:3] 5 6 7
str_as_num_safe(x_num)  # correct
#>  num [1:3] 5 6 7
str_as_num_safe(x_char) # correct
#>  num [1:3] 5 6 7
str_as_num_safe(x_fact) # correct
#>  num [1:3] 5 6 7

# The 'more efficient' suggestions in `help(factor)` and
# R FAQ 7.10 *only* work for factors.
str_as_num_fact(x_int)  # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_7.10(x_int)  # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_fact(x_num)  # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_7.10(x_num)  # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_fact(x_char) # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_7.10(x_char) # clearly wrong
#>  num [1:3] NA NA NA
str_as_num_fact(x_fact) # correct
#>  num [1:3] 5 6 7
str_as_num_7.10(x_fact) # correct
#>  num [1:3] 5 6 7

# as.numeric() gives the *indices* of the factor levels
# for factors
str_as_num_base(x_int)  # correct
#>  num [1:3] 5 6 7
str_as_num_base(x_num)  # correct
#>  num [1:3] 5 6 7
str_as_num_base(x_char) # correct
#>  num [1:3] 5 6 7
str_as_num_base(x_fact) # wrong
#>  num [1:3] 1 2 3
```
