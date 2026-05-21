# Convert x to numeric

Convert `x` to a vector with
[numeric](https://rdrr.io/r/base/numeric.html)
[mode](https://rdrr.io/r/base/mode.html) in a way that works
irrespective if `x` is a [factor](https://rdrr.io/r/base/factor.html), a
character vector, or already a numeric vector.

## Usage

``` r
as.numeric_safe(x)
```

## Arguments

- x:

  A factor, or a character or numeric vector to be converted to a
  numeric vector.

## Value

A numeric vector, possibly `numeric(0)` or containing `NA_real_`.

## Details

`as.numeric_safe()` uses a suggestion from
[`factor()`](https://rdrr.io/r/base/factor.html) and [R FAQ
7.10](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f)
that not only works if `x` is a factor but also if `x` is a character
vector or already is a numeric vector (even though though the last
example of `as.numeric` comments that this approach is less efficient
for long vectors).

In contrast, the alternative conversions `as.numeric(levels(x))[x]`
(suggested in the `Warning` of
[`factor()`](https://rdrr.io/r/base/factor.html)) and
`as.numeric(levels(x))[as.integer(x)]` (suggested in R `FAQ 7.10` linked
to above) *only* work if `x` is factor, see the `Examples`.

Furthermore, [`as.numeric()`](https://rdrr.io/r/base/numeric.html)
*cannot* be used to convert a factor to numeric, because it returns
their underlying numeric integer representation (i.e., the indices of
the factor levels), see the `Warnings` in
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) and
[`factor()`](https://rdrr.io/r/base/factor.html).

`as.numeric_safe()` changes the
[type](https://rdrr.io/r/base/typeof.html) of
[integers](https://rdrr.io/r/base/integer.html) from `integer` to
`double`, even though integers already have
[mode](https://rdrr.io/r/base/mode.html)
[numeric](https://rdrr.io/r/base/numeric.html).

## Notes

`NULL` and zero-length vectors are converted to `numeric(0)`. Logical
vectors of length larger than zero are converted to a vector of
`NA_real_`, with a warning.

## See also

[`formatC()`](https://rdrr.io/r/base/formatc.html) and
[option](https://rdrr.io/r/base/options.html) `scipen` on formatting
numbers,
[`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)

Other functions to modify character vectors:
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`reorder_cols()`](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

Other functions to modify factors:
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
x_int <- 5:7
x_num <- as.numeric(x_int)
x_char <- as.character(x_int)
x_fact <- as.factor(x_int)

str_as_num_safe <- function(x) {str(as.numeric_safe(x))}
str_as_num_fact <- function(x) {str(as.numeric(levels(x))[x])}
str_as_num_7.10 <- function(x) {str(as.numeric(levels(x))[as.integer(x)])}
str_as_num_base <- function(x) {str(as.numeric(x))}

# as.numeric_safe() works irrespective the type of x
str_as_num_safe(x_int)  # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_safe(x_num)  # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_safe(x_char) # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_safe(x_fact) # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7

# The 'more efficient' suggestion in help(factor) *only* works for factors.
str_as_num_fact(x_int)  # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_fact(x_num)  # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_fact(x_char) # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_fact(x_fact) # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7

# The 'more efficient' suggestion in R FAQ 7.10 *only* works for factors.
str_as_num_7.10(x_int)  # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_7.10(x_num)  # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_7.10(x_char) # num [1:3] NA NA NA # wrong, but clearly so.
#>  num [1:3] NA NA NA
str_as_num_7.10(x_fact) # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7

# as.numeric() gives the *indices* of the factor levels for factors
str_as_num_base(x_int)  # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_base(x_num)  # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_base(x_char) # num [1:3] 5 6 7 # fine
#>  num [1:3] 5 6 7
str_as_num_base(x_fact) # num [1:3] 1 2 3 # wrong!
#>  num [1:3] 1 2 3
```
