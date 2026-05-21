# Convert a vector to a character string

Convert a vector to a character string, preserving names, rounding
numeric values.

## Usage

``` r
vect_to_char(
  x,
  signif = 3L,
  width = Inf,
  sep = ": ",
  collapse = ", ",
  ignore_newlines = TRUE
)
```

## Arguments

- x:

  A vector, [factor](https://rdrr.io/r/base/factor.html), `NULL`, or a
  non-dataframe [list](https://rdrr.io/r/base/list.html) (unlisted
  through `unlist(x, use.names = TRUE)`, with a warning).

- signif:

  Positive number of length one, rounded to the nearest positive integer
  indicating the number of significant digits to round numeric `x` to.

- width:

  A positive number giving the maximum line width (in characters) after
  wrapping. Can be `Inf` to not wrap text.

- sep:

  Character string of length one used to separate the names and the
  values. Ignored if `x` does not have names.

- collapse:

  Character string of length one to collapse values into a single
  character string, or `NULL` to return each value as an element of a
  character vector.

- ignore_newlines:

  `TRUE` or `FALSE`: should newlines in `x` be replaced by a blank
  character?

## Value

The names and values in `x`, with values of numeric `x` rounded to
`signif` [significant](https://rdrr.io/r/base/Round.html) digits,
wrapped to `width` characters.

If `collapse` is `NULL`, a character *vector* with the same elements as
`x` is returned, wrapping on a per-element basis. If `collapse` is not
`NULL`, the name-value pairs are separated by `collapse`, thus returning
a character *string*.

See `details` on handling of some special values.

## Details

`vect_to_char()` returns `NULL` as `"NULL"`, other zero-length objects
as `"<class>(0)"` (e.g., `"logical(0)"`), `""` as `'""'`, and
non-logical `NA`s as `"NA_<class>_"` (e.g., `"NA_real_"`; for
[factors](https://rdrr.io/r/base/factor.html) this is
`"NA_character_"`).

## Programming notes

To get a cross-tabulation of `x` into a character string, one can use
`paste0(vect_to_char(c(table(x)), sep = " (", collapse = "), "), ")")`,
see the last `Example`.

## See also

[`toString()`](https://rdrr.io/r/base/toString.html) which can be used
if names of `x` can be removed.

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`reorder_cols()`](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

Other functions to modify factors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
x <- 1:3
names(x) <- letters[x]
vect_to_char(x = x) # "a: 1, b: 2, c: 3"
#> [1] "a: 1, b: 2, c: 3"
vect_to_char(x = x, collapse = NULL) # c("a: 1", "b: 2", "c: 3")
#> [1] "a: 1" "b: 2" "c: 3"
vect_to_char(x = unname(x)) # "1, 2, 3"
#> [1] "1, 2, 3"
y <- x / 7
vect_to_char(x = y, signif = 7) # "a: 0.1428571, b: 0.2857143, c: 0.4285714"
#> [1] "a: 0.1428571, b: 0.2857143, c: 0.4285714"
vect_to_char(x = y, signif = 2, sep = " = ", collapse = " and ", width = 15)
#> [1] "a = 0.14 and b\n= 0.29 and c =\n0.43"
# "a = 0.14 and b\n= 0.29 and c =\n0.43"

x_char <- c(a = "abc", b = "def", c = "this is text")
vect_to_char(x = x_char) # "a: abc, b: def, c: this is text"
#> [1] "a: abc, b: def, c: this is text"
vect_to_char(x = unname(x_char)) # "abc, def, this is some text"
#> [1] "abc, def, this is text"

# Using vect_to_char() to get a frequency table
x <- 1:10
names(x) <- letters[x]
y <- 5:15
names(y) <- letters[y]
x <- c(x, y)
paste0(vect_to_char(c(table(x)), sep = " (", collapse =  "), "), ")")
#> [1] "1 (1), 2 (1), 3 (1), 4 (1), 5 (2), 6 (2), 7 (2), 8 (2), 9 (2), 10 (2), 11 (1), 12 (1), 13 (1), 14 (1), 15 (1)"
```
