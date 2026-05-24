# Create a character vector from a string with quotation marks

Create a character vector from a string with quotation marks

## Usage

``` r
unpaste_unquote(x, collapse = c(", ", "; "), quotemarks = c("'", "\""))
```

## Arguments

- x:

  A character string

- collapse:

  Character vector with elements that were used to collapse values when
  `x` was created, and are now used to
  [split](https://rdrr.io/r/base/strsplit.html) `x` on. Can be
  `character(0)` to leave `x` as a character string.

- quotemarks:

  Character vector with quotation marks to be removed from `x`. Can be
  `character(0)` to not remove any quotation marks.

## Value

`x` without the quotation marks and split into a vector.

## Details

`unpaste_unquote()` is not the exact reverse of
[`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html):
it does *not* restore zero-length elements to their original value,
e.g., `"'NULL'"` to `NULL`, or `"'character(0)'"` to `character(0)`.

## See also

[`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.html)
for the approximate opposite of `unpaste_unquote()`.

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

## Examples

``` r
x <- paste_quoted(c("ff", "gG", "HH"))
x
#> [1] "'ff', 'gG', 'HH'"
unpaste_unquote(x = x, collapse = ", ", quotemarks = "'")
#> [1] "ff" "gG" "HH"
unpaste_unquote(x = x, collapse = ", ", quotemarks = "\"")
#> [1] "'ff'" "'gG'" "'HH'"
unpaste_unquote(x = x, collapse = character(0), quotemarks = c("'", "\""))
#> [1] "ff, gG, HH"
unpaste_unquote(x = x, collapse = character(0), quotemarks = character(0))
#> [1] "'ff', 'gG', 'HH'"
```
