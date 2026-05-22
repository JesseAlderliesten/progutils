# Are values absent

Check that values from one vector are absent from another vector

## Usage

``` r
not_in(x, table, value = TRUE)
```

## Arguments

- x:

  Vector or factor with values to test absence from `table`. `x` should
  have a length larger than zero and not be of
  [type](https://rdrr.io/r/base/typeof.html) `double`.

- table:

  Vector or factor in which to test for absence of `x`. `table` should
  have a length larger than zero and not be of `type` `double`.

- value:

  `TRUE` or `FALSE`: should a vector with values be returned instead of
  a boolean vector?

## Value

If `value` is `TRUE`: the values in `x` that are absent from `table`,
with a zero-length object of the same type as `x`, e.g., `character(0)`
or `logical(0)` if none of the values in `x` are absent from `table`
(i.e., all are present in `table`). If `value` is `FALSE`: a boolean
vector indicating for each element in `x` if it is absent from `table`.

## Details

Duplicates in `x` are kept, in contrast to
[`setdiff()`](https://rdrr.io/r/base/sets.html), see the `Examples`.

[Factor-input](https://rdrr.io/r/base/factor.html) to `x` is converted
to character, to prevent returning a factor with all values of `x` as
[levels](https://rdrr.io/r/base/levels.html).

[NA](https://rdrr.io/r/base/NA.html)s are allowed in `x` and `table` and
behave the same as other values: the returned `NA`s (if `value` is
`TRUE`) and the returned zero-length value (if `value` is `FALSE`) have
the same type as the `NA`s in `x` if `NA`s are absent from `table`.
`NA`s of different types in `x` and `table` match each other.

Names are *not* considered when matching but are retained in the output,
similar to `%in%`.

## Programming notes

`not_in()` does not allow input of
[type](https://rdrr.io/r/base/typeof.html) `double` because matching
such input should take small numerical errors into account by using a
tolerance, for example, as the error message indicates, using
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md).

`not_in()` does not allow zero-length input because zero-length input
behaves slightly different from other values: if `character(0)` is
present in `x` but absent from `table`, `not_in()` would return
`logical(0)` if `value` is `FALSE`. If `value` is `TRUE`, the behaviour
would be normal: returning `character(0)`.

Apart from not allowing numeric or zero-length input,
`not_in(x, table, value = TRUE)` is equivalent to `x %w/o% table`, where
`%w/o%` is an unexported function from `tools`: `tools:::'%w/o%'`.
Similarly, `not_in(x, table, value = FALSE)` is equivalent to
`x %notin% table`, where `%notin%` is an exported function from `base-`R
since version `4.6.0` which before that was present as unexported
function `tools:::'%notin%'`.

## See also

[`setdiff()`](https://rdrr.io/r/base/sets.html) for a similar function
which removes duplicates,
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md)
to match numeric input using a tolerance,
[`match()`](https://rdrr.io/r/base/match.html), and, from R `4.6.0`
onwards, `'%notin%'`, on which this function is based.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
x <- letters[1:4]
table <- letters[3:6]
not_in(x, table) # c("a", "b")
#> [1] "a" "b"
not_in(as.factor(x), as.factor(table)) # c("a", "b")
#> [1] "a" "b"
# c(TRUE, TRUE, FALSE, FALSE), same as !(x %in% table):
not_in(x, table, value = FALSE)
#> [1]  TRUE  TRUE FALSE FALSE

x_dupl <- c(x, letters[c(2, 4:6, 5)])
table_dupl <- letters[c(3:8, 5:7)]
not_in(x_dupl, table_dupl) # c("a", "b", "b")
#> [1] "a" "b" "b"
setdiff(x_dupl, table_dupl) # c("a", "b")
#> [1] "a" "b"
not_in(x_dupl, table_dupl, value = FALSE)
#> [1]  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
# c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)

# Names are not considered when matching but are retained in the output
not_in(c(x = "c", y = "b", z = "a"), c(a = "a", b = "b"))
#>   x 
#> "c" 
```
