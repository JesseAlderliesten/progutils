# Check that values are absent

Check that values from one vector are absent from another vector

## Usage

``` r
not_in(x, table, value = TRUE)
```

## Arguments

- x:

  Vector or factor with values to test absence from `table`. `x` should
  not be of [type](https://rdrr.io/r/base/typeof.html) `double`.

- table:

  Vector or factor in which to test for absence of `x`. `table` should
  not be of `type` `double`.

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

Zero-length input behaves slightly different from other values:
`not_in()` returns `logical(0)` for zero-length input to `x` if that is
absent from `table` if `value` is `FALSE`. If `value` is `TRUE`, the
behaviour is normal: returning `x`.

[NA](https://rdrr.io/r/base/NA.html) is allowed in `x` and `table` and
behaves the same as other values: the returned `NA` (if `value` is
`TRUE`) and the returned zero-length value (if `value` is `FALSE`) have
the same type as the `NA` in `x` if `NA` is absent from `table`. `NA`s
of different types in `x` and `table` match each other.

Names are **not** considered when matching but are retained in the
output, similar to `%in%`.

## Programming notes

`not_in()` does **not** allow input of
[type](https://rdrr.io/r/base/typeof.html) `double` because matching
such input should allow for small numerical errors by using a tolerance,
for example, as the error message indicates, using
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md).

Apart from **not** allowing numeric input,
`not_in(x, table, value = FALSE)` is equivalent to `x %notin% table`,
where `%notin%` is a function in base R since version `4.6.0`.

## See also

[`setdiff()`](https://rdrr.io/r/base/sets.html) for a similar function
which removes duplicates;
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md)
to match numeric input using a tolerance;
[`identical()`](https://rdrr.io/r/base/identical.html), `not_in()` and
[`match()`](https://rdrr.io/r/base/match.html) (containing `%in%` and,
from R `4.6.0` onwards, `'%notin%'`) to check for exact (in)equality,
with [`Comparison`](https://rdrr.io/r/base/Comparison.html) to do so
using binary operators

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
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
