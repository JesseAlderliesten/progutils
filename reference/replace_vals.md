# Replace character values or factor levels

Replace character values or factor levels

## Usage

``` r
replace_vals(
  x,
  old,
  new,
  ignore_case = FALSE,
  allow_multiple = TRUE,
  warn_absent = TRUE,
  signal_case_old = c("warning", "error", "message", "quiet"),
  signal_case_new = c("warning", "error", "message", "quiet"),
  signal_old_ignore_case = TRUE,
  quiet = FALSE
)
```

## Arguments

- x:

  [Character](https://rdrr.io/r/base/character.html) vector with values
  to be replaced, or [factor](https://rdrr.io/r/base/factor.html) with
  [levels](https://rdrr.io/r/base/levels.html) to be replaced.

- old:

  Character vector with values to be replaced. Should be
  [unique](https://rdrr.io/r/base/unique.html) and not contain the value
  `new`, otherwise an error is thrown.

- new:

  Character string of length one with the new value.

- ignore_case:

  `TRUE` or `FALSE`: ignore [case](https://rdrr.io/r/base/chartr.html)
  when looking for `old`? See `Details`.

- allow_multiple:

  `TRUE` or `FALSE`: allow multiple values of `old` to match `x`? See
  `Details`.

- warn_absent:

  `TRUE` or `FALSE`: warn if `old` nor `new` are found in `x`? It is
  silently assumed replacement is not necessary anymore if `new` is
  found.

- signal_case_old, signal_case_new:

  `"error"`, `"warning"`, `"message"`, or `"quiet"`: character string
  indicating the type of
  [signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
  if values in `x` are a case-insensitive match but not a case-sensitive
  match to `old` or `new`, respectively. For `signal_case_old`, this is
  also influenced by argument `signal_old_ignore_case`.

- signal_old_ignore_case:

  `TRUE` or `FALSE`: use `signal_case_old` if `ignore_case` is `TRUE`?
  If `FALSE`, no signal is emitted if `ignore_case` is `TRUE`.

- quiet:

  `TRUE` or `FALSE`: suppress printing the message with the values that
  have been replaced?

## Value

`x` with the requested replacements. Factor levels are **not** reordered
after the replacement.

## Details

Values in `x` that only differ from `new` in their case are not
adjusted, but lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal_case_new`.

An error is thrown if `allow_multiple` is `FALSE` and multiple values of
`old` match `x`, unless those values of `old` only differ in their case
and `ignore_case` is `TRUE`. An example of such an exception is
`replace_vals(x = c("a", "A"), old = c("a", "A"), new = "b", ignore_case = TRUE, allow_multiple = FALSE)`.

If `quiet` is `FALSE`, a message indicates which values have been
replaced. The order of the factor *levels* determines the order used in
the message.

## See also

[`gsub()`](https://rdrr.io/r/base/grep.html)
[`replace()`](https://rdrr.io/r/base/replace.html)

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_filename()`](https://jessealderliesten.github.io/progutils/reference/get_filename.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md)

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

Other functions to modify factors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`reorder_levels()`](https://jessealderliesten.github.io/progutils/reference/reorder_levels.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
x <- c("k", "l", "m")

# All values in 'x' that match any value of 'old' are replaced by 'new'.
replace_vals(x = rep(x, 2), old = x[3:2], new = "b", allow_multiple = TRUE)
#> Replaced values 'l', 'm' with 'b'
#> [1] "k" "b" "b" "k" "b" "b"

# Factor input to 'x' is handled by replacing the values and levels. The
# levels are not reordered
replace_vals(x = as.factor(x), old = x[3:2], new = "b", allow_multiple = TRUE)
#> Replaced values 'l', 'm' with 'b'
#> [1] k b b
#> Levels: k b

# Case-insensitive matching is used if 'ignore_case' is TRUE, with a warning
# if 'signal_case_old' is '"warning"'
replace_vals(x = "A", old = "a", new = "b", ignore_case = TRUE,
             signal_case_old = "warning")
#> Warning: Values in 'x' are a case-insensitive match but not a case-sensitive match to 'old' ('a'): 'A'
#> Replaced values 'A' with 'b'
#> [1] "b"

# Case-sensitive matching is used if 'ignore_case' is FALSE. A warning is
# issued if no match is found and 'warn_absent' is 'TRUE'
replace_vals(x = "A", old = "a", new = "b", ignore_case = FALSE,
             warn_absent = TRUE)
#> Warning: Values in 'x' are a case-insensitive match but not a case-sensitive match to 'old' ('a'): 'A'
#> Warning: None of the values of argument 'old' ('a') were found in 'x' ('A')!
#> [1] "A"

# If 'allow_multiple' is FALSE, an error is thrown if multiple values of
# 'old' match 'x'.
try(replace_vals(x = x, old = letters[13:12], new = "b",
                 allow_multiple = FALSE))
#> Error in replace_vals(x = x, old = letters[13:12], new = "b", allow_multiple = FALSE) : 
#>   Multiple values of 'old' ('m', 'l') matched 'x' ('k', 'l', 'm'): 'l', 'm'
```
