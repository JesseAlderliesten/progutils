# Check for Case Differences

Signal the presence of values that differ only in their case.

## Usage

``` r
check_case(
  x,
  y = NULL,
  signal = c("error", "warning", "message", "quiet"),
  signal_within = c("warning", "message", "quiet", "error")
)
```

## Arguments

- x, y:

  [character
  vectors](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  to check, see `Value`.

- signal:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  indicating the type of signal to be used: `"error"` to throw an
  [error](https://rdrr.io/r/base/stop.html), `"warning"` to issue a
  [warning](https://rdrr.io/r/base/warning.html), `"message"` to show a
  [message](https://rdrr.io/r/base/message.html), or `"quiet"` to be
  quiet.

- signal_within:

  as `signal` but indicating the type of
  [signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
  to be used if values within `x` or within `y` differ only in their
  case.

## Value

A [character
vector](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
with [unique](https://rdrr.io/r/base/unique.html) values that only
differ from other values in their case, or `character(0)` if no such
values are present, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

If `y` is **not** `NULL`: values in `x` that only differ from values in
`y` in their case. Such values lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal`. Values within `x` or within `y` only
differ in their case lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal_within`.

If `y` is `NULL`: values in `x` that only differ from other values in
`x` in their case. Such values lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal_within`.

## See also

[`tolower()`](https://rdrr.io/r/base/chartr.html) and
[`toupper()`](https://rdrr.io/r/base/chartr.html) to change case.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_nonalnum()`](https://jessealderliesten.github.io/progutils/reference/replace_nonalnum.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md)

## Examples

``` r
x <- c("a", "b", "C", "D")
y <- c("a", "B", "c", "D")
z <- c("a", "b", "C", "D", "A")

try(check_case(x = x, y = y, signal = "error")) # an error
#> Error : 'x' contains values that only differ in their case from values in 'y':
#> 'b', 'C'
# c("b", "C"), with a warning:
(check_case(x = x, y = y, signal = "warning"))
#> Warning: 'x' contains values that only differ in their case from values in 'y':
#> 'b', 'C'
#> [1] "b" "C"

(check_case(x = x, y = "h", signal = "warning")) # character(0)
#> character(0)

(check_case(x = z, y = NULL, signal = "warning",
            signal_within = "message"))
#> 'x' contains values that only differ in their case: 'a', 'A'
#> [1] "a" "A"
# c("a", "A"), with a message:
# 'x' contains values that only differ in their case: 'a', 'A'

(check_case(x = z, y = "h", signal = "warning",
            signal_within = "warning"))
#> Warning: 'x' contains values that only differ in their case: 'a', 'A'
#> character(0)
# character(0), with a warning:
# 'x' contains values that only differ in their case: 'a', 'A'
(check_case(x = z, y = "h", signal = "warning",
            signal_within = "quiet"))
#> character(0)
# character(0), without a warning

(check_case(x = x, y = z, signal = "warning"))
#> Warning: 'y' contains values that only differ in their case: 'a', 'A'
#> Warning: 'x' contains values that only differ in their case from values in 'y':
#> 'a'
#> [1] "a"
# "a", with warnings
# 1: 'y' contains values that only differ in their case:
# 'a', 'A'
# 2: 'x' contains values that only differ in their case
# from values in 'y': 'a'
```
