# Check for values that differ only in their case

Check for values that differ only in their case

## Usage

``` r
check_case_helper(x, signal = c("error", "warning", "message", "quiet"))
```

## Arguments

- x:

  Character vector to check.

- signal:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  indicating the type of signal to be used: `"error"` to throw an
  [error](https://rdrr.io/r/base/stop.html), `"warning"` to issue a
  [warning](https://rdrr.io/r/base/warning.html), `"message"` to show a
  [message](https://rdrr.io/r/base/message.html), or `"quiet"` to be
  quiet.

## Value

A [character
vector](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
with [unique](https://rdrr.io/r/base/unique.html) values in `x` that
only differ from each other in their case, or `character(0)` if no such
values are present, returned
[invisibly](https://rdrr.io/r/base/invisible.html).

## Details

Values in `x` that only differ from each other in their case lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal`.

## See also

[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md)
to check between two vectors for values that only differ in their case;
[`tolower()`](https://rdrr.io/r/base/chartr.html) and
[`toupper()`](https://rdrr.io/r/base/chartr.html) to change case.

## Examples

``` r
x <- c("Ee", "Ee", "LL", "Ll")
try(progutils:::check_case_helper(x = x, signal = "error"))
#> Error : 'x' contains values that only differ in their case: 'LL', 'Ll'
progutils:::check_case_helper(x = x, signal = "warning")
#> Warning: 'x' contains values that only differ in their case: 'LL', 'Ll'
```
