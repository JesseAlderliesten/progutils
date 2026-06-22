# Check for values that differ only in their case

Check for values that differ only in their case

## Usage

``` r
check_case(x, signal = c("error", "warning", "message", "quiet"))
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

Character vector with [unique](https://rdrr.io/r/base/unique.html),
[sorted](https://rdrr.io/r/base/sort.html) values in `x` that only
differ from each other in their case, or `character(0)` if no such
values are present. The return is
[invisible](https://rdrr.io/r/base/invisible.html).

## Details

Values in `x` that only differ from each other in their case lead to a
[signal](https://jessealderliesten.github.io/progutils/reference/signal_text.md)
as indicated by argument `signal`.

## Notes

The sorting order in the result depends on the used
[locale](https://rdrr.io/r/base/locales.html) (see also the section
`Details` of [`Comparison`](https://rdrr.io/r/base/Comparison.html)),
which also affects if uppercase characters are sorted before or after
lowercase characters.

## See also

[`tolower()`](https://rdrr.io/r/base/chartr.html) and
[`toupper()`](https://rdrr.io/r/base/chartr.html) to **change** case.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)

## Examples

``` r
x <- c("Ee", "Ee", "LL", "Ll")
try(check_case(x = x, signal = "error"))
#> Error : 'x' contains values that only differ in their case: 'LL', 'Ll'
check_case(x = x, signal = "warning")
#> Warning: 'x' contains values that only differ in their case: 'LL', 'Ll'
```
