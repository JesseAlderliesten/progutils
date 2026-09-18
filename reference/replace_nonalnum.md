# Replace non-alphanumeric characters

Replace or remove non-alphanumeric characters, keeping dots and handling
underscores as indicated by `keep_underscores`.

## Usage

``` r
replace_nonalnum(x, replacement = "_", keep_underscore = TRUE)
```

## Arguments

- x:

  [character
  vector](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  with the (possibly empty) strings to remove non-alphanumeric
  characters from.

- replacement:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  used to replace non-alphanumeric characters. It might be empty (i.e.,
  `""`) to remove instead of replace non-alphanumeric characters.

- keep_underscore:

  `TRUE` or `FALSE`: keep underscores? If `FALSE`, underscores are
  replaced by `replacement`, which only makes sense if a non-default
  `replacement` is used such that it is not an underscore, see the last
  `Example`.

## Value

`x` with non-alphanumeric characters other than dots, and possibly
underscores, replaced by `replacement`.

## Details

This implementation uses the [character
class](https://rdrr.io/r/base/regex.html) `[:alnum:]` that depends on
the current [locale](https://rdrr.io/r/base/locales.html), see the
`Programming notes` in
[`checkinput::all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.html).

Empty strings are allowed as input to `x` because they might also be
returned by `replace_nonalnum()` if the empty character string (`""`) is
used for `replacement`.

## See also

[`trimws()`](https://rdrr.io/r/base/trimws.html) to remove leading
and/or trailing whitespace;
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md)
to replace specific values;
[`checkinput::all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.html)
and
[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
for more specific checks.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`round_levels()`](https://jessealderliesten.github.io/progutils/reference/round_levels.md),
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md)

## Examples

``` r
replace_nonalnum("a+b.txt")
#> [1] "a_b.txt"
replace_nonalnum("a b.txt")
#> [1] "a_b.txt"
replace_nonalnum("ab12._")  # returned unchanged
#> [1] "ab12._"
replace_nonalnum(c("a+b.txt", "a b.txt", "ab12._"))
#> [1] "a_b.txt" "a_b.txt" "ab12._" 

# Removing instead of replacing nonalphanumeric characters
replace_nonalnum("a+b.txt", replacement = "") # "ab.txt"
#> [1] "ab.txt"

# Handling underscores
replace_nonalnum("a+bc_d.txt", replacement = "", keep_underscore = TRUE)  # "abc_d.txt"
#> [1] "abc_d.txt"
replace_nonalnum("a+bc_d.txt", replacement = "", keep_underscore = FALSE) # "abcd.txt"
#> [1] "abcd.txt"
# Seems to keep the underscore because the default 'replacement' is an underscore
replace_nonalnum("a+bc_d.txt", keep_underscore = FALSE) # "a_bc_d.txt"
#> [1] "a_bc_d.txt"
```
