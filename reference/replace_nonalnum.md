# Replace Non-Alphanumeric Characters

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
  with strings to remove non-alphanumeric characters from. Empty strings
  (`""`) are allowed.

- replacement:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  used to replace non-alphanumeric characters. It might be empty (i.e.,
  `""`) to remove instead of replace non-alphanumeric characters.

- keep_underscore:

  `TRUE` or `FALSE`: keep underscores? If `FALSE`, underscores are
  replaced by `replacement`, which is only apparent if a non-default
  `replacement` is used such that it is not an underscore, see the last
  'Example'.

## Value

`x` with non-alphanumeric characters other than dots, as well as
underscores if `keep_underscores` is `FALSE`, replaced by `replacement`.
An empty string (`""`) is returned if the empty character string (`""`)
is used for `replacement` and `x` does not contain non-replaced
characters.

## Details

This implementation uses the [character
class](https://rdrr.io/r/base/regex.html) `[:alnum:]` that depends on
the current [locale](https://rdrr.io/r/base/locales.html), see the
'Programming Notes' in
[`checkinput::all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.html).

## See also

[`trimws()`](https://rdrr.io/r/base/trimws.html) to remove leading
whitespace, trailing whitespace, or both;
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
with more relevant links, to replace specific values;
[`checkinput::all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.html)
and
[`checkinput::is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.html)
for checks on the validity of strings.

Other functions to check equality:
[`are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.md),
[`check_case()`](https://jessealderliesten.github.io/progutils/reference/check_case.md),
[`get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.md),
[`not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signif_custom()`](https://jessealderliesten.github.io/progutils/reference/signif_custom.md)

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
`reexports`,
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`signal_text()`](https://jessealderliesten.github.io/progutils/reference/signal_text.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

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
replace_nonalnum("a+bc_d.txt", replacement = "",
                 keep_underscore = TRUE)  # "abc_d.txt"
#> [1] "abc_d.txt"
replace_nonalnum("a+bc_d.txt", replacement = "",
                 keep_underscore = FALSE) # "abcd.txt"
#> [1] "abcd.txt"
# Seems to keep the underscore because the
# default 'replacement' is an underscore
replace_nonalnum("a+bc_d.txt",
                 keep_underscore = FALSE) # "a_bc_d.txt"
#> [1] "a_bc_d.txt"
```
