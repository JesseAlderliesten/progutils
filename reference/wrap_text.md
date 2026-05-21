# Wrap text

Wrap text at blank characters to achieve a maximum line width.

## Usage

``` r
wrap_text(x, width = 80L, ignore_newlines = TRUE)
```

## Arguments

- x:

  Character vector to be wrapped.

- width:

  A positive number giving the maximum line width (in characters) after
  wrapping. Can be `Inf` to not wrap text.

- ignore_newlines:

  `TRUE` or `FALSE`: should newlines in `x` be replaced by a blank
  character?

## Value

A string containing `x` wrapped to a maximum of `width` characters, with
newlines inserted at blank characters. A warning is issued if the width
of a fragment in the output exceeds `width`: this occurs if a stretch of
characters longer than `width` occurs without a blank character to wrap
at.

## Details

`x` of length larger than one is pasted into a single string, separating
the parts by blank characters.

Consecutive white space is collapsed into a single blank character,
except for double spaces after periods, question marks and exclamation
marks (as documented in the section `Details` of
[`strwrap()`](https://rdrr.io/r/base/strwrap.html)).

Leading and trailing newlines are collapsed into a single blank
character if `ignore_newlines` is `TRUE` and are retained if
`ignore_newlines` is `FALSE`.

## Programming notes

The call `wrap_text(x, width)` can be replaced by
`paste0(strwrap(x, width + 1L), collapse = "\n")` if `x` has length one
and `ignore_newlines` is `TRUE`.

Using `wrap_text()` on `x` of variable length, e.g., in the text of
warnings that report a file path or a user-provided variable name, makes
the location of newlines unpredictable and thus difficult to test
reliably. To circumvent this, put the constant part in the front of the
message and hardcode newlines using `\n`.

## Notes

The output is printed as a string with newlines represented as `\n`. Use
[`cat()`](https://rdrr.io/r/base/cat.html) on the output to print it in
the way it is formatted in messages.

Argument `width` in `wrap_text()` indicates the maximum width of text
after wrapping, i.e., the width **after** which text should be wrapped.
In contrast, argument `width` in
[`strwrap()`](https://rdrr.io/r/base/strwrap.html) indicates the width
**at** which text should be wrapped.

## See also

[`cat()`](https://rdrr.io/r/base/cat.html)
[`paste()`](https://rdrr.io/r/base/paste.html)
[`strwrap()`](https://rdrr.io/r/base/strwrap.html) `bbmle:::strwrapx()`

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
[`file_path_no_ext()`](https://jessealderliesten.github.io/progutils/reference/file_path_no_ext.md),
[`reorder_cols()`](https://jessealderliesten.github.io/progutils/reference/reorder_cols.md),
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)

## Examples

``` r
example_text <- "A piece\nof text that you want to wrap over multiple lines"
cat(wrap_text(example_text,
              width = 20, ignore_newlines = TRUE))
#> A piece of text that
#> you want to wrap
#> over multiple lines
cat(wrap_text(example_text,
              width = 20, ignore_newlines = FALSE))
#> A piece
#> of text that you
#> want to wrap over
#> multiple lines
```
