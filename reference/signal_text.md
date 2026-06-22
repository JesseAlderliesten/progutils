# Signal a text to the user

Signal a text to the user through an error, warning, or message.

## Usage

``` r
signal_text(
  text,
  signal = c("error", "warning", "message", "quiet"),
  origin = character(0)
)
```

## Arguments

- text:

  Vector with text to be signalled, coerced to character by
  [`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md).

- signal:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.html)
  indicating the type of signal to be used: `"error"` to throw an
  [error](https://rdrr.io/r/base/stop.html), `"warning"` to issue a
  [warning](https://rdrr.io/r/base/warning.html), `"message"` to show a
  [message](https://rdrr.io/r/base/message.html), or `"quiet"` to be
  quiet.

- origin:

  Character string pasted behind `text` giving the origin of the
  message. Ignored if it is `character(0)`.

## Value

`text`, with a phrase indicating the origin of the signal if `origin` is
not `NULL`, returned [invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`text` is coerced to a character vector using
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
which treats zero-length input and input with length larger than one
better than [`message()`](https://rdrr.io/r/base/message.html) etc. that
use [`paste0()`](https://rdrr.io/r/base/paste.html), see the `Examples`.
Call
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)
beforehand on `text` to control rounding and wrapping, see the
`Examples`.

`text` is signalled through an
[error](https://rdrr.io/r/base/stop.html),
[warning](https://rdrr.io/r/base/warning.html),
[message](https://rdrr.io/r/base/message.html), or quietly, depending on
argument `signal`. To make `text` available for further queries when
using `signal_text()` in another function, add the content of the signal
as an attribute to the returned object, see the last `Example`.

## See also

[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

Other functions to modify character vectors:
[`as.numeric_safe()`](https://jessealderliesten.github.io/progutils/reference/as.numeric_safe.md),
`reexports`,
[`replace_vals()`](https://jessealderliesten.github.io/progutils/reference/replace_vals.md),
[`unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.md),
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

## Examples

``` r
test_text <- c("Some text", "Some other text")
try(signal_text(text = test_text, signal = "error"))
#> Error : Some text, Some other text
signal_text(text = test_text, signal = "warn")
#> Warning: Some text, Some other text
signal_text(text = test_text, signal = "message")
#> Some text, Some other text
signal_text(text = test_text, signal = "quiet")

# signal_text() handles input to 'text' with length larger than one more
# sensible than message():
test_numbers <- 11:13
signal_text(text = test_numbers, signal = "message")
#> 11, 12, 13
message(test_numbers)
#> 111213

# Call vect_to_char() beforehand on 'text' to control rounding of numbers and
# wrapping of text:
message(test_numbers / 7)
#> 1.571428571428571.714285714285711.85714285714286
signal_text(text = test_numbers / 7, signal = "message")
#> 1.57, 1.71, 1.86
signal_text(text = vect_to_char(x = test_numbers / 7, signif = 4, width = 15),
            signal = "message")
#> 1.571, 1.714,
#> 1.857

# This example shows how to make the content of the signal available for
# further queries:
signal_negative <- function(x, signal = c("error", "warning", "message", "quiet")) {
  text_signal <- "This is a negative number"
  if(x < 0) {
    progutils::signal_text(text = text_signal, signal = signal)
    attributes(x) <- list(text_signal = text_signal)
  } else {
    # Using '""' as attribute such that
    # grepl(pattern = "<text>", x = attr(x, "text_signal"), fixed = TRUE)
    # returns 'FALSE' if <text> is not found in 'text_signal' (using
    # 'character(0)' as attribute would lead to 'logical(0)' as return).
    attributes(x) <- list(text_signal = "")
  }
  x
}

res_negative <- signal_negative(x = -1, signal = "warning")
#> Warning: This is a negative number
res_positive <- signal_negative(x = 1, signal = "warning")
res_negative
#> [1] -1
#> attr(,"text_signal")
#> [1] "This is a negative number"
res_positive
#> [1] 1
#> attr(,"text_signal")
#> [1] ""
grepl(pattern = "negative", x = attr(res_negative, "text_signal"),
      ignore.case = FALSE, fixed = TRUE) # TRUE
#> [1] TRUE
grepl(pattern = "negative", x = attr(res_positive, "text_signal"),
      ignore.case = FALSE, fixed = TRUE) # FALSE
#> [1] FALSE
```
