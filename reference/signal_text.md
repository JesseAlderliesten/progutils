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

  Character string indicating the type of signal to be used: `"error"`
  to throw an [error](https://rdrr.io/r/base/stop.html), `"warning"` to
  issue a [warning](https://rdrr.io/r/base/warning.html), `"message"` to
  show a [message](https://rdrr.io/r/base/message.html), or `"quiet"` to
  be quiet.

- origin:

  Character string pasted behind `text` giving the origin of the
  message. Ignored if it is `character(0)`.

## Value

`text`, returned [invisibly](https://rdrr.io/r/base/invisible.html).

## Details

`text` is coerced to a character vector using
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md),
which treats input with length larger than one better than
[message](https://rdrr.io/r/base/message.html) etc. that use
[`paste0()`](https://rdrr.io/r/base/paste.html), see the `Examples`.
Call
[`vect_to_char()`](https://jessealderliesten.github.io/progutils/reference/vect_to_char.md)
beforehand on `text` to control rounding and wrapping.

## Side effects

`text` is signalled through an
[error](https://rdrr.io/r/base/stop.html),
[warning](https://rdrr.io/r/base/warning.html),
[message](https://rdrr.io/r/base/message.html), or quietly, depending on
argument `signal`.

## See also

[`wrap_text()`](https://jessealderliesten.github.io/progutils/reference/wrap_text.md)

## Examples

``` r
test_text <- c("Some text", "Some other text")
try(signal_text(text = test_text, signal = "error"))
#> Error : Some text, Some other text
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
```
