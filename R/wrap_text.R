#' Wrap text
#'
#' Wrap text at blank characters to achieve a maximum line width.
#'
#' @param x Character vector to be wrapped.
#' @param width A positive number giving the maximum line width (in characters)
#' after wrapping. Can be `Inf` to not wrap text.
#' @param ignore_newlines `TRUE` or `FALSE`: should newlines in `x` be replaced
#' by a blank character?
#'
#' @details
#' `x` of length larger than one is pasted into a single string, separating the
#' parts by blank characters.
#'
#' Consecutive white space is collapsed into a single blank character, except
#' for double spaces after periods, question marks and exclamation marks (as
#' documented in the section `Details` of [strwrap()]).
#'
#' Leading and trailing newlines are collapsed into a single blank character if
#' `ignore_newlines` is `TRUE` and are retained if `ignore_newlines` is `FALSE`.
#'
#' @returns
#' A string containing `x` wrapped to a maximum of `width` characters, with
#' newlines inserted at blank characters. A warning is issued if the width of a
#' fragment in the output exceeds `width`: this occurs if a stretch of
#' characters longer than `width` occurs without a blank character to wrap at.
#'
#' @section Programming notes:
#' The call `wrap_text(x, width)` can be replaced by
#' `paste0(strwrap(x, width + 1L), collapse = "\n")` if `x` has length one and
#' `ignore_newlines` is `TRUE`.
#'
#' @section Notes:
#' The output is printed as a string with newlines represented as `\n`. Use
#' [cat()] on the output to print it in the way it is formatted in messages.
#'
#' Argument `width` in `wrap_text()` indicates the maximum width of text after
#' wrapping, i.e., the width **after** which text should be wrapped. In
#' contrast, argument `width` in [strwrap()] indicates the width **at** which
#' text should be wrapped.
#'
#' @section Programming notes:
#' Using `wrap_text()` on `x` of variable length, e.g., in the text of warnings
#' that report a file path or a user-provided variable name, makes the location
#' of newlines unpredictable and thus difficult to test reliably. To circumvent
#' this, put the constant part in the front of the message and hardcode newlines
#' using `\n`.
#'
#' @seealso [cat()] [paste()] [strwrap()] `bbmle:::strwrapx()`
#'
#' @family functions to modify character vectors
#'
#' @examples
#' example_text <- "A piece\nof text that you want to wrap over multiple lines"
#' cat(wrap_text(example_text,
#'               width = 20, ignore_newlines = TRUE))
#' cat(wrap_text(example_text,
#'               width = 20, ignore_newlines = FALSE))
#'
#' @export
wrap_text <- function(x, width = 80L, ignore_newlines = TRUE) {
  stopifnot(checkinput::all_characters(x, allow_empty = TRUE),
            checkinput::is_positive(width),
            checkinput::is_logical(ignore_newlines))
  if(length(x) > 1L) {
    x <- paste0(x, collapse = " ")
  }

  bool_start_blank <- grepl(pattern = "^[[:blank:]]", x = x)
  bool_start_newline <- grepl(pattern = "^\n", x = x)
  bool_end_blank <- grepl(pattern = "[[:blank:]]$", x = x)
  bool_end_newline <- grepl(pattern = "\n$", x = x)

  n_discard <- 0L
  if(!ignore_newlines) {
    x <- strsplit(x = x, split = "\n")[[1]]
  } else {
    if(bool_start_newline) {
      n_discard <- attr(regexpr("^\n+", x), "match.length") - 1L
    }
  }

  # Notes:
  # - strwrap() removes leading white space, trailing white space, and trailing
  #   newlines; and converts each leading newline to an element with an empty
  #   character string. See 'Details' about their handling in 'wrap_text()'.
  # - Using floor(x = width) because floor(Inf) is Inf, whereas as.integer(Inf)
  #   is NA. Using 'width = width + 1L' to have a more intuitive meaning for
  #   argument 'width', see 'Notes'.
  width <- floor(x = width)
  x <- strwrap(x, width = width + 1L)
  if(n_discard > 0L) {
    x <- x[-(1:n_discard)]
  }

  if(bool_start_blank || (ignore_newlines && bool_start_newline)) {
    x[1L] <- paste0(" ", x[1L])
  }

  n_el <- length(x)
  if(bool_end_newline && !ignore_newlines) {
    x[n_el] <- paste0(x[n_el], "\n")
  }
  if(bool_end_blank || (ignore_newlines && bool_end_newline)) {
    x[n_el] <- paste0(x[n_el], " ")
  }

  bool_too_long <- nchar(x) > width
  if(any(bool_too_long)) {
    warning("Width of ", length(which(bool_too_long)), " text fragments (",
            toString(nchar(x[bool_too_long])), " characters) exceeds 'width' (",
            width, " characters)")
  }
  paste0(x, collapse = "\n")
}
