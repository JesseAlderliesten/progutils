#' Wrap text
#'
#' Wrap text at blank characters to achieve a maximum line width.
#'
#' @param x Character vector to be wrapped.
#' @param width A positive number (rounded to a whole number) giving the maximum
#' line width (in characters) after wrapping. Can be `Inf` to not wrap text.
#' @param ignore_newlines `TRUE` or `FALSE`: should newlines in `x` be replaced
#' by a space character? If `FALSE`, newlines are respected, although a newline
#' at the end of `x` is always removed.
#'
#' @returns `x` with newlines inserted at blank characters to wrap `x` to
#' a maximum of `width` characters. `x` of length larger than one is pasted into
#' a single string, separating the parts by spaces. Consecutive white space in
#' `x` is collapsed into a single blank character, except for double spaces
#' after periods, question marks and exclamation marks (as documented in the
#' section `Details` of [strwrap()]). Leading white space in `x` is completely
#' removed.
#'
#' @section Programming note:
#' The call `wrap_text(x, width)` can be replaced by
#' `paste0(strwrap(x, width + 1L), collapse = "\n")` if `x` has length one and
#' `ignore_newlines` is `TRUE`.
#'
#' @note
#' A warning is issued if the width of a fragment in the output exceeds `width`.
#'
#' The output is printed as a string with newlines represented as `\n`. Use
#' [cat()] on the output to print it in the way it is formatted in messages.
#'
#' Argument `width` in `wrap_text()` indicates the maximum width of text after
#' wrapping, i.e., the width *after* which text should be wrapped. In contrast,
#' argument `width` in [strwrap()] indicates the width *at* which text should be
#' wrapped.
#'
#' @seealso [cat()] [paste()] [strwrap()]
#' @family functions to modify character vectors
#'
#' @examples
#' cat(wrap_text("A piece\nof text that you want to wrap over multiple lines",
#'               width = 20, ignore_newlines = TRUE))
#' cat(wrap_text("A piece\nof text that you want to wrap over multiple lines",
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
  # round(Inf) is Inf, whereas as.integer(Inf) is NA.
  width <- round(x = width, digits = 0)
  if(!ignore_newlines) {
    x <- strsplit(x = x, split = "\n")[[1]]
  }
  # Using width + 1L to have a more intuitive meaning for argument 'width'.
  x <- strwrap(x, width = width + 1L)
  bool_too_long <- nchar(x) > width
  if(any(bool_too_long)) {
    warning("Width of ", length(which(bool_too_long)), " text fragments (",
            toString(nchar(x[bool_too_long])), " characters) exceeds 'width' (",
            width, " characters)")
  }
  paste0(x, collapse = "\n")
}
