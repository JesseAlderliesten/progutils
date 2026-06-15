#' Signal a text to the user
#'
#' Signal a text to the user through an error, warning, or message.
#'
#' @param text Vector with text to be signalled, coerced to character by
#' [vect_to_char()].
#' @param signal Character string indicating the type of signal to be used:
#' `"error"` to throw an [error][stop()], `"warning"` to issue a [warning],
#' `"message"` to show a [message], or `"quiet"` to be quiet.
#' @param origin Character string pasted behind `text` giving the origin of the
#' message. Ignored if it is `character(0)`.
#'
#' @details
#' `text` is coerced to a character vector using [vect_to_char()], which treats
#' zero-length input and input with length larger than one better than
#' [message()] etc. that use [paste0()], see the `Examples`. Call [vect_to_char()]
#' beforehand on `text` to control rounding and wrapping, see the `Examples`.
#'
#' `text` is signalled through an [error][stop()], [warning], [message], or
#' quietly, depending on argument `signal`. To make `text` available for further
#' queries when using `signal_text()` in another function, add the content of the signal as an
#' attribute to the returned object, see the last `Example`.
#'
#' @returns
#' `text`, with a phrase indicating the origin of the signal if `origin` is not
#' `NULL`, returned [invisibly][invisible()].
#'
#' @seealso
#' [wrap_text()]
#'
#' @examples
#' test_text <- c("Some text", "Some other text")
#' try(signal_text(text = test_text, signal = "error"))
#' signal_text(text = test_text, signal = "warn")
#' signal_text(text = test_text, signal = "message")
#' signal_text(text = test_text, signal = "quiet")
#'
#' # signal_text() handles input to 'text' with length larger than one more
#' # sensible than message():
#' test_numbers <- 11:13
#' signal_text(text = test_numbers, signal = "message")
#' message(test_numbers)
#'
#' # Call vect_to_char() beforehand on 'text' to control rounding of numbers and
#' # wrapping of text:
#' message(test_numbers / 7)
#' signal_text(text = test_numbers / 7, signal = "message")
#' signal_text(text = vect_to_char(x = test_numbers / 7, signif = 4, width = 15),
#'             signal = "message")
#'
#' # This example shows how to make the content of the signal available for
#' # further queries:
#' signal_negative <- function(x, signal = c("error", "warning", "message", "quiet")) {
#'   text_signal <- "This is a negative number"
#'   if(x < 0) {
#'     progutils::signal_text(text = text_signal, signal = signal)
#'     attributes(x) <- list(text_signal = text_signal)
#'   } else {
#'     # Using '""' as attribute such that
#'     # grepl(pattern = "<text>", x = attr(x, "text_signal"), fixed = TRUE)
#'     # returns 'FALSE' if <text> is not found in 'text_signal' (using
#'     # 'character(0)' as attribute would lead to 'logical(0)' as return).
#'     attributes(x) <- list(text_signal = "")
#'   }
#'   x
#' }
#'
#' res_negative <- signal_negative(x = -1, signal = "warning")
#' res_positive <- signal_negative(x = 1, signal = "warning")
#' res_negative
#' res_positive
#' grepl(pattern = "negative", x = attr(res_negative, "text_signal"),
#'       ignore.case = FALSE, fixed = TRUE) # TRUE
#' grepl(pattern = "negative", x = attr(res_positive, "text_signal"),
#'       ignore.case = FALSE, fixed = TRUE) # FALSE
#'
#' @export
signal_text <- function(text, signal = c("error", "warning", "message", "quiet"),
                        origin = character(0)) {
  signal <- match.arg(signal)
  text <- vect_to_char(x = text, ignore_newlines = FALSE)
  stopifnot(checkinput::is_character(origin, allow_zerolength = TRUE))

  if(length(origin) > 0L) {
    origin <- paste0("\n< This ", signal, " originated from: ", origin, ". >")
    text <- paste0(text, origin)
  }

  switch(signal,
         "error" = stop(text, call. = FALSE),
         "warning" = warning(text, call. = FALSE),
         "message" = message(text),
         "quiet" = NULL,
         stop("Invalid value for argument 'signal': ", paste_quoted(signal)))

  invisible(text)
}
