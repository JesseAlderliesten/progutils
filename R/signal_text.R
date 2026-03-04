#' Signal a text to the user
#'
#' Signal a text to the user through an error, warning, or message.
#'
#' @param text Vector with text to be signalled, coerced to character by
#' [vect_to_char()].
#' @param signal Character string indicating the type of signal to be used:
#' `"error"` to throw an [error][stop], `"warning"` to issue a [warning],
#' `"message"` to show a [message], or `"quiet"` to be quiet.
#' @param origin Character string pasted behind `text` giving the origin of the
#' message. Ignored if it is `character(0)`.
#'
#' @details
#' `text` is coerced to a character vector using [vect_to_char()], which treats
#' input with length larger than one better than [message] etc. that use
#' [paste0()], see the `Examples`. Call [vect_to_char()] beforehand on `text` to
#' control rounding and wrapping.
#'
#' @returns
#' `text`, returned [invisibly][invisible].
#'
#' @section Side effects:
#' `text` is signalled through an [error][stop], [warning], [message], or
#' quietly, depending on argument `signal`.
#'
#' @seealso
#' [wrap_text()]
#'
#' @examples
#' test_text <- c("Some text", "Some other text")
#' try(signal_text(text = test_text, signal = "error"))
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
#' @export
signal_text <- function(text, signal = c("error", "warning", "message", "quiet"),
                        origin = character(0)) {
  signal <- match.arg(signal)
  text <- vect_to_char(x = text, ignore_newlines = FALSE)
  stopifnot(checkinput::is_character(origin, allow_zero = TRUE))

  if(length(origin) > 0L) {
    origin <- paste0("\n< This ", signal, " originated from: ", origin, ". >")
    text <- paste0(text, origin)
  }

  switch(signal,
         "error" = stop(text, call. = FALSE),
         "warning" = warning(text, call. = FALSE),
         "message" = message(text),
         "quiet" = NULL,
         stop(paste0("Invalid value for argument 'signal': ",
                     paste_quoted(signal))))

  invisible(text)
}
