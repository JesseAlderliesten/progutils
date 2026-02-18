#' Concatenate x to a string with quoted elements.
#'
#' Concatenate a vector to a string with quoted elements, returning `NULL` as
#' `"'NULL'"` and other zero-length objects as `"'<class>(0)'"`, e.g.,
#' `"'logical(0)'"`.
#'
#' @param x Vector to be converted to a character string.
#'
#' @returns
#' A character string consisting of the elements of `x` surrounded by single
#' quotes, separated by commas. `NULL` is returned as `"'NULL'"`, other
#' zero-length objects are returned as `"'<class>(0)'"`, e.g., `"'logical(0)'"`.
#'
#' @section Notes:
#' An error occurs if multiple arguments are provided because then `x` probably
#' was accidentally not [combined][c()]. For example, the call
#' `paste_quoted("a", "b")` will return the error `unused argument ("b")`. The
#' probably intended call is `paste_quoted(c("a", "b"))`, returning `"'a', 'b'"`.
#'
#' @seealso
#' [sQuote()]; [toString()] which can be used instead of
#' `paste(x, collapse = ", ")`; [paste0()].
#' @family functions to modify character vectors
#'
#' @examples
#' paste_quoted(c(3, 4)) # "'3', '4'"
#' paste_quoted(NULL) # "'NULL'"
#' paste_quoted(c(a = 3, b = 4)) # "'3', '4'" # Warns about dropping names.
#'
#' @export
paste_quoted <- function(x) {
  stopifnot(is.vector(x) || is.factor(x) || is.null(x))

  if(!is.null(names(x))) {
    warning_text <- "'x' has names, these will be discarded."
    if(is.numeric(x)) {
      warning_text <- paste0(warning_text, " Use vect_to_char() instead of",
                             " paste_quoted() to preserve names of numeric 'x'.")
    }
    warning(wrap_text(warning_text))
  }

  if(is.null(x)) {
    x <- "NULL"
  }

  if(length(x) == 0L) {
    x <- paste0(class(x), "(0)")
  }

  # Same as paste0(sQuote(x, q = FALSE), collapse = ", ") but much faster
  paste0("'", paste(x, collapse = "', '"), "'")
}
