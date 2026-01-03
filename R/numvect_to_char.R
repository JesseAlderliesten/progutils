#' Convert a vector to a character string
#'
#' Convert a vector to a character string, preserving names and rounding numeric
#' values.
#'
#' @param x A vector or a non-dataframe list that can be unlisted to obtain a
#' vector.
#' @param signif Positive number of length one, rounded to the nearest positive
#' integer indicating the number of significant digits to round numeric `x` to.
#' @param sep Character string of length one used to separate the names and the
#' values.
#' @param collapse Character string of length one to collapse values into a
#' single character string, or `NULL` to return each value as an element of a
#' character vector.
#' @inheritParams wrap_text
#'
#' @returns A character string with the values in `x`, preserving names and
#' rounded numeric `x` to `signif` digits, wrapped to `width`.
#'
#' @section Wishlist:
#' Write a similar function for dataframes or matrices, with the name of the
#' relevant columns only once.
#'
#' Implement customized rounding using `signif_custom()`, e.g.,
#' `x <- signif_custom(x = x, digits = signif, type = type)`
#'
#' @section Programming note:
#' To get a cross-tabulation of `x` into a character string, one could use
#' `paste0(numvect_to_char(c(table(z)), sep = " (", collapse =  "), "), ")")`,
#' see the last `Example`.
#'
#' @section To do:
#' Rename to `vect_to_char()`.
#'
#' @seealso [base::toString()] which can be used as `toString(x)` or
#' `toString(signif_custom(x, ...))` if `x` is unnamed or names can be removed.
#' @family functions to modify character vectors
#'
#' @examples
#' x <- 1:3
#' names(x) <- letters[x]
#' numvect_to_char(x = x) # "a: 1, b: 2, c: 3"
#' numvect_to_char(x = x, collapse = NULL) # c("a: 1", "b: 2", "c: 3")
#' numvect_to_char(x = unname(x)) # "1, 2, 3"
#' y <- x / 7
#' numvect_to_char(x = y, signif = 7) # "a: 0.1428571, b: 0.2857143, c: 0.4285714"
#' numvect_to_char(x = y, signif = 2, sep = " = ", collapse = " and ", width = 15)
#' # "a = 0.14 and b\n= 0.29 and c =\n0.43"
#'
#' x_char <- c(a = "abc", b = "def", c = "this is text")
#' numvect_to_char(x = x_char) # "a: abc, b: def, c: this is text"
#' numvect_to_char(x = unname(x_char)) # "abc, def, this is some text"
#'
#' # Showing the use of numvect_to_char to get a frequency table
#' x <- 1:10
#' names(x) <- letters[x]
#' y <- 5:15
#' names(y) <- letters[y]
#' z <- c(x, y)
#' paste0(numvect_to_char(c(table(z)), sep = " (", collapse =  "), "), ")")
#'
#' @export
numvect_to_char <- function(x, signif = 3L, sep = ": ", collapse = ", ",
                            width = Inf) {
  stopifnot(checkinput::is_positive(signif), checkinput::is_character(sep),
            is.null(collapse) || checkinput::is_character(collapse),
            checkinput::is_positive(width))
  if(is.list(x) && !is.data.frame(x)) {
    x <- base::unlist(x, use.names = TRUE)
    warning("Unlisted 'x' to obtain a vector!")
  }
  stopifnot(is.vector(x))

  if(is.numeric(x)) {
    x <- signif(x = x, digits = signif)
  }

  if(checkinput::all_characters(x = names(x), allow_empty = TRUE,
                                allow_zero = TRUE, allow_NA = TRUE)) {
    x <- paste(names(x), x, sep = sep, collapse = collapse)
  } else {
    x <- paste(x, collapse = collapse)
  }

  # Not calling wrap_text() directly because it would paste the character vector
  # obtained for 'collapse = NULL' into a single string.
  vapply(X = x, FUN = wrap_text, FUN.VALUE = character(1L), width = width,
         USE.NAMES = FALSE)
}
