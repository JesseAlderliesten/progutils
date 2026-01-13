#' Convert a vector to a character string
#'
#' Convert a vector to a character string, preserving names, rounding numeric
#' values, returning `NULL` as `"'NULL'"` and returning other zero-length
#' objects as `"'<class>(0)'"`, e.g., `"'logical(0)'"`.
#'
#' @param x A vector, [factor], non-dataframe [list], or `NULL`.
#' @param signif Positive number of length one, rounded to the nearest positive
#' integer indicating the number of significant digits to round numeric `x` to.
#' @param sep Character string of length one used to separate the names and the
#' values. Not used if `x` does not have names.
#' @param collapse Character string of length one to collapse values into a
#' single character string, or `NULL` to return each value as an element of a
#' character vector.
#' @inheritParams wrap_text
#'
#' @details
#' Numeric `x` is rounded to `signif` digits.
#'
#' `NULL` is returned as `"'NULL'"`, other zero-length objects are returned as
#' `"'<class>(0)'"`, e.g., `"'logical(0)'"`.
#'
#' The output is wrapped to `width` characters if `width` is not `NULL`. This
#' occurs on a per-element basis if a character vector is returned because
#' `collapse` is `NULL`.
#'
#' @returns
#' A character string containing the names and rounded values in `x`, separating
#' name-value pairs by `collapse`. If `collapse` is `NULL`, a character *vector*
#' with the same elements as `x` is returned. The output is wrapped to `width`
#' characters.
#'
#' @section Wishlist:
#' Write a similar function for dataframes or matrices, with the name of the
#' relevant columns only once.
#'
#' Implement customized rounding using `signif_custom()`, e.g.,
#' `x <- signif_custom(x = x, digits = signif, type = type)`.
#'
#' See also:
#' - wrMisc::convToNum()
#' - num_as_char() in https://github.com/hneth/ds4psy/blob/master/R/num_util_fun.R
#' - drop_sci_note() in https://github.com/trinker/textclean/blob/master/R/utils.R
#'
#' @section Programming note:
#' To get a cross-tabulation of `x` into a character string, one could use
#' `paste0(vect_to_char(c(table(z)), sep = " (", collapse =  "), "), ")")`,
#' see the last `Example`.
#'
#' @seealso [toString()] which can be used if names of `x` can be removed.
#' @family functions to modify character vectors
#'
#' @examples
#' x <- 1:3
#' names(x) <- letters[x]
#' vect_to_char(x = x) # "a: 1, b: 2, c: 3"
#' vect_to_char(x = x, collapse = NULL) # c("a: 1", "b: 2", "c: 3")
#' vect_to_char(x = unname(x)) # "1, 2, 3"
#' y <- x / 7
#' vect_to_char(x = y, signif = 7) # "a: 0.1428571, b: 0.2857143, c: 0.4285714"
#' vect_to_char(x = y, signif = 2, sep = " = ", collapse = " and ", width = 15)
#' # "a = 0.14 and b\n= 0.29 and c =\n0.43"
#'
#' x_char <- c(a = "abc", b = "def", c = "this is text")
#' vect_to_char(x = x_char) # "a: abc, b: def, c: this is text"
#' vect_to_char(x = unname(x_char)) # "abc, def, this is some text"
#'
#' # Using vect_to_char() to get a frequency table
#' x <- 1:10
#' names(x) <- letters[x]
#' y <- 5:15
#' names(y) <- letters[y]
#' z <- c(x, y)
#' paste0(vect_to_char(c(table(z)), sep = " (", collapse =  "), "), ")")
#'
#' @export
vect_to_char <- function(x, signif = 3L, sep = ": ", collapse = ", ",
                         width = Inf) {
  stopifnot(checkinput::is_positive(signif), checkinput::is_character(sep),
            is.null(collapse) || checkinput::is_character(collapse),
            checkinput::is_positive(width))

  if(is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, use.names = TRUE)
    warning("Unlisted 'x' to obtain a vector!")
  }
  stopifnot(is.vector(x) || is.factor(x) || is.null(x))

  if(is.numeric(x)) {
    x <- signif(x = x, digits = signif)
  }

  if(is.null(x)) {
    x <- "NULL"
  }

  if(length(x) == 0L) {
    x <- paste0(class(x), "(0)")
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
