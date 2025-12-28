#' Convert a numeric vector to a character string
#'
#' Convert a numeric vector to a character string, optionally preserving names
#' and rounding the values.
#'
#' @param x a numeric vector or a non-dataframe list that can be unlisted to
#' obtain a numeric vector.
#' @param signif positive integer of length one indicating the number of
#' significant digits to round to.
#' @param sep Character string of length one used to separate the names and the
#' values.
#' @param collapse character string of length one to collapse values into a
#' single character string, or `NULL` to return each value as an element of a
#' character vector.
#' @inheritParams wrap_text
#'
#' @returns A character string with the values in `x`, preserving names and
#' rounded to `signif` digits.
#'
#' @section Wishlist:
#' Write a similar function for dataframes or matrices, with the name of the
#' relevant columns only once.
#'
#' Implement customized rounding using `signif_custom()`, e.g.,
#' `rounded_vals <- signif_custom(x = x, digits = signif, type = type)`
#'
#' @section Programming note:
#' To get a cross-tabulation of `x` into a character string, one could use
#' `paste0(numvect_to_char(c(table(z)), sep = " (", collapse =  "), "), ")")`,
#' see the last `Example`.
#'
#' @seealso [base::toString()] which can be used as `toString(x)` or
#' `toString(signif_custom(x, ...))` if `x` is unnamed or names can be removed.
#' @family functions to modify character vectors
#'
#' @examples
#' x <- 1:3
#' names(x) <- letters[x]
#' numvect_to_char(x = x) # "a: 1, b: 2, c: 3"
#' numvect_to_char(x = unname(x)) # "1, 2, 3"
#' y <- x
#' y <- y / 7
#' numvect_to_char(x = y, signif = 7) # "a: 0.1428571, b: 0.2857143, c: 0.4285714"
#' numvect_to_char(x = y, signif = 2, sep = " = ", collapse = " and ", width = 15)
#' # "a = 0.14 and b\n= 0.29 and c =\n0.43"
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
  stopifnot(checkinput::is_nonnegative(signif),
            is.null(collapse) || checkinput::is_character(collapse),
            checkinput::is_positive(width))
  if(is.list(x) && !is.data.frame(x)) {
    x <- base::unlist(x, use.names = TRUE)
    warning("Unlisted 'x' to obtain a vector!")
  }
  stopifnot(is.vector(x), is.numeric(x))

  rounded_vals <- signif(x = x, digits = signif)
  if(checkinput::all_characters(x = names(x), allow_empty = TRUE,
                                allow_zero = TRUE, allow_NA = TRUE)) {
    out_char <- paste(names(rounded_vals), rounded_vals, sep = sep,
                      collapse = collapse)
  } else {
    out_char <- paste(rounded_vals, collapse = collapse)
  }
  wrap_text(x = out_char, width = width)
}
