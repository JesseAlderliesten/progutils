#' Are numeric vectors nearly equal
#'
#' Test element-wise near-equality of numeric vectors by allowing for small
#' numeric errors to make `are_equal()` safer than [==][Comparison].
#'
#' @param x,y Numeric vectors to compare for equality.
#' @param tol A small [positive][checkinput::is_positive()] number. Numbers that
#' differ less in value than `tol` are considered to be equal.
#'
#' @returns A vector with logical values (`TRUE`, `FALSE` or `NA`) indicating if
#' elements in `x` and `y` are equal to each other. [NA] is returned for
#' comparisons involving numeric `NA`s (i.e., `NA_integer_` and `NA_real_`),
#' [NaN]s, or [infinite values][Inf] with the same sign.
#'
#' @section Acknowledgement:
#' Code `abs(x - y) < tol` was taken from `dplyr::near()`.
#'
#' @seealso
#' [all.equal()] to check more generally for near-equality; [identical()] to
#' check for exact equality; [Comparison] to compare two vectors using binary
#' operators; [match()] to compare non-numeric vectors; [\R FAQ 7.31](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#' for background on numerical equality; the vignette about type coercion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#'
#' @examples
#' x <- sqrt(2)^2
#' x == 2 # FALSE
#' x - 2 # about 4.44e-16
#' are_equal(x = x, y = 2) # TRUE
#'
#' are_equal(x = c(2, 3, 3,         NA, Inf),
#'           y = c(2, 3, 3 + 1e-8, NA, Inf))
#' are_equal(x = 3, y = c(2, 3, 3 + 1e-8, NA, Inf))
#'
#' @export
are_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  stopifnot(checkinput::all_numbers(x, allow_NA = TRUE, allow_NaN = TRUE),
            checkinput::all_numbers(y, allow_NA = TRUE, allow_NaN = TRUE),
            checkinput::is_positive(tol))

  length_x <- length(x)
  length_y <- length(y)
  if(length_x != length_y && length_x != 1L && length_y != 1L) {
    stop("Lengths of 'x' (", length_x, ") and 'y' (", length_y,
         ") are not compatible!")
  }

  abs(x - y) < tol
}
