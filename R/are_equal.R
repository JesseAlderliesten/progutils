#' Are numbers equal within a tolerance?
#'
#' Test element-wise equality of numeric vectors while allowing for small
#' numeric differences. The tolerance makes `are_equal()` safer than
#' [==][Comparison] when used on numeric vectors.
#'
#' @param x,y Numeric vectors to compare for equality.
#' @param tol A small [positive][checkinput::is_positive()] number. Numbers with
#' absolute differences smaller than `tol` are considered to be equal.
#'
#' @returns A vector with logical values (`TRUE`, `FALSE` or `NA`) indicating if
#' elements in `x` and `y` are equal to each other. [NA] is returned for
#' comparisons involving `NA`s or [NaN]s and for comparisons of
#' [infinite values][Inf] with the same sign.
#'
#' @section Acknowledgement:
#' Code taken from `dplyr::near()`, with added argument checking.
#'
#' @section Programming note:
#' Legacy code contained `near_adj()`, a more-elaborate version of `are_equal()`
#' that also worked on [data.frame]s and [matrices][matrix()] (with checks for
#' compatible [dimensions][dim()]) and contained optional type conversion.
#'
#' @seealso
#' [all.equal()] to compare objects more generally for near-equality,
#' [Comparison] to compare two vectors using binary operators, [match()] to
#' compare character vectors.
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
  stopifnot(checkinput::all_numbers(x), checkinput::all_numbers(y),
            checkinput::is_positive(tol))

  length_x <- length(x)
  length_y <- length(y)
  if(length_x != length_y && length_x != 1L && length_y != 1L) {
    stop("Lengths of 'x' (", length_x, ") and 'y' (", length_y,
         ") are not compatible!")
  }

  abs(x - y) < tol
}
