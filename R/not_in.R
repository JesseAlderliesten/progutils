#' Check if values from a vector are not present in another vector
#'
#' @param x Vector with values to look for absence from `y`.
#' @param y Vector in which to look for absence of values `x`.
#' @param value `TRUE` or `FALSE`: should a vector with values be returned
#' instead of a boolean vector?
#'
#' @details
#' Duplicates in `x` are kept, in contrast to [setdiff()], see the `Examples`.
#'
#' [Factor-input][factor()] to `x` is converted to character, to prevent
#' returning a factor with all values of `x` as [levels].
#'
#' [NA]s of different types match each other.
#'
#' @returns
#' If `value` is `TRUE`: the values in `x` that are absent from `y` or, if none
#' of the values in `x` are absent from `y` (i.e., all are present in `y`), a
#' zero-length object of the same type as `x`, e.g., `character(0)` or
#' `logical(0)`. If `value` is `FALSE`: a boolean vector indicating for each
#' element in `x` if it is absent from `y`, i.e., `!(x %in% y)`.
#'
#' @section Programming note:
#' `not_in(x, y, value = FALSE)` gives the same output as function `%notin%`
#' that is present in \R`-devel base` since 05/12/2025 with definition
#' `match(x, table, nomatch = 0L) == 0L`.
#'
#' @seealso [match()] [setdiff()] `%notin%` (from \R 4.6.0 onwards)
#'
#' @examples
#' x <- letters[1:4]
#' y <- letters[3:6]
#' not_in(x, y) # c("a", "b")
#' not_in(as.factor(x), as.factor(y)) # c("a", "b")
#' not_in(x, y, value = FALSE) # c(TRUE, TRUE, FALSE, FALSE), same as !(x %in% y)
#'
#' x_dupl <- c(x, letters[c(2, 4:6, 5)])
#' y_dupl <- letters[c(3:8, 5:7)]
#' not_in(x_dupl, y_dupl) # c("a", "b", "b")
#' setdiff(x_dupl, y_dupl) # c("a", "b")
#' not_in(x_dupl, y_dupl, value = FALSE)
#' # c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
#'
#' @export
not_in <- function(x, y, value = TRUE) {
  stopifnot(is.null(dim(x)), is.null(dim(y)), checkinput::is_logical(value))
  if(is.factor(x)) {
    x <- as.character(x)
  }

  out <- !(x %in% y)
  if(value) {
    out <- x[out]
  }
  out
}
