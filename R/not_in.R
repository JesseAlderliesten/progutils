#' Are values not present in `table`
#'
#' Check if values from one vector are not present in another vector
#'
#' @param x Non-numeric vector or factor with values to test absence from
#' `table`. `x` should have a length larger than zero.
#' @param table Non-numeric vector or factor in which to test for absence of `x`.
#' `table` should have a length larger than zero.
#' @param value `TRUE` or `FALSE`: should a vector with values be returned
#' instead of a boolean vector?
#'
#' @details
#' Duplicates in `x` are kept, in contrast to [setdiff()], see the `Examples`.
#'
#' [Factor-input][factor()] to `x` is converted to character, to prevent
#' returning a factor with all values of `x` as [levels].
#'
#' [NA]s are allowed in `x` or `table` and they behave the same as other values:
#' if `NA`s are present in `x` but absent from `table`, `not_in()` returns `NA`s
#' of the same type as those in `x` if `value` is `TRUE`, and returns `TRUE` if
#' `value` is `FALSE`. `NA`s of different types in `x` and `table` match each
#' other, and the returned zero-length value has the same type as `x` if `value`
#' is `TRUE`.
#'
#' @returns
#' If `value` is `TRUE`: the values in `x` that are absent from `table` or, if
#' none of the values in `x` are absent from `table` (i.e., all are present in
#' `table`), a zero-length object of the same type as `x`, e.g., `character(0)`
#' or `logical(0)`. If `value` is `FALSE`: a boolean vector indicating for each
#' element in `x` if it is absent from `table`.
#'
#' @section Programming note:
#' `not_in()` does not allow numeric input because matching numeric input should
#' take small numerical errors into account by using a tolerance, see
#' [are_equal()].
#'
#' `not_in()` does not allow zero-length input because zero-length input behaves
#' slightly different from other values: if `character(0)` is present in `x` but
#' absent from `table`, `not_in()` would return `logical(0)` if `value` is
#' `FALSE`. If `value` is `TRUE`, the behaviour would be normal: returning
#' `character(0)`.
#'
#' Apart from not allowing numeric or zero-length input,
#' `not_in(x, table, value = TRUE)` is equivalent to the unexported function
#' `tools:::'%w/o%'`. Similarly, `not_in(x, table, value = FALSE)` is equivalent
#' to the function `'%notin%'` which is present as exported function in
#' `base-`\R from version `4.6.0` onwards and before that was present as
#' unexported function `tools:::'%notin%'`.
#'
#' @seealso
#' [match()], and, from \R `4.6.0` onwards, `'%notin%'`, on which this function
#' is based; [setdiff()] for a similar function which removes duplicates;
#' [are_equal()] to match numeric input using a tolerance.
#'
#' @examples
#' x <- letters[1:4]
#' table <- letters[3:6]
#' not_in(x, table) # c("a", "b")
#' not_in(as.factor(x), as.factor(table)) # c("a", "b")
#' not_in(x, table, value = FALSE) # c(TRUE, TRUE, FALSE, FALSE), same as !(x %in% table)
#'
#' x_dupl <- c(x, letters[c(2, 4:6, 5)])
#' table_dupl <- letters[c(3:8, 5:7)]
#' not_in(x_dupl, table_dupl) # c("a", "b", "b")
#' setdiff(x_dupl, table_dupl) # c("a", "b")
#' not_in(x_dupl, table_dupl, value = FALSE)
#' # c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
#'
#' @export
not_in <- function(x, table, value = TRUE) {
  stopifnot(is.null(dim(x)), length(x) > 0L, !is.numeric(x),
            is.null(dim(table)), length(table) > 0L, !is.numeric(table),
            checkinput::is_logical(value))
  if(is.factor(x)) {
    x <- as.character(x)
  }

  if(value) {
    x[match(x, table, nomatch = 0L) == 0L]
  } else {
    match(x, table, nomatch = 0L) == 0L
  }
}
