#' Are values not present in `table`
#'
#' Check if values from one vector are not present in another vector
#'
#' @param x Vector or factor with values to test absence from `table`. `x`
#' should have a length larger than zero and not be of [type][typeof] `double`.
#' @param table Vector or factor in which to test for absence of `x`. `table`
#' should have a length larger than zero and not be of `type` `double`.
#' @param value `TRUE` or `FALSE`: should a vector with values be returned
#' instead of a boolean vector?
#'
#' @details
#' Duplicates in `x` are kept, in contrast to [setdiff()], see the `Examples`.
#'
#' [Factor-input][factor()] to `x` is converted to character, to prevent
#' returning a factor with all values of `x` as [levels].
#'
#' [NA]s are allowed in `x` and `table` and behave the same as other values: the
#' returned `NA`s (if `value` is `TRUE`) and the returned zero-length value (if
#' `value` is `FALSE`) have the same type as the `NA`s in `x` if no `NA`s are
#' present in `table`. `NA`s of different types in `x` and `table` match each
#' other.
#'
#' @returns
#' If `value` is `TRUE`: the values in `x` that are absent from `table` or, if
#' none of the values in `x` are absent from `table` (i.e., all are present in
#' `table`), a zero-length object of the same type as `x`, e.g., `character(0)`
#' or `logical(0)`. If `value` is `FALSE`: a boolean vector indicating for each
#' element in `x` if it is absent from `table`.
#'
#' @section Programming notes:
#' `not_in()` does not allow input of [type][typeof] `double` because matching
#' such input should take small numerical errors into account by using a
#' tolerance, for example, as the error message indicates, using [are_equal()].
#'
#' `not_in()` does not allow zero-length input because zero-length input behaves
#' slightly different from other values: if `character(0)` is present in `x` but
#' absent from `table`, `not_in()` would return `logical(0)` if `value` is
#' `FALSE`. If `value` is `TRUE`, the behaviour would be normal: returning
#' `character(0)`.
#'
#' Apart from not allowing numeric or zero-length input,
#' `not_in(x, table, value = TRUE)` is equivalent to `x %w/o% table`, where
#' `%w/o%` is an unexported function from `tools`: `tools:::'%w/o%'`. Similarly,
#' `not_in(x, table, value = FALSE)` is equivalent to `x %notin% table`, where
#' `%notin%` is an exported function from `base-`\R since version `4.6.0` which
#' before that was present as unexported function `tools:::'%notin%'`.
#'
#' @seealso
#' [setdiff()] for a similar function which removes duplicates; [are_equal()] to
#' match numeric input using a tolerance; [match()], and, from \R `4.6.0`
#' onwards, `'%notin%'`, on which this function is based.
#'
#' @examples
#' x <- letters[1:4]
#' table <- letters[3:6]
#' not_in(x, table) # c("a", "b")
#' not_in(as.factor(x), as.factor(table)) # c("a", "b")
#' # c(TRUE, TRUE, FALSE, FALSE), same as !(x %in% table):
#' not_in(x, table, value = FALSE)
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
  stopifnot(is.null(dim(x)), length(x) > 0L,
            "Use are_equal() to match input of type 'double'" = !is.double(x),
            is.null(dim(table)), length(table) > 0L,
            "Use are_equal() to match input of type 'double'" = !is.double(table),
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
