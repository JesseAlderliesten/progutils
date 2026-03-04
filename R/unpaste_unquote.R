#' Create a character vector from a string with quotation marks
#'
#' @param x A character string
#' @param collapse Character vector with elements that were used to collapse
#' values when `x` was created, and are now used to [split][strsplit()] `x` on.
#' Can be `character(0)` to leave `x` as a character string.
#' @param quotemarks Character vector with quotation marks to be removed from
#' `x`. Can be `character(0)` to not remove any quotation marks.
#'
#' @details
#' `unpaste_unquote()` is not the exact reverse of [paste_quoted()]: it does
#' *not* restore zero-length elements to their original value, e.g., `"'NULL'"`
#' to `NULL`, or `"'character(0)'"` to `character(0)`.
#'
#' @returns
#' `x` without the quotation marks and split into a vector.
#'
#' @seealso
#' [paste_quoted()] for the opposite of `unpaste_unquote()`
#'
#' @family functions to modify character vectors
#'
#' @examples
#' x <- "'ff', 'gG', 'HH'"
#' unpaste_unquote(x = x, collapse = ", ", quotemarks = "'")
#' unpaste_unquote(x = x, collapse = ", ", quotemarks = "\"")
#' unpaste_unquote(x = x, collapse = character(0), quotemarks = c("'", "\""))
#' unpaste_unquote(x = x, collapse = character(0), quotemarks = character(0))
#'
#' @export
unpaste_unquote <- function(x, collapse = c(", ", "; "),
                            quotemarks = c("'", "\"")) {
  stopifnot(is.character(x),
            checkinput::all_characters(collapse, allow_zero = TRUE),
            checkinput::all_characters(quotemarks, allow_zero = TRUE))

  for(mark in quotemarks) {
    x <- gsub(pattern = mark, replacement = "", x = x, fixed = TRUE)
  }
  for(collapse_str in collapse) {
    x <- unlist(strsplit(x = x, split = collapse_str, fixed = TRUE))
  }
  x
}
