#' Split a character string into a vector, removing quotation marks
#'
#' @param x a [character string][checkinput::is_character()].
#' @param collapse [character vector][checkinput::all_characters()] with
#' elements that were used to collapse
#' values when `x` was created, and are now used to [split][strsplit()] `x` on.
#' Can be `character(0)` to leave `x` as a character string.
#' @param quotemarks [character vector][checkinput::all_characters()] with
#' quotation marks to be removed from `x`. Can be `character(0)` to not remove
#' any quotation marks.
#'
#' @details
#' `unpaste_unquote()` does **not** restore `NA`s or zero-length elements to
#' their original values after removing the quotation marks, such that
#' `unpaste_unquote()` is **not** the exact reverse of [paste_quoted()]. For
#' example, `"'NA_character_'"` becomes `"NA_character_"` instead of
#' `NA_character_`, `"'NULL'"` becomes `"NULL"` instead of `NULL`, and
#' `"'character(0)'"` becomes `"character(0)"` instead of `character(0)`.
#'
#' @returns
#' `x` without the quotation marks in `quotemarks`, split into a vector on the
#' string in `collapse`.
#'
#' @seealso
#' [paste_quoted()] for the approximate opposite of `unpaste_unquote()`.
#'
#' @family functions to modify character vectors
#'
#' @examples
#' x <- paste_quoted(c("ff", "gG", "HH"))
#' x
#' unpaste_unquote(x = x, collapse = ", ", quotemarks = "'")
#' unpaste_unquote(x = x, collapse = ", ", quotemarks = "\"")
#' unpaste_unquote(x = x, collapse = character(0), quotemarks = c("'", "\""))
#' unpaste_unquote(x = x, collapse = character(0), quotemarks = character(0))
#'
#' @export
unpaste_unquote <- function(x, collapse = c(", ", "; "),
                            quotemarks = c("'", "\"")) {
  stopifnot(is.character(x),
            checkinput::all_characters(collapse, allow_zerolength = TRUE),
            checkinput::all_characters(quotemarks, allow_zerolength = TRUE))

  for(mark in quotemarks) {
    x <- gsub(pattern = mark, replacement = "", x = x, fixed = TRUE)
  }
  for(collapse_str in collapse) {
    x <- unlist(strsplit(x = x, split = collapse_str, fixed = TRUE))
  }
  x
}
