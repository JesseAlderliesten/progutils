#' Round levels
#'
#' Round numeric values of factor levels, e.g., to create facet labels that fit
#' better.
#'
#' @param x [factor] or [character] vector containing
#' [numerish values][as.numeric_safe()] to be rounded, see `Details`.
#' @param level_order `NULL` to sort levels on increasing numerical value, or a
#' a vector indicating the desired order of the factor [levels].
#' @inheritParams signif_custom digits type
#'
#' @returns
#' `x` with rounded factor levels.
#'
#' @details
#' Arguments `digits` and `type` are passed to [signif_custom()].
#'
#' Levels of `x` that are not present in its values will be silently dropped.
#'
#' Values in `x` that do not occur in `level_order` will be added to
#' `level_order`, with a warning. Values in `level_order` that do not occur in
#' `x` will be dropped, with a warning.
#'
#' @family functions to check equality
#' @family functions to modify factors
#'
#' @examples
#' round_levels(x = factor(c(2, 2.2, 2.8)), level_order = c("2.2", "2.8", "2"))
#' round_levels(x = factor(c(2, 2.2, 2.8)), level_order = c("2.2", "2.8", "2.0"))
#' round_levels(x = factor(c(2, 2.2, 2.8)), level_order = NULL)
#'
#' @export
round_levels <- function(x, level_order = NULL, digits = 4L,
                         type = c("selective", "expanded")) {
  stopifnot(is.null(dim(x)), !is.list(x),
            is.null(level_order) || is.vector(level_order),
            !is.list(level_order))
  if(!is.numeric(x)) {
    x <- as.numeric_safe(x)
  }
  if(is.null(level_order)) {
    level_order <- sort(unique(
      signif_custom(x = x, digits = digits, type = type)
    ), na.last = TRUE)
  } else {
    level_order <- signif_custom(x = as.numeric_safe(level_order),
                                 digits = digits, type = type)
  }
  reorder_levels(x = as.factor(signif_custom(x = x, digits = digits, type = type)),
                 new_order = as.character(level_order))
}
