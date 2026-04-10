#' Reorder factor levels
#'
#' @param x Factor with levels to be reordered to `new_order`, or character
#' vector to be converted to a factor with levels ordered as `new_order`. Should
#' have length larger than zero.
#' @param new_order Unique character vector with a length larger than zero
#' indicating the new order of the factor levels.
#' @param warn_drop_order `TRUE` or `FALSE`: warn if values of `new_order` are
#' dropped because they are not present in `x`?
#'
#' @details
#' Character input to `x` is silently converted to a [factor] before reordering
#' the factor levels.
#'
#' Values of factor `x` that are not present in its levels are added to its
#' levels, with a warning.
#'
#' Levels of factor `x` that are not present in its values are dropped, with a
#' warning.
#'
#' Levels of `x` that are missing from `new_order` are appended to `new_order`,
#' with a warning.
#'
#' Values in `new_order` that are missing from levels of `x` are dropped, with a
#' warning if `warn_drop_order` is `TRUE`.
#'
#' @returns
#' `x` after converting it to a [factor] with its levels reordered to
#' `new_order`, dropping values in `new_order` not present in `x`.
#'
#' @section Notes:
#' Reordering levels of factor `f` by replacing levels through code like
#' `levels(f) <- levels(f)[<some order>]` does *not* work, see the last `Example`.
#'
#' @section Programming notes:
#' Could also use an approach like (see [levels()]):
#' `levels(x) <- list("Yes" = c("Y", "Yes"), "No" = c("N", "No"))`
#'
#' @seealso
#' [reorder_cols] [stats::relevel()] to assign one reference level to a factor
#'
#' @family functions to modify factors
#'
#' @examples
#' orig <- factor(letters[c(12:13, 13:11)], levels = letters[13:11])
#' orig
#' reorder_levels(x = orig, new_order = letters[11:13])
#'
#' # Changing the levels directly does *not* work because it changes the values
#' levels(orig) <- letters[11:13]
#' orig
#'
#' @export
reorder_levels <- function(x, new_order, warn_drop_order = TRUE) {
  stopifnot(is.factor(x) || is.character(x), length(x) > 0L,
            checkinput::all_characters(new_order, allow_empty = TRUE,
                                       allow_NA = TRUE),
            "Values in 'new_order' should be unique" = anyDuplicated(new_order) == 0L,
            checkinput::is_logical(warn_drop_order))

  if(is.character(x)) {
    x <- factor(x, exclude = NULL)
  } else {
    x_dropped <- droplevels(x, exclude = NULL)
    dropped_levels <- not_in(levels(x), levels(x_dropped), value = TRUE)
    added_levels <- not_in(levels(x_dropped), levels(x), value = TRUE)

    # NA has been added to the levels of 'x' if NA is present in 'x' but was not
    # present in its levels
    x <- x_dropped
    if(length(dropped_levels) > 0L) {
      warning(wrap_text(x = paste0(
        "Dropped levels of 'x' that are not present in its values: ",
        paste_quoted(dropped_levels)
      )))
    }
    if(length(added_levels) > 0L) {
      warning(wrap_text(x = paste0(
        "Added values of 'x' that were not present in its levels to its levels: ",
        paste_quoted(added_levels)
      )))
    }
  }

  bool_levels_in_new_order <- levels(x) %in% new_order
  if(any(!bool_levels_in_new_order)) {
    levels_appended <- levels(x)[!bool_levels_in_new_order]
    new_order <- c(new_order, levels_appended)
    warning(wrap_text(paste0(
      "Appended levels of 'x' that were not present in 'new_order' to 'new_order': ",
      paste_quoted(levels_appended))))
  }

  bool_order_in_levels <- new_order %in% levels(x)
  if(warn_drop_order && any(!bool_order_in_levels)) {
    warning(wrap_text(paste0(
      "Dropped values of 'new_order' that are not present in 'x': ",
      paste_quoted(new_order[!bool_order_in_levels]))))
  }
  factor(x, levels = new_order[bool_order_in_levels], exclude = NULL)
}
