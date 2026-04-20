#' Reorder columns
#'
#' @param x Object with at least one column. Columns should have unique,
#' syntactically valid names, see [checkinput::all_names()].
#' @param new_order Character vector with a length larger than zero containing
#' unique names in the new order of the columns.
#'
#' @details
#' Column names of `x` that are missing from `new_order` are appended to
#' `new_order`, with a warning.
#'
#' Values in `new_order` that are missing from column names of `x` are dropped,
#' with a warning.
#'
#' @returns
#' `x` with reordered columns.
#'
#' @seealso
#' [order()] [reorder_levels] [sort()]
#'
#' @family functions to modify character vectors
#'
#' @examples
#' test_df <- data.frame(a = 1:2, b = 11:12, c = 21:22)
#' reorder_cols(x = test_df, new_order = c("b", "a", "c"))
#'
#' # reorder_cols() appends column names that are missing from 'new_order' and
#' # drops values from 'new_order' that are missing from column names, both with
#' # a warning.
#' reorder_cols(x = test_df, new_order = c("b", "a", "d"))
#'
#' @export
reorder_cols <- function(x, new_order) {
  stopifnot(
    "'x' should have columns" = !is.null(dim(x)),
    "'x' should have at least one column" = ncol(x) > 0L,
    checkinput::all_characters(new_order, allow_empty = TRUE, allow_NA = TRUE),
    "Values in 'new_order' should be unique" = anyDuplicated(new_order) == 0L)

  if(is.null(colnames(x))) {
    stop("'x' should have column names!")
  }

  if(any(is.na(colnames(x))) ||
     any(colnames(x) != make.names(colnames(x), unique = TRUE))) {
    checkinput::all_names(colnames(x))
    stop("Column names of 'x' should be unique and syntactically valid!")
  }

  original_order <- colnames(x)
  bool_present <- original_order %in% new_order
  if(any(!bool_present)) {
    new_order <- c(new_order, original_order[!bool_present])
    warning("Appended columns that are present in 'x' but missing from 'new_order':\n",
            paste_quoted(original_order[!bool_present]))
  }

  bool_order_in_names <- new_order %in% colnames(x)
  if(any(!bool_order_in_names)) {
    warning("Dropped values of 'new_order' that are not present in column",
            " names of 'x':\n", paste_quoted(new_order[!bool_order_in_names]))
  }

  x[, new_order[bool_order_in_names], drop = FALSE]
}
