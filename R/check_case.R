#' Check for case differences
#'
#' Signal the presence of values that differ only in their case.
#'
#' @inheritParams signal_text signal
#' @param x,y [character vectors][checkinput::all_characters()] to check, see
#' `Value`.
#' @param signal_within as `signal` but indicating the type of
#' [signal][signal_text()] to be used if values within `x` or within `y` differ
#' only in their case.
#'
#' @returns
#' A [character vector][checkinput::all_characters()] with [unique][unique()]
#' values that only differ from other values in their case, or `character(0)` if
#' no such values are present, returned [invisibly][invisible].
#'
#' If `y` is **not** `NULL`: values in `x` that only differ from values in `y`
#' in their case. Such values lead to a [signal][signal_text()] as indicated by
#' argument `signal`. Values within `x` or within `y` only differ in their case
#' lead to a [signal][signal_text()] as indicated by argument `signal_within`.
#'
#' If `y` is `NULL`: values in `x` that only differ from other values in `x` in
#' their case. Such values lead to a [signal][signal_text()] as indicated by
#' argument `signal_within`.
#'
#' @family functions to check equality
#'
#' @seealso
#' [tolower()] and [toupper()] to change case.
#'
#' @examples
#' x <- c("a", "b", "C", "D")
#' y <- c("a", "B", "c", "D")
#' z <- c("a", "b", "C", "D", "A")
#'
#' try(check_case(x = x, y = y, signal = "error")) # an error
#' (check_case(x = x, y = y, signal = "warning")) # c("b", "C"), with a warning
#'
#' (check_case(x = x, y = "h", signal = "warning")) # character(0)
#'
#' (check_case(x = z, y = NULL, signal = "warning", signal_within = "message"))
#' # c("a", "A"), with a message:
#' # 'x' contains values that only differ in their case: 'a', 'A'
#'
#' (check_case(x = z, y = "h", signal = "warning", signal_within = "warning"))
#' # character(0), with a warning:
#' # 'x' contains values that only differ in their case: 'a', 'A'
#' (check_case(x = z, y = "h", signal = "warning", signal_within = "quiet"))
#' # character(0), without a warning
#'
#' (check_case(x = x, y = z, signal = "warning"))
#' # "a", with warnings
#' # 1: 'y' contains values that only differ in their case: 'a', 'A'
#' # 2: 'x' contains values that only differ in their case from values in 'y': 'a'
#'
#' @export
check_case <- function(
    x,
    y = NULL,
    signal = c("error", "warning", "message", "quiet"),
    signal_within = c("warning", "message", "quiet", "error")) {

  stopifnot(is.character(x), is.atomic(x), is.null(dim(x)))
  if(!is.null(y)) {
    stopifnot(is.character(y), is.atomic(y), is.null(dim(y)))
  }

  signal <- match.arg(signal)
  signal_within <- match.arg(signal_within)
  vals <- character(0)

  # Signal if 'x' or 'y' themselves contain values that differ only in their case.
  res_within <- check_case_helper(x, signal = signal_within)
  if(is.null(y)) {
    return(invisible(res_within))
  }
  check_case_helper(y, signal = signal_within)

  # First screening to return early if no matches are present
  if(!any(tolower(x) %in% tolower(y))) {
    invisible(vals)
  }

  # Making values unique and keeping only non-NA values that have case makes it
  # easier to compare values
  x <- unique(x)
  bool_NA_x <- is.na(x)
  if(any(bool_NA_x)) {
    x <- x[!bool_NA_x]
  }
  x <- x[tolower(x) != toupper(x)]

  y <- unique(y)
  bool_NA_y <- is.na(y)
  if(any(bool_NA_y)) {
    y <- y[!bool_NA_y]
  }
  y <- y[tolower(y) != toupper(y)]

  bool_lower_x <- x == tolower(x)
  bool_upper_x <- x == toupper(x)
  bool_lower_y <- y == tolower(y)
  bool_upper_y <- y == toupper(y)

  ind_hit_lower <- which(bool_lower_x)[toupper(x[bool_lower_x]) %in% y[bool_upper_y]]
  ind_hit_upper <- which(bool_upper_x)[tolower(x[bool_upper_x]) %in% y[bool_lower_y]]
  ind_hit <- sort(unique(c(ind_hit_lower, ind_hit_upper)))

  if(length(ind_hit) > 0L) {
    vals <- x[ind_hit]
    signal_text(paste0("'x' contains values that only differ in their case",
                       " from values in 'y':\n", paste_quoted(vals)),
                signal = signal)
  }

  invisible(vals)
}
