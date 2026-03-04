#' Check for values that differ only in their case
#'
#' @inheritParams signal_text
#' @param x Character vector to check.
#'
#' @returns
#' Character vector with [unique()] [sorted][sort()] values in `x` that only
#' differ from each other in their case, or `character(0)` if no such values are
#' present. The return is [invisible].
#'
#' @section Side effects:
#' Values in `x` that only differ from each other in their case generate an
#' [error][stop], [warning], or [message] if `signal` is `error`, `warning`, or
#' `message`, respectively. Values are silently returned if `signal` is `quiet`.
#'
#' @family functions to check equality
#'
#' @examples
#' x <- c("Ee", "Ee", "LL", "Ll")
#' try(check_case(x = x, signal = "error"))
#' check_case(x = x, signal = "warning")
#'
#' @export
check_case <- function(x, signal = c("error", "warning", "message", "quiet")) {
  stopifnot(is.character(x))
  signal <- match.arg(signal)

  vals <- character(0)

  x <- unique(x)
  if(length(x) == length(unique(tolower(x)))) {
    invisible(vals)
  }

  # Removing NAs makes it easier to compare values.
  bool_NA <- is.na(x)
  if(any(bool_NA)) {
    x <- x[!bool_NA]
  }

  bool_lower <- x == tolower(x)
  bool_upper <- x == toupper(x)
  bool_both <- bool_lower & bool_upper
  bool_lower <- bool_lower & !bool_both
  bool_upper <- bool_upper & !bool_both

  x_lower <- x[bool_lower]
  x_upper <- x[bool_upper]
  x_mixed <- x[!bool_lower & !bool_upper & !bool_both]

  bool_lower_other <- x_lower %in% tolower(c(x_upper, x_mixed))
  x_lower_other <- x_lower[bool_lower_other]
  if(any(bool_lower_other)) {
    bool_upper_in_lower <- tolower(x_upper) %in% x_lower_other
    bool_mixed_in_lower <- tolower(x_mixed) %in% x_lower_other
    x_lower_other <- c(x_lower_other, x_upper[bool_upper_in_lower],
                       x_mixed[bool_mixed_in_lower])
    x_upper <- x_upper[!bool_upper_in_lower]
    x_mixed <- x_mixed[!bool_mixed_in_lower]
  }

  bool_upper_other <- x_upper %in% toupper(x_mixed)
  x_upper_other <- x_upper[bool_upper_other]
  if(any(bool_upper_other)) {
    bool_mixed_in_upper <- toupper(x_mixed) %in% x_upper_other
    x_upper_other <- c(x_upper_other, x_mixed[bool_mixed_in_upper])
    x_mixed <- x_mixed[!bool_mixed_in_upper]
  }

  # Duplicated values have been removed from 'x' so any duplicate found is
  # caused by a difference in case
  bool_mixed_diff <- duplicated(tolower(x_mixed))
  x_mixed_diff <- x_mixed[bool_mixed_diff]
  if(any(bool_mixed_diff)) {
    x_mixed_diff <- c(x_mixed_diff, x_mixed[duplicated(tolower(x_mixed),
                                                       fromLast = TRUE)])
  }

  vals <- c(x_lower_other, x_upper_other, x_mixed_diff)

  if(length(vals) > 0L) {
    vals <- sort(vals)
    signal_text(text = paste0("'x' contains values that only differ in their case: ",
                              paste_quoted(vals)),
                signal = signal)
  }
  invisible(vals)
}
