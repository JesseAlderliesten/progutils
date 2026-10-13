#' Check for values that differ only in their case
#'
#' @inheritParams signal_text signal
#' @param x Character vector to check.
#'
#' @details
#' Values in `x` that only differ from each other in their case lead to a
#' [signal][signal_text()] as indicated by argument `signal`.
#'
#' @returns
#' A [character vector][checkinput::all_characters()] with [unique][unique()]
#' values in `x` that only differ from each other in their case, or
#' `character(0)` if no such values are present, returned [invisibly][invisible].
#'
#' @seealso
#' [check_case()] to check between two vectors for values that only differ in
#' their case; [tolower()] and [toupper()] to change case.
#'
#' @examples
#' x <- c("Ee", "Ee", "LL", "Ll")
#' try(progutils:::check_case_helper(x = x, signal = "error"))
#' progutils:::check_case_helper(x = x, signal = "warning")
#'
#' @keywords internal
check_case_helper <- function(x, signal = c("error", "warning", "message", "quiet")) {
  stopifnot(checkinput::all_characters(
    x, allow_empty = TRUE, allow_zerolength = TRUE, allow_NA = TRUE))

  x_deparsed <- deparse1(substitute(x))
  x_name <- paste_quoted(x_deparsed)

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

  (tolower(x) %in% tolower(x)) & !(x %in% x)

  bool_anylower <- x == tolower(x)
  bool_anyupper <- x == toupper(x)

  # The combination x[bool_anylower & bool_anyupper] would identify values that
  # cannot occur in different case, so does not have to be used.
  x_lower <- x[bool_anylower & !bool_anyupper]
  x_upper <- x[bool_anyupper & !bool_anylower]
  x_mixed <- x[!bool_anylower & !bool_anyupper]

  x_lower_other <- x_lower[x_lower %in% tolower(c(x_upper, x_mixed))]
  if(length(x_lower_other) > 0L) {
    bool_upper_in_lower <- tolower(x_upper) %in% x_lower_other
    bool_mixed_in_lower <- tolower(x_mixed) %in% x_lower_other
    x_lower_other <- c(x_lower_other, x_upper[bool_upper_in_lower],
                       x_mixed[bool_mixed_in_lower])
    x_upper <- x_upper[!bool_upper_in_lower]
    x_mixed <- x_mixed[!bool_mixed_in_lower]
  }

  x_upper_other <- x_upper[x_upper %in% toupper(x_mixed)]
  if(length(x_upper_other) > 0L) {
    bool_mixed_in_upper <- toupper(x_mixed) %in% x_upper_other
    x_upper_other <- c(x_upper_other, x_mixed[bool_mixed_in_upper])
    x_mixed <- x_mixed[!bool_mixed_in_upper]
  }

  # Duplicated values have been removed from 'x' so any duplicate found is
  # caused by a difference in case
  x_mixed_diff <- x_mixed[duplicated(tolower(x_mixed))]
  if(length(x_mixed_diff) > 0L) {
    x_mixed_diff <- c(x_mixed_diff,
                      x_mixed[duplicated(tolower(x_mixed), fromLast = TRUE)])
  }

  vals <- c(x_lower_other, x_upper_other, x_mixed_diff)

  if(length(vals) > 0L) {
    vals <- vals[order(match(vals, x))]
    signal_text(paste0(x_name, " contains values that only differ in their case: ",
                       paste_quoted(vals)),
                signal = signal)
  }
  invisible(vals)
}
