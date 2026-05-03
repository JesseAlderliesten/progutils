#' Replace character values or factor levels
#'
#' @param x [Character][character] vector with values to be replaced, or
#' [factor] with [levels] to be replaced.
#' @param old Character vector with values to be replaced. Should be [unique]
#' and not contain the value `new`, otherwise an error is thrown.
#' @param new Character string of length one with the new value.
#' @param ignore_case `TRUE` or `FALSE`: ignore [case][tolower] when looking for
#' `old`? See `Details`.
#' @param allow_multiple `TRUE` or `FALSE`: allow multiple values of `old` to
#' match `x`? See `Details`.
#' @param warn_absent `TRUE` or `FALSE`: warn if `old` nor `new` are found in
#' `x`? It is silently assumed replacement is not necessary anymore if `new` is
#' found.
#' @param signal_case_old,signal_case_new `"error"`, `"warning"`, `"message"`,
#' or `"quiet"`: character string indicating the type of [signal][signal_text()]
#' if values in `x` are a case-insensitive match but not a case-sensitive match
#' to `old` or `new`, respectively. For `signal_case_old`, this is also
#' influenced by argument `signal_old_ignore_case`.
#' @param signal_old_ignore_case `TRUE` or `FALSE`: use `signal_case_old` if
#' `ignore_case` is `TRUE`? If `FALSE`, no signal is emitted if `ignore_case` is
#' `TRUE`.
#' @param quiet `TRUE` or `FALSE`: suppress printing the message with the values
#' that have been replaced?
#'
#' @details
#' Values in `x` that only differ from `new` in their case are not adjusted, but
#' lead to a [signal][signal_text()] as indicated by argument `signal_case_new`.
#'
#' An error is thrown if `allow_multiple` is `FALSE` and multiple values of
#' `old` match `x`, unless those values of `old` only differ in their case and
#' `ignore_case` is `TRUE`. An example of such an exception is
#' `replace_vals(x = c("a", "A"), old = c("a", "A"), new = "b", ignore_case = TRUE, allow_multiple = FALSE)`.
#'
#' If `quiet` is `FALSE`, a message indicates which values have been replaced.
#' The order of the factor *levels* determines the order used in the message.
#'
#' @returns
#' `x` with the requested replacements. Factor levels are *not* reordered after
#' the replacement.
#'
#' @section Programming notes:
#' Rewrite to perform all checks using [check_case], then defer the actual
#' replacement to [gsub()], and then use `which(x_orig != x)` to find and report
#' on replacements?
#'
#' Factor levels can be replaced through a named list:
#' `f <- addNA(as.factor(c(letters[11:13], NA_character_)))`
#' `levels(f) <- list(c = "k", b = "l", a = "m", h = NA_character_)`
#'
#' @family functions to check equality
#' @family functions to modify character vectors
#' @family functions to modify factors
#'
#' @seealso
#' [gsub()] [replace()]
#'
#' @examples
#' x <- c("k", "l", "m")
#'
#' # All values in 'x' that match any value of 'old' are replaced by 'new'.
#' replace_vals(x = rep(x, 2), old = x[3:2], new = "b", allow_multiple = TRUE)
#'
#' # Factor input to 'x' is handled by replacing the values and levels. The
#' # levels are not reordered
#' replace_vals(x = as.factor(x), old = x[3:2], new = "b", allow_multiple = TRUE)
#'
#' # Case-insensitive matching is used if 'ignore_case' is TRUE, with a warning
#' # if 'signal_case_old' is '"warning"'
#' replace_vals(x = "A", old = "a", new = "b", ignore_case = TRUE,
#'              signal_case_old = "warning")
#'
#' # Case-sensitive matching is used if 'ignore_case' is FALSE. A warning is
#' # issued if no match is found and 'warn_absent' is 'TRUE'
#' replace_vals(x = "A", old = "a", new = "b", ignore_case = FALSE,
#'              warn_absent = TRUE)
#'
#' # If 'allow_multiple' is FALSE, an error is thrown if multiple values of
#' # 'old' match 'x'.
#' try(replace_vals(x = x, old = letters[13:12], new = "b",
#'                  allow_multiple = FALSE))
#'
#' @export
replace_vals <- function(
    x, old, new, ignore_case = FALSE, allow_multiple = TRUE, warn_absent = TRUE,
    signal_case_old = c("warning", "error", "message", "quiet"),
    signal_case_new = c("warning", "error", "message", "quiet"),
    signal_old_ignore_case = TRUE, quiet = FALSE) {

  signal_case_old <- match.arg(signal_case_old)
  signal_case_new <- match.arg(signal_case_new)

  stopifnot(is.null(dim(x)), is.character(x) || is.factor(x), length(x) > 0L,
            checkinput::all_characters(old, allow_empty = TRUE, allow_NA = TRUE),
            checkinput::is_character(new, allow_empty = TRUE, allow_NA = TRUE),
            checkinput::is_logical(ignore_case),
            checkinput::is_logical(allow_multiple),
            checkinput::is_logical(warn_absent),
            checkinput::is_logical(signal_old_ignore_case),
            checkinput::is_logical(quiet))

  if(anyDuplicated(old) != 0L) {
    stop("Values in 'old' (", paste_quoted(old), ") should be unique!")
  }

  if(new %in% old) {
    stop("'new' (", paste_quoted(new), ") should not be present in 'old' (",
         paste_quoted(old), ")")
  }

  if(is.factor(x)) {
    x_orig <- x
    levels_repl <- replace_vals(
      x = levels(x), old = old, new = new, ignore_case = ignore_case,
      allow_multiple = allow_multiple, warn_absent = warn_absent,
      signal_case_old = signal_case_old, signal_case_new = signal_case_new,
      quiet = quiet)
    levels(x) <- levels_repl
    if(anyNA(c(levels_repl, new))) {
      # Use addNA(x) because 'NA' levels disappear upon assignment. Call
      # 'unique' to prevent NAs being present multiple times if 'new' is NA.
      x <- reorder_levels(x = addNA(x), new_order = unique(levels_repl),
                          warn_drop_order = FALSE)
    }
    return(x)
  }

  if(signal_case_new != "quiet") {
    bool_case_new <- tolower(x) %in% tolower(new) & !(x %in% new)
    if(any(bool_case_new, na.rm = TRUE)) {
      msg_text_new <- paste0(
        "Values in 'x' are a case-insensitive match but not a case-sensitive",
        " match to 'new' (", paste_quoted(new), "): ",
        paste_quoted(x[bool_case_new]))
      signal_text(text = msg_text_new, signal = signal_case_new)
    }
  }

  if(signal_case_old != "quiet" && (!ignore_case || signal_old_ignore_case)) {
    bool_case_old <- tolower(x) %in% tolower(old) & !(x %in% old)
    if(any(bool_case_old, na.rm = TRUE)) {
      msg_text_old <- paste0(
        "Values in 'x' are a case-insensitive match but not a case-sensitive",
        " match to 'old' (", paste_quoted(old), "): ",
        paste_quoted(x[bool_case_old]))
      signal_text(text = msg_text_old, signal = signal_case_old)
    }
  }

  if(!ignore_case) {
    ind_replace <- which(x %in% old)
  } else {
    bool_replace <- tolower(x) %in% tolower(old)
    ind_replace <- which(bool_replace)

    # Need to check (case-sensitive) inequality of 'x' and 'new' to prevent
    # false-positive matches if a value in tolower(old) is equal to tolower(new),
    # e.g., for replace_vals(x = "A", old = "a", new = "A", ignore_case = TRUE)
    if(tolower(new) %in% tolower(old)) {
      ind_replace <- ind_replace[!(x[ind_replace] %in% new)]
    }
  }

  if(length(ind_replace) > 0L) {
    old_detected <- unique(x[ind_replace])
    if(!ignore_case) {
      old_detected_case <- old_detected
    } else {
      old_detected_case <- unique(tolower(old_detected))
    }

    old_detected_quoted <- paste_quoted(old_detected)
    if(!allow_multiple && length(old_detected_case) > 1L) {
      stop("Multiple values of 'old' (", paste_quoted(old),
           ") matched 'x' (", paste_quoted(x), "): ", old_detected_quoted)
    }
    if(!quiet) {
      message("Replaced values ", old_detected_quoted, " with ", paste_quoted(new))
    }
    x[ind_replace] <- new
  } else {
    if(warn_absent && !(new %in% x)) {
      warning("None of the values of argument 'old' (", paste_quoted(old),
              ") were found in 'x' (", paste_quoted(x), ")!")
    }
  }
  x
}
