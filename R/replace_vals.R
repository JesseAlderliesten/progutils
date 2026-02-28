#' Replace character values or factor levels.
#'
#' @param x Character vector with values to be replaced, or factor with levels
#' to be replaced.
#' @param old Character vector with values to be replaced. Should be unique and
#' not contain the value in `new`.
#' @param new Character string of length one with the new value.
#' @param ignore_case `TRUE` or `FALSE`: ignore case when looking for `old`?
#' @param allow_multiple `TRUE` or `FALSE`: allow multiple values of `old` to
#' match `x`? If `FALSE`, an error is thrown if multiple values of `old` match `x`.
#' @param warn_absent `TRUE` or `FALSE`: warn if `old` nor `new` are found in
#' `x`? If `new` is found, it is silently assumed replacement is not necessary
#' anymore.
#' @param warn_case_new,warn_case_old Character strings indicating when a warning
#' should be issued if values in `x` are a case-insensitive match but not a
#' case-sensitive match to `new` or `old`, respectively. Possible values are
#' `"always"` to always warn, `"ignore_case"` to only warn if `ignore_case` is
#' `TRUE`, `"case_sensitive"` to only warn if `ignore_case` is `FALSE` and
#' `"never"` to never warn.
#' @param quiet `TRUE` or `FALSE`: suppress printing a message with the values
#' that have been replaced?
#'
#' @details
#' `ignore_case` does not affect `new`, e.g.,
#' `replace_vals(x = c("a", "B"), old = "a", new = "b", ignore_case = TRUE)`
#' returns `c("b", "B")`, *not* `c("b", "b")`. Could add an argument to add
#' `tolower(new)` to `old` if `ignore_case` is `TRUE`?
#'
#' If `allow_multiple` is `FALSE`, an error is thrown if multiple values in `old`
#' match `x`. This is useful if `old` should only be replaced with `new` if a
#' single value in `old` occurs in `x`, but not if multiple values of `old`
#' occur in `x`.
#'
#' @returns
#' `x` with the requested replacements.
#'
#' @section Programming notes:
#' Factor levels can be replaced through a named list:
#' `f <- addNA(as.factor(c(letters[11:13], NA_character_)))`
#' `levels(f) <- list(c = "k", b = "l", a = "m", h = NA_character_)`
#'
#' @family functions to modify character vectors
#' @family functions to modify factors
#'
#' @examples
#' # All values in 'x' that match any value of 'old' are replaced by 'new'.
#' replace_vals(x = letters[c(12:13, 13)], old = letters[13:11], new = "b",
#'              allow_multiple = TRUE)
#'
#' # If 'allow_multiple' is FALSE, an error is thrown if multiple values of
#' # 'old' match 'x'.
#' try(replace_vals(x = letters[c(12:13, 13)], old = letters[13:11], new = "b",
#'              allow_multiple = FALSE))
#'
#' # Case-insensitive matching is used if 'ignore_case' is TRUE
#' replace_vals(x = "A", old = "a", new = "b", ignore_case = TRUE)
#'
#' # Case-sensitive matching is used if 'ignore_case' is FALSE
#' # warn_absent = TRUE: warn if no match is found
#' replace_vals(x = "A", old = "a", new = "b", ignore_case = FALSE,
#'              warn_absent = TRUE)
#'
#' # Factor input to 'x'
#' replace_vals(x = as.factor(c("k", "l", "m")), old = c("k", "l"), new = "b",
#'              ignore_case = TRUE, allow_multiple = TRUE, warn_absent = TRUE)
#'
#' replace_vals(x = as.factor(c("m", "l", "k", "M", "L", "K", "m")),
#'              old = c("M", "k"), new = "x", ignore_case = FALSE,
#'              allow_multiple = TRUE)
#'
#' @export
replace_vals <- function(x, old, new, ignore_case = FALSE,
                         allow_multiple = TRUE, warn_absent = TRUE,
                         warn_case_new = c("always", "ignore_case",
                                           "case_sensitive", "never"),
                         warn_case_old = c("case_sensitive", "never",
                                           "always", "ignore_case"),
                         quiet = FALSE) {
  warn_case_new <- match.arg(warn_case_new, several.ok = FALSE)
  warn_case_old <- match.arg(warn_case_old, several.ok = FALSE)
  stopifnot(is.null(dim(x)), is.character(x) || is.factor(x), length(x) > 0L,
            checkinput::all_characters(old, allow_empty = TRUE, allow_NA = TRUE),
            "values in 'old' should be unique" = anyDuplicated(old) == 0L,
            checkinput::is_character(new, allow_empty = TRUE, allow_NA = TRUE),
            checkinput::is_logical(ignore_case),
            checkinput::is_logical(allow_multiple),
            checkinput::is_logical(warn_absent),
            checkinput::is_logical(quiet))

  if(new %in% old) {
    stop("'new' (", paste_quoted(new), ") should not be present in 'old' (",
         paste_quoted(old), ")")
  }

  if(is.factor(x)) {
    x_orig <- x
    levels(x) <- replace_vals(
      x = levels(x), old = old, new = new, ignore_case = ignore_case,
      allow_multiple = allow_multiple, warn_absent = warn_absent,
      warn_case_new = warn_case_new, warn_case_old = warn_case_old,
      quiet = quiet)
    if(anyNA(x)) {
      # No need to warn about adding level 'NA' if it already was the last level.
      if(anyNA(rev(levels(x_orig))[-1])) {
        warning("'NA' will become the last level: use reorder_levels() to fix that!")
      }
      # 'NA' factor levels disappear upon assignment.
      x <- addNA(x)
    }
    return(x)
  }

  if(warn_case_new == "always" || (ignore_case && warn_case_new == "ignore_case") ||
     (!ignore_case && warn_case_new == "case_sensitive")) {
    bool_warn_case <- tolower(x) %in% tolower(new) & !(x %in% new)
    if(any(bool_warn_case)) {
      warning(paste0("Values in 'x' (", paste_quoted(x), ") are a case-insensitive",
                     " match but not a case-sensitive match to 'new' (",
                     paste_quoted(new), "): ", paste_quoted(x[bool_warn_case])))
    }
  }

  if(warn_case_old == "always" || (ignore_case && warn_case_old == "ignore_case") ||
     (!ignore_case && warn_case_old == "case_sensitive")) {
    bool_warn_case <- tolower(x) %in% tolower(old) & !(x %in% old)
    if(any(bool_warn_case)) {
      warning(paste0("Values in 'x' (", paste_quoted(x), ") are a case-insensitive",
                     " match but not a case-sensitive match to 'old' (",
                     paste_quoted(old), "): ", paste_quoted(x[bool_warn_case])))
    }
  }

  if(!ignore_case) {
    ind_replace <- which(x %in% old)
  } else {
    bool_replace <- tolower(x) %in% tolower(old)

    # Need to check (case-sensitive) inequality of 'x' and 'new' to prevent
    # false-positive matches if tolower(old) is equal to tolower(new), e.g., for
    # replace_vals(x = "A", old = "a", new = "A", ignore_case = TRUE) or
    # replace_vals(x = "a", old = "A", new = "a", ignore_case = TRUE)
    if(tolower(new) %in% tolower(old)) {
      bool_replace <- bool_replace & !(x[bool_replace] %in% new[bool_replace])
    }
    ind_replace <- which(bool_replace)
  }

  if(length(ind_replace) > 0L) {
    old_detected <- sort(unique(x[ind_replace]), na.last = TRUE)
    old_detected_quoted <- paste_quoted(old_detected)
    if(!allow_multiple && length(old_detected) > 1L) {
      stop("Multiple matches to 'old' (", paste_quoted(old),
           ") were found in 'x' (", paste_quoted(x), "): ", old_detected_quoted)
    }
    if(!quiet) {
      message("Replaced values ", old_detected_quoted, " with '", new, "'")
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
