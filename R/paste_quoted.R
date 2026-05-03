#' Concatenate x to a string with quoted elements.
#'
#' Concatenate a vector or factor to a string with quoted elements.
#'
#' @details
#' `paste_quoted()` returns `NULL` as `"'NULL'"`, other zero-length objects as
#' `"'<class>(0)'"` (e.g., `"'logical(0)'"`), `""` as `'""'`, and non-logical
#' `NA`s as `"'NA_<class>_'"` (e.g., `"'NA_real_'"`; for [factors][factor] this
#' is `"'NA_character_'"`).
#'
#' @param x Vector or factor to be converted to a character string.
#'
#' @returns
#' A character string consisting of the elements of `x` surrounded by single
#' quotes, separated by commas. See `details` on handling of some special values.
#'
#' @section Notes:
#' An error occurs if multiple arguments are provided because then `x` probably
#' was accidentally not [combined][c()]. For example, the call
#' `paste_quoted("a", "b")` will return the error `unused argument ("b")`. The
#' probably intended call is `paste_quoted(c("a", "b"))`, returning `"'a', 'b'"`.
#'
#' @seealso
#' [toString()] which can be used instead of `paste(x, collapse = ", ")`
#' [sQuote()] [paste0()] [unpaste_unquote()] for the opposite of `paste_quoted()`
#'
#' @family functions to modify character vectors
#'
#' @examples
#' paste_quoted(c(3, 4)) # "'3', '4'"
#' paste_quoted(NULL) # "'NULL'"
#' paste_quoted(c(a = 3, b = 4)) # "'3', '4'" # Warns about dropping names.
#'
#' @export
paste_quoted <- function(x) {
  stopifnot(is.vector(x) || is.factor(x) || is.null(x), !is.list(x))

  if(!is.null(names(x))) {
    warning_text <- "'x' has names, these will be discarded."
    if(is.numeric(x)) {
      warning_text <- paste0(warning_text, " Use vect_to_char() instead of",
                             " paste_quoted() to preserve names of numeric 'x'.")
    }
    warning(wrap_text(warning_text))
  }

  if(is.factor(x)) {
    x <- as.character(x)
  }

  if(length(x) == 0L) {
    if(is.null(x)) {
      x <- "NULL"
    } else {
      x <- paste0(class(x), "(0)")
    }
  }

  bool_NA <- is.na(x) & !is.nan(x)
  if(any(bool_NA)) {
    if(!is.logical(x)) {
      x[bool_NA] <- paste0("NA_", class(x), "_")
    }
  }

  bool_zchar <- !nzchar(x)
  if(any(bool_zchar)) {
    x[bool_zchar] <- "\"\""
  }

  # Same as paste0(sQuote(x, q = FALSE), collapse = ", ") but much faster
  paste0("'", paste(x, collapse = "', '"), "'")
}
