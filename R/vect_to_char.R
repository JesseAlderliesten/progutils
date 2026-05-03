#' Convert a vector to a character string
#'
#' Convert a vector to a character string, preserving names, rounding numeric
#' values.
#'
#' @details
#' `vect_to_char()` returns `NULL` as `"NULL"`, other zero-length objects as
#' `"<class>(0)"` (e.g., `"logical(0)"`), `""` as `'""'`, and non-logical `NA`s
#' as `"NA_<class>_"` (e.g., `"NA_real_"`; for [factors][factor] this is
#' `"NA_character_"`).
#'
#' @param x A vector, [factor], `NULL`, or a non-dataframe [list] (unlisted
#' through `unlist(x, use.names = TRUE)`, with a warning).
#' @param signif Positive number of length one, rounded to the nearest positive
#' integer indicating the number of significant digits to round numeric `x` to.
#' @param sep Character string of length one used to separate the names and the
#' values. Ignored if `x` does not have names.
#' @param collapse Character string of length one to collapse values into a
#' single character string, or `NULL` to return each value as an element of a
#' character vector.
#' @inheritParams wrap_text
#'
#' @returns
#' The names and values in `x`, with values of numeric `x` rounded to `signif`
#' [significant][signif()] digits, wrapped to `width` characters.
#'
#' If `collapse` is `NULL`, a character *vector* with the same elements as `x`
#' is returned, wrapping on a per-element basis. If `collapse` is not `NULL`,
#' the name-value pairs are separated by `collapse`, thus returning a character
#' *string*.
#'
#' See `details` on handling of some special values.
#'
#' @section Programming notes:
#' To get a cross-tabulation of `x` into a character string, one can use
#' `paste0(vect_to_char(c(table(x)), sep = " (", collapse =  "), "), ")")`,
#' see the last `Example`.
#'
#' @seealso [toString()] which can be used if names of `x` can be removed.
#'
#' @family functions to modify character vectors
#' @family functions to modify factors
#'
#' @examples
#' x <- 1:3
#' names(x) <- letters[x]
#' vect_to_char(x = x) # "a: 1, b: 2, c: 3"
#' vect_to_char(x = x, collapse = NULL) # c("a: 1", "b: 2", "c: 3")
#' vect_to_char(x = unname(x)) # "1, 2, 3"
#' y <- x / 7
#' vect_to_char(x = y, signif = 7) # "a: 0.1428571, b: 0.2857143, c: 0.4285714"
#' vect_to_char(x = y, signif = 2, sep = " = ", collapse = " and ", width = 15)
#' # "a = 0.14 and b\n= 0.29 and c =\n0.43"
#'
#' x_char <- c(a = "abc", b = "def", c = "this is text")
#' vect_to_char(x = x_char) # "a: abc, b: def, c: this is text"
#' vect_to_char(x = unname(x_char)) # "abc, def, this is some text"
#'
#' # Using vect_to_char() to get a frequency table
#' x <- 1:10
#' names(x) <- letters[x]
#' y <- 5:15
#' names(y) <- letters[y]
#' x <- c(x, y)
#' paste0(vect_to_char(c(table(x)), sep = " (", collapse =  "), "), ")")
#'
#' @export
vect_to_char <- function(x, signif = 3L, width = Inf, sep = ": ",
                         collapse = ", ", ignore_newlines = TRUE) {
  stopifnot(checkinput::is_positive(signif), checkinput::is_positive(width),
            checkinput::is_character(sep),
            is.null(collapse) || checkinput::is_character(collapse))

  if(is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, use.names = TRUE)
    warning("Unlisted 'x' to obtain a vector!")
  }
  stopifnot(is.vector(x) || is.factor(x) || is.null(x))

  if(is.numeric(x)) {
    x <- signif(x = x, digits = signif)
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
    if(!is.list(x)) {
      if(!is.logical(x)) {
        x[bool_NA] <- paste0("NA_", class(x), "_")
      }
    } else {
      NA_new <- paste0("NA_",
                       vapply(X = x[bool_NA], FUN = class,
                              FUN.VALUE = character(1), USE.NAMES = FALSE),
                       "_")
      NA_new[NA_new == "NA_logical_"] <- "NA"
      x[bool_NA] <- NA_new
    }
  }

  bool_zchar <- !nzchar(x)
  if(any(bool_zchar)) {
    x[bool_zchar] <- "\"\""
  }

  if(!is.null(names(x))) {
    x <- paste(names(x), x, sep = sep, collapse = collapse)
  } else {
    x <- paste(x, collapse = collapse)
  }

  # Not calling wrap_text() directly because it would paste the character vector
  # obtained for 'collapse = NULL' into a single string.
  vapply(X = x, FUN = wrap_text, FUN.VALUE = character(1L), width = width,
         ignore_newlines = ignore_newlines, USE.NAMES = FALSE)
}
