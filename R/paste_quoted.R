#' Concatenate text to a single string of quoted elements.
#'
#' Concatenate text to a single string of quoted elements, printing `NULL` and
#' `character(0)` as `"'NULL'"`.
#'
#' @param x Vector to be converted to a single character string.
#'
#' @returns A character string consisting of the elements of `x` surrounded by
#' quotes (i.e., `'`), separated by commas (i.e., `,`).
#'
#' @section To do:
#' See also [dQuote()], [sQuote()] and [Quotes()]
#'
#' @section Wishlist: Implement preserving names, e.g., using
#' `paste(names(x), x, sep = ": ", collapse = ", ")` (used in [numvect_to_char()])
#' or `paste0("'", paste(names(x), x, sep = ": ", collapse = "', '"), "'")`. Add
#' argument 'use_names = c("numeric", "all", "none") to use that?
#'
#' @note
#' `NULL` and `character(0)` are printed as `"'NULL'"`. Other zero-length
#' objects are printed as `"''"`.
#'
#' An error occurs if multiple arguments are provided. Then `x` was probably
#' accidentally not concatenated. For example, the call `paste_quoted("a", "b")`
#' will return the error `unused argument ("b")`. The probably intended call is
#' `paste_quoted(c("a", "b"))`.
#'
#' @seealso [paste0()] [toString()] which can be used instead of
#' `paste(x, collapse = ", ")`
#' @family functions to modify character vectors
#'
#' @examples
#' paste_quoted(c(3, 4)) # "'3', '4'"
#' paste_quoted(NULL) # "'NULL'"
#' paste_quoted(c(a = 3, b = 4)) # "'3', '4'" # Warns about dropping names.
#'
#' @export
paste_quoted <- function(x) {
  stopifnot(is.vector(x) || is.factor(x) || is.null(x))

  if(is.factor(x)) {
    warning("'x' is a factor and will be converted to numeric.")
    x <- as.numeric_safe(x)
  }

  if(!is.null(names(x))) {
    warning_text <- "'x' has names, these will be discarded."
    if(is.numeric(x)) {
      warning_text <- paste0(warning_text, " Use numvect_to_char() instead of",
                             " paste_quoted() to preserve names of numeric 'x'.")
    }
    warning(wrap_text(warning_text))
  }

  if(is.null(x) || identical(x, character(0))) {
    x <- "NULL"
  }
  paste0("'", paste(x, collapse = "', '"), "'")
}
