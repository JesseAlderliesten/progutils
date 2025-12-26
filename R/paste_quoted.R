#' Concatenate text to a single string of quoted elements.
#'
#' Concatenate text to a single string of quoted elements, printing `NULL` and
#' `character(0)` as `"'NULL'"`.
#'
#' @param x Vector to be converted to a single character string.
#' @param ... Not used. Present as a programming tool, see `Details`.
#'
#' @details
#' The use of ellipsis (`...`) as second argument makes it possible to detect if
#' arguments other than `x` are not named. Then a warning is raised because then
#' `x` was probably accidentally not concatenated. For example, the call
#' `paste_quoted("a", "b")` would return `"bab"` if no ellipses were present
#' (using `'b'` as argument `surround`), whereas now `"'a'"` is returned, with a
#' warning that the probably intended call is `paste_quoted(c("a", "b"))`.
#'
#' @returns A character string consisting of the elements of `x` surrounded by
#' quotes (i.e., `'`), separated by commas (i.e., `,`).
#'
#' @section To do:
#' See also [dQuote()], [sQuote()] and [Quotes()]
#'
#' Is there a way to check that arguments are not formal arguments without using
#' the ellipsis argument? See (1) `test_function()` for a more elaborate code to
#' warn about arguments that are not formal arguments of the called function;
#' (2) [chkDots()] with argument `allowed` implemented from \R 4.6.0;
#' (3) [methods::dotsMethods]; (4) functions `check_dots_empty()`,
#' `check_dots_unnamed()`, and `check_dots_used()` in package `ellipsis`;
#' (5a) `chk_used()` and `vld_used()` from package `chk`;
#' (5b) [this StackOverflow post](https://stackoverflow.com/questions/64001629);
#' (6) [this blog post](https://www.rostrum.blog/posts/2024-03-12-eclipse/);
#' (7) ellipsis.txt in my private progutils.
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
#' A warning is issued if `x` has names or if unnamed arguments (other
#' than `x`) are present, see the `Wishlist`.
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
paste_quoted <- function(x, ...) {
  stopifnot(is.vector(x) || is.factor(x) || is.null(x))

  call <- match.call()
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

  ignored_args <- names(call[-c(1)])[!(names(call[-c(1)]) %in%
                                         formalArgs(paste_quoted))]

  if(length(ignored_args) > 0L)  {
    bool_ignored_args_empty <- ignored_args == ""
    text_ignored_args <- character(0)

    if(any(!bool_ignored_args_empty)) {
      text_ignored_args <- paste0(
        "arguments ", paste_quoted(ignored_args[!bool_ignored_args_empty]),
        if(any(bool_ignored_args_empty)) {" and "}
      )
    }

    if(any(bool_ignored_args_empty)) {
      text_ignored_args <- paste0(text_ignored_args,
                                  length(which(bool_ignored_args_empty)),
                                  " unnamed arguments")
    }

    warning(wrap_text(x = paste0(
      "Ignored ", text_ignored_args, " that are not formal arguments of",
      " paste_quoted(). Did you forget to combine 'x', e.g., use a call of the",
      " form 'paste_quoted(a, b)' instead of the intended form",
      " 'paste_quoted(c(a, b))'?")))
  }

  if(is.null(x) || identical(x, character(0))) {
    x <- "NULL"
  }
  paste0("'", paste(x, collapse = "', '"), "'")
}
