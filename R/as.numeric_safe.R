#' Convert x to numeric
#'
#' Convert `x` to a vector with [numeric][as.numeric()] [mode] in a way that
#' works if `x` is a [factor], a character vector, or already a numeric vector.
#'
#' @param x A factor, or a character or numeric vector to be converted to a
#' numeric vector.
#' @param keep_integer `TRUE` or `FALSE`: return input of [type][typeof] integer
#' without converting it to double.
#'
#' @returns A numeric vector, possibly `numeric(0)` or containing `NA_real_`.
#'
#' @details
#' The [type][typeof] of [integers][integer()] is kept `integer` if
#' `keep_integer` is `TRUE` and is changed to `double` if `keep_integer` is
#' `FALSE`.
#'
#' `NULL` and zero-length vectors are converted to `numeric(0)`, except for
#' `integer(0)` that is converted to `integer(0)` if `keep_integer` is `TRUE`.
#' Logical vectors
#' of length larger than zero are converted to a vector of `NA_real_`, with a
#' warning.
#'
#' `as.numeric_safe()` uses a suggestion from [factor()] and
#' [\R FAQ 7.10](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f)
#' that works if `x` is a factor, a character vector or already is a numeric
#' vector (even though the last example of `as.numeric()` comments that this
#' approach is less efficient for long vectors).
#'
#' In contrast, the alternative conversions `as.numeric(levels(x))[x]` (suggested
#' in the `Warning` of [factor()]) and `as.numeric(levels(x))[as.integer(x)]`
#' (suggested in \R `FAQ 7.10` linked to above) **only** work if `x` is factor,
#' see the `Examples`.
#'
#' Furthermore, `as.numeric()` **cannot** be used to convert a factor to numeric,
#' because it returns their underlying numeric integer representation (i.e., the
#' indices of the factor levels), see the `Warnings` in [as.numeric()] and
#' [factor()].
#'
#' @seealso
#' [formatC()] and [option][options()] `scipen` on formatting numbers,
#' [utils::type.convert()]
#'
#' @family functions to convert types
#' @family functions to modify character vectors
#' @family functions to modify factors
#'
#' @examples
#' x_int <- 5:7
#' x_num <- as.numeric(x_int)
#' x_char <- as.character(x_int)
#' x_fact <- as.factor(x_int)
#'
#' str(as.numeric_safe(x_int, keep_integer = TRUE))
#' str(as.numeric_safe(x_int, keep_integer = FALSE))
#'
#' str_as_num_safe <- function(x) {str(as.numeric_safe(x))}
#' str_as_num_fact <- function(x) {str(as.numeric(levels(x))[x])}
#' str_as_num_7.10 <- function(x) {str(as.numeric(levels(x))[as.integer(x)])}
#' str_as_num_base <- function(x) {str(as.numeric(x))}
#'
#' # as.numeric_safe() works irrespective the type of x
#' str_as_num_safe(x_int)  # correct
#' str_as_num_safe(x_num)  # correct
#' str_as_num_safe(x_char) # correct
#' str_as_num_safe(x_fact) # correct
#'
#' # The 'more efficient' suggestions in `help(factor)` and
#' # R FAQ 7.10 *only* work for factors.
#' str_as_num_fact(x_int)  # clearly wrong
#' str_as_num_7.10(x_int)  # clearly wrong
#' str_as_num_fact(x_num)  # clearly wrong
#' str_as_num_7.10(x_num)  # clearly wrong
#' str_as_num_fact(x_char) # clearly wrong
#' str_as_num_7.10(x_char) # clearly wrong
#' str_as_num_fact(x_fact) # correct
#' str_as_num_7.10(x_fact) # correct
#'
#' # as.numeric() gives the *indices* of the factor levels
#' # for factors
#' str_as_num_base(x_int)  # correct
#' str_as_num_base(x_num)  # correct
#' str_as_num_base(x_char) # correct
#' str_as_num_base(x_fact) # wrong
#'
#' @export
as.numeric_safe <- function(x, keep_integer = TRUE) {
  # Prevent coercion leading to output with NAs
  stopifnot(is.null(x) || is.factor(x) || is.vector(x), !is.list(x))
  if(keep_integer && is.integer(x)) {
    x
  } else {
    as.numeric(as.character(x))
  }
}
