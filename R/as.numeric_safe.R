#' Convert x to numeric
#'
#' Convert x to a numeric vector in a way that works irrespective if `x` is a
#' factor, a character vector, or already a numeric vector.
#'
#' @param x A factor, or a character or numeric vector to be converted to a
#' numeric vector.
#'
#' @returns A numeric vector.
#'
#' @details
#' `as.numeric_safe()` uses a suggestion from [factor()] and
#' [\R FAQ 7.10](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f)
#' that not only works if `x` is a factor but also if `x` is a character vector
#' or already is a numeric vector (even though though the last example of
#' `as.numeric` comments that this approach is less efficient for long vectors).
#'
#' In contrast, the alternative conversions `as.numeric(levels(x))[x]` (suggested
#' in the `Warning` of `factor()`) and `as.numeric(levels(x))[as.integer(x)]`
#' (suggested in \R `FAQ 7.10`) *only* work if `x` is factor, see the `Examples`.
#'
#' Furthermore, `as.numeric()` *cannot* be used to convert a factor to numeric,
#' because it returns their underlying numeric (integer) representation (i.e.,
#' the indices of the factor levels), see the `Warnings` in [as.numeric()] and
#' [factor()].
#'
#' @section Wishlist:
#' Could add argument `keep_integer` indicating if input of type [integer()]
#' should be returned as-is, i.e., without converting to numeric type.
#'
#' @note
#' NULL and zero-length vectors are converted to numeric(0). Logical vectors of
#' length > 0 are converted to a vector of numerical NAs (i.e., `NA_real_`),
#' with a warning.
#'
#' @family functions to modify character vectors
#' @family functions to modify factors
#'
#' @examples
#' x_int <- 5:7
#' x_num <- as.numeric(x_int)
#' x_char <- as.character(x_int)
#' x_fact <- as.factor(x_int)
#'
#' str_as_num_safe <- function(x) {str(as.numeric_safe(x))}
#' str_as_num_fact <- function(x) {str(as.numeric(levels(x))[x])}
#' str_as_num_7.10 <- function(x) {str(as.numeric(levels(x))[as.integer(x)])}
#' str_as_num_base <- function(x) {str(as.numeric(x))}
#'
#' # as.numeric_safe() works irrespective the type of x
#' str_as_num_safe(x_int)  # num [1:3] 5 6 7 # fine
#' str_as_num_safe(x_num)  # num [1:3] 5 6 7 # fine
#' str_as_num_safe(x_char) # num [1:3] 5 6 7 # fine
#' str_as_num_safe(x_fact) # num [1:3] 5 6 7 # fine
#'
#' # The 'more efficient' suggestion in help(factor) *only* works for factors.
#' str_as_num_fact(x_int)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_fact(x_num)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_fact(x_char) # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_fact(x_fact) # num [1:3] 5 6 7 # fine
#'
#' # The 'more efficient' suggestion in R FAQ 7.10 *only* works for factors.
#' str_as_num_7.10(x_int)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_num)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_char) # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_fact) # num [1:3] 5 6 7 # fine
#'
#' # as.numeric() gives the *indices* of the factor levels for factors
#' str_as_num_base(x_int)  # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_num)  # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_char) # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_fact) # num [1:3] 1 2 3 # wrong!
#'
#' @export
as.numeric_safe <- function(x) {
  # Prevent coercion leading to output with NAs
  stopifnot(is.null(x) || is.factor(x) || is.vector(x), !is.list(x))
  as.numeric(as.character(x))
}
