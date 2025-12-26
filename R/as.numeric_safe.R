#' Convert x to numeric
#'
#' Convert x to numeric in a way that works irrespective if `x` is character,
#' factor, or already numeric.
#'
#' @param x A character, factor, or numeric object to be converted to numeric.
#'
#' @returns A numeric object.
#'
#' @details
#' `as.numeric()` *cannot* be used to convert a factor to numeric, because that
#' returns their underlying numeric (integer) representation (i.e., the indices
#' of the factor levels), see the `Warnings` in [as.numeric()] and [factor()].
#'
#' The conversions `as.numeric(levels(x))[x]` (suggested in the `Warning` of
#' [factor()] and `as.numeric(levels(x))[as.integer(x)]` (suggested in
#' [\R FAQ 7.10](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f))
#' *only* work if `x` is factor.
#'
#' `as.numeric_safe()` uses the alternative also suggested in `factor()` and
#' \R `FAQ 7.10` that works irrespective if `x` is a factor, character or
#' numeric (even though the last example of `as.numeric` comments that it is
#' less efficient for long vectors).
#'
#' @section Wishlist:
#' Could add argument `keep_integer` indicating if input of type [integer()]
#' should be returned as-is, i.e., without converting to numeric type.
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
#' str_as_num_base <- function(x) {str(as.numeric(x))}
#' str_as_num_fact <- function(x) {str(as.numeric(levels(x))[x])}
#' str_as_num_7.10 <- function(x) {str(as.numeric(levels(x))[as.integer(x)])}
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
#' # as.numeric() gives the *indices* of the factor levels for factors
#' str_as_num_base(x_int)  # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_num)  # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_char) # num [1:3] 5 6 7 # fine
#' str_as_num_base(x_fact) # num [1:3] 1 2 3 # wrong!
#'
#' # The 'more efficient' suggestion in R FAQ 7.10 *only* works for factors.
#' str_as_num_7.10(x_int)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_num)  # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_char) # num [1:3] NA NA NA # wrong, but clearly so.
#' str_as_num_7.10(x_fact) # num [1:3] 5 6 7 # fine
#'
#' @export
as.numeric_safe <- function(x) {
  as.numeric(as.character(x))
}
