#' Round numbers
#'
#' Round numbers to a specified minimum number of significant digits while
#' ensuring that the integer part is not rounded.
#'
#' @param x [numeric vector][checkinput::all_numbers()], [matrix] or
#' [data.frame], see `Details`.
#' @param digits the **minimum** number of [significant digits][signif()] to
#' round to (see `Details`) or `Inf` to not round values.
#' @param type [character string][checkinput::is_character()] `"selective"` or
#' `"expanded"` to indicate the type of rounding to be used, see `Details`.
#'
#' @returns
#' `x` with rounded values, see `Details`.
#'
#' @details
#' [Rounding][signif()] numbers to a specified number of significant digits can
#' lead to values ending in rounded zeros, e.g., `"14300"` instead of `"14286"`
#' when rounding `(1e5)/7` to 3 significant digits. To prevent this,
#' `signif_custom()` treats the value in argument `digits` as a **minimum**
#' value and rounds to more significant digits if that is needed to not round
#' the integer part of numbers.
#'
#' If `type` is `"selective"`, the required value of `digits` is determined for
#' each value individually, such that the number of significant digits might
#' differ if `x` has length larger than one. If type is `"expanded"`, the value
#' of `digits` is increased to the same value for all numbers in a vector or
#' column.
#'
#' [Infinite values][Inf] are is returned unchanged. `NA_integer_` and
#' `NA_real_` are both returned as [NA_real_][NA]. `signif_custom()` also
#' handles [matrices][matrix] and [dataframes][data.frame], rounding the numeric
#' columns with rounding of type `expanded` type on a per-column basis. It does
#' **not** handle [factors][factor], use [as.numeric_safe()] on `x` or use
#' `round_levels(x = x, level_order = levels(x), digits = digits, type = type)`.
#'
#' @seealso
#' [round()] to round to a specified number of decimal places; [signif()] to
#' round to a specified number of significant digits; [zapsmall()] to put small
#' values to zero; [formatC()] for other ways to format numbers;
#' [round_levels()] to round [factor] levels.
#'
#' @family functions to check equality
#'
#' @examples
#' x1 <- (1e5)/7
#' signif(x = x1, digits = 3) # returns 14300
#' signif_custom(x1, digits = 3) # returns 14286 to not round the integer part
#'
#' x2 <- c(10^c(-1, 5)/7)
#' signif(x = x2, digits = 3) # returns c(1.43e-02, 1.43e+04)
#' # Increase the number of digits only for the
#' # second number, returning c(0.0143, 14286.0000)
#' signif_custom(x2, digits = 3, type = "selective")
#' # Increase the number of digits for both
#' # numbers, returning c(0.014286, 14286.0000)
#' signif_custom(x2, digits = 3, type = "expanded")
#'
#' x4 <- c(-0.1, 1, -1e4, 1e5) / 7
#' signif(x = x4, digits = 3)
#' # returns c(-0.0143,   0.143,   -1430,   14300)
#' signif_custom(x = x4, digits = 3, type = "selective")
#' # returns c(-0.0143,   0.1430,  -1429.0, 14286)
#' signif_custom(x = x4, digits = 3, type = "expanded")
#' # returns c(-0.014286, 0.14286, -1428.6, 14286)
#'
#' x_df <- data.frame(a = 1:3, b = letters[11:13], c = c(1e5, 1e3, 1)/7, d = pi)
#' signif_custom(x_df, type = "selective")
#' signif_custom(x_df, type = "expanded")
#'
#' @export
signif_custom <- function(x, digits = 3L, type = c("selective", "expanded")) {
  type <- match.arg(type, several.ok = FALSE)

  # Notes:
  # - Values in argument 'digits' of signif() are rounded to the nearest integer
  #   in the range from 0 to 22 (unless it is Inf such that no rounding is
  #   applied), so no need to check if 'digits' are nonnegative and integer.
  stopifnot(checkinput::is_number(digits))

  if(!is.null(nrow(x))) {
    if(is.matrix(x)) {
      ind_cols_numeric <- which(apply(X = x, MARGIN = 2L, FUN = is.numeric))
    } else {
      ind_cols_numeric <- which(
        unlist(lapply(X = x, FUN = is.numeric), use.names = FALSE))
    }

    for(ind_col in ind_cols_numeric) {
      x[, ind_col] <- signif_custom(x = x[, ind_col], digits = digits, type = type)
    }
    return(x)
  } else {
    if(is.factor(x)) {
      stop("'signif_custom()' does not handle factors. You can use",
           "\nsignif_custom(x = progutils::as.numeric_safe(x), digits = digits,",
           " type = type) or\nprogutils::round_levels(x = x, level_order =",
           " levels(x), digits = digits, type = type)")
    }
    stopifnot(is.numeric(x))
  }

  # Notes:
  # - Using abs(x) to also work if x contains negative values.
  # - Selecting finite elements of 'x' to prevent -Inf and Inf from putting
  #   'ceiling_digits_x' at Inf
  ceiling_digits_x <- ceiling(log10(abs(x[is.finite(x)])))

  if(type == "expanded") {
    digits <- max(digits, ceiling_digits_x)
  } else {
    digits_p <- pmax(digits, ceiling_digits_x)

    # numeric(0) in 'x' (which also arises if 'x' contains only infinite values)
    # propagates to zero-length numeric(0) in ceiling_digits_x and in digits_p,
    # leading to an error when passed to signif(). To circumvent this, argument
    # 'digits' is used instead of digits_p for zero-length digits_p.
    if(length(digits_p) > 0) {
      digits <- digits_p
    }
  }

  signif(x = x, digits = digits)
}
