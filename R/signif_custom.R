#' Round numbers
#'
#' Round numbers to a specified minimum number of [significant digits][signif()]
#' while ensuring the integer part is not rounded. This prevents values from
#' ending in rounded zeros, e.g., `14300` instead of `14286` when rounding
#' `1e5 / 7` to three significant digits.
#'
#' @param x [numeric vector][checkinput::all_numbers()], [matrix] or
#' [data.frame], see `Details`.
#' @param digits the **minimum** number of [significant digits][signif()] to
#' round to (see `Details`) or `Inf` to not round values.
#' @param type [character string][checkinput::is_character()] `"selective"` or
#' `"expanded"` indicating the type of rounding to be used, see `Details`.
#'
#' @returns
#' `x` with values rounded according to `digits` and `type`.
#'
#' @details
#' Numeric values are rounded according to arguments `digits` and `type`. Values
#' are **not** rounded if `digits` is [Inf]. Otherwise, the value of `digits`
#' rounded to the nearest positive integer is used as the **minimum** number of
#' [significant digits][signif()] to round numeric values in `x` to while
#' ensuring their integer parts are not rounded.
#'
#' If `type` is `"selective"`, values are individually rounded to more
#' significant digits than `digits` if that is needed to not round their integer
#' part, such that the number of significant digits might differ between values
#' in a vector or in a column. If `type` is `"expanded"`, all values in a vector
#' or a column are rounded to the same, possibly increased, value of `digits`.
#'
#' [Infinite values][Inf] are is returned unchanged. `NA_integer_` and
#' `NA_real_` are both returned as [NA_real_][NA]. `signif_custom()` also
#' handles [matrices][matrix] and [dataframes][data.frame], rounding the numeric
#' columns with rounding of type `expanded` type on a per-column basis. It does
#' **not** handle [factors][factor]: use [as.numeric_safe()] on `x` or use
#' `round_factor(x = x, level_order = levels(x), digits = digits, type = type)`.
#'
#' @seealso
#' [round()] to round to a specified number of decimal places; [signif()] to
#' round to a specified number of significant digits; [zapsmall()] to put small
#' values to zero; [formatC()] for other ways to format numbers;
#' [round_factor()] to round [factors][factor].
#'
#' @family functions to check equality
#'
#' @examples
#' x1 <- (1e5) / 7
#' signif(x = x1, digits = 3) # returns 14300
#' signif_custom(x1, digits = 3) # returns 14286 to not round the integer part
#'
#' x2 <- 10^c(-1, 5) / 7
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
#' x_df <- data.frame(a = 1:3, b = letters[11:13], c = c(1e5, 1e3, 1) / 7, d = pi)
#' signif_custom(x_df, type = "selective")
#' signif_custom(x_df, type = "expanded")
#'
#' @export
signif_custom <- function(x, digits = 3L, type = c("selective", "expanded")) {
  type <- match.arg(type, several.ok = FALSE)

  # Notes:
  # - Values in argument 'digits' of signif() are rounded to the nearest integer
  #   in the range from 0 to 22 (unless it is Inf such that no rounding is
  #   applied), so no need to check that 'digits' is nonnegative and integer.
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
    # numeric(0) in 'x' (which also arises if 'x' contains only infinite values)
    # propagates to zero-length numeric(0) in ceiling_digits_x and in digits,
    # leading to an error when passed to signif(). To prevent this,
    # ceiling_digits_x is only used instead of 'digits' if the former has a
    # length larger than zero.
    if(length(ceiling_digits_x) > 0) {
      digits <- pmax(digits, ceiling_digits_x)
    }
  }

  signif(x = x, digits = digits)
}
