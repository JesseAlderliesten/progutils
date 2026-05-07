#' Show head and tail of object
#'
#' @param x object of which the first and last part should be returned.
#' @param n [make_natural][checkinput::make_natural] number (default `3L`)
#' indicating the number of elements or rows to return as `head` and `tail`.
#'
#' @returns The `n` first and `n` last elements or rows of `x`. `x` is returned
#' completely if `n` is not less than half the number of elements or rows of `x`.
#' Using `n = 0` results in a zero-length or zero-row object.
#'
#' @seealso
#' [utils::head()] [utils::tail()]
#'
#' @examples
#' x <- letters
#' names(x) <- LETTERS
#' head_tail(x)
#' head_tail(x, n = 4)
#' head_tail(x, n = 40)
#' try(head_tail(x, n = 0))
#' try(head_tail(x, n = 4.1))
#'
#' m <- head_tail(matrix(data = 1:40, ncol = 4,
#'                       dimnames = list(LETTERS[1:10], letters[1:4])))
#' head_tail(m)
#' head_tail(as.data.frame(m))
#'
#' @export
head_tail <- function(x, n = 3L) {
  n <- checkinput::make_natural(n, strict = TRUE)

  if(is.null(dim(x))) {
    if(length(x) > 2L * n) {
      x <- c(utils::head(x, n), utils::tail(x, n))
    }
  } else {
    if(nrow(x) > 2L * n) {
      x <- rbind(utils::head(x, n), utils::tail(x, n))
    }
  }
  x
}
