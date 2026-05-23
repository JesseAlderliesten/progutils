#' Show first and last part of an object
#'
#' @param x object of which the first and last part should be returned.
#' @param n [natural][checkinput::make_natural] number (default `3L`) indicating
#' the number of elements or rows to return as `head` and `tail`.
#'
#' @returns The `n` first and last elements or rows of `x`.
#'
#' @seealso
#' [utils::head()] [utils::tail()]
#'
#' @examples
#' x <- letters[1:10]
#' names(x) <- LETTERS[1:10]
#' x
#' head_tail(x)
#' head_tail(x, n = 4)
#' head_tail(x, n = 40)
#' try(head_tail(x, n = 0))
#' try(head_tail(x, n = 4.1))
#'
#' df <- head_tail(matrix(data = 1:40, ncol = 4,
#'                        dimnames = list(LETTERS[1:10], letters[1:4])))
#' head_tail(df)
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
