#' Check if a pattern matches a file
#'
#' Check if a pattern matches a file that is present in the working directory.
#'
#' @inheritParams check_file
#'
#' @returns `TRUE` or `FALSE` indicating if `pattern` matches a file name. A
#' warning is issued if `pattern` matches multiple file names. An error is
#' thrown if `pattern` does not match any file names.
#'
#' @seealso [check_file()] [create_dir()] [is_character()]
#'
#' @examples
#' is_file(pattern <- "AMESP") # TRUE # (file 'NAMESPACE')
#' is_file(pattern <- "amesp", ignore_case = TRUE) # TRUE # (file 'NAMESPACE')
#' is_file(pattern <- "amesp", ignore_case = FALSE) # Error reporting presence of case-insensitive match 'NAMESPACE'
#' is_file(pattern <- "abc", ignore_case = FALSE) # Error reporting no match found.
#' is_file(pattern <- "ICEN") # TRUE # Reporting multiple matches.
#'
#' @export
is_file <- function(pattern, ignore_case = TRUE, quietly = FALSE) {
  # Defer argument checking to check_file().
  checkinput::is_character(x = check_file(pattern, ignore_case = ignore_case,
                                          quietly = quietly))
}
