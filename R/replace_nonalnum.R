#' Replace Non-Alphanumeric Characters
#'
#' Replace or remove non-alphanumeric characters, keeping dots and handling
#' underscores as indicated by `keep_underscores`.
#'
#' @param x [character vector][checkinput::all_characters()] with strings to
#' remove non-alphanumeric characters from. Empty strings (`""`) are allowed.
#' @param replacement [character string][checkinput::is_character()] used to
#' replace non-alphanumeric characters. It might be empty (i.e., `""`) to remove
#' instead of replace non-alphanumeric characters.
#' @param keep_underscore `TRUE` or `FALSE`: keep underscores? If `FALSE`,
#' underscores are replaced by `replacement`, which is only apparent if a
#' non-default `replacement` is used such that it is not an underscore, see the
#' last 'Example'.
#'
#' @returns
#' `x` with non-alphanumeric characters other than dots, as well as underscores
#' if `keep_underscores` is `FALSE`, replaced by `replacement`. An empty string
#' (`""`) is returned if the empty character string (`""`) is used for
#' `replacement` and `x` does not contain non-replaced characters.
#'
#' @details
#' This implementation uses the [character class][regex] `[:alnum:]` that
#' depends on the current [locale][locales], see the 'Programming Notes' in
#' [checkinput::all_names()].
#'
#' @seealso
#' [trimws()] to remove leading whitespace, trailing whitespace, or both;
#' [replace_vals()], with more relevant links, to replace specific values;
#' [checkinput::all_names()] and [checkinput::is_path()] for checks on the
#' validity of strings.
#'
#' @family functions to check equality
#' @family functions to modify character vectors
#'
#' @examples
#' replace_nonalnum("a+b.txt")
#' replace_nonalnum("a b.txt")
#' replace_nonalnum("ab12._")  # returned unchanged
#' replace_nonalnum(c("a+b.txt", "a b.txt", "ab12._"))
#'
#' # Removing instead of replacing nonalphanumeric characters
#' replace_nonalnum("a+b.txt", replacement = "") # "ab.txt"
#'
#' # Handling underscores
#' replace_nonalnum("a+bc_d.txt", replacement = "",
#'                  keep_underscore = TRUE)  # "abc_d.txt"
#' replace_nonalnum("a+bc_d.txt", replacement = "",
#'                  keep_underscore = FALSE) # "abcd.txt"
#' # Seems to keep the underscore because the
#' # default 'replacement' is an underscore
#' replace_nonalnum("a+bc_d.txt",
#'                  keep_underscore = FALSE) # "a_bc_d.txt"
#'
#' @export
replace_nonalnum <- function(x,
                             replacement = "_",
                             keep_underscore = TRUE) {
  stopifnot(checkinput::all_characters(x, allow_empty = TRUE),
            checkinput::is_character(replacement, allow_empty = TRUE),
            checkinput::is_logical(keep_underscore))
  if(keep_underscore) {
    pattern <- "[^[:alnum:]._]"
  } else {
    pattern <- "[^[:alnum:].]"
  }
  gsub(pattern = pattern, replacement = replacement, x = x)
}
