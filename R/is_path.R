#' Check that `x` is a valid path
#'
#' Check that `x` is a path that s likely to be a valid path.
#'
#' @param path [character string][is_character()] with the path.
#' @param as_file `TRUE` or `FALSE`: is `path` intended to point to a file
#' instead of to a folder?
#'
#' @details
#' `is_path()` puts some restrictions on paths because it is used to check for
#' valid paths before creating a directory:
#'
#' - `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`, `>`,
#'   nor any of the control characters (`ASCII` octal codes 000 through 037 and
#'   177, see `help("regex")`).
#' - `path` components should **not** end in a slash, backslash, space or dot
#'   (with the exception of `path` being `"."` to indicate the working
#'   directory): those characters would be removed if the directory to which
#'   `path` points is created, leading to a mismatch between the created
#'   directory and the returned path.
#' - `path` components should **not** be case-insensitive variants of `CON`,
#'   `PRN`, `AUX`, `NUL`, `COM<non-zero digit>`, `LPT<non-zero digit>`, nor
#'   these names followed by an extension: these are reserved names in Windows.
#'
#' In contrast to functions from `checkinput`, `is_path` will produce an error
#' if `path` is not a valid path.
#'
#' @returns
#' `TRUE`: an error occurs if `path` is not a valid path.
#'
#' @section Programming notes:
#' Ways to make `is_path()` even stricter:
#'
#' - Do **not** allow `.` or `..` except as complete directory component at the
#'   beginning?
#' - Check that the normalized path is not equal to normalized `tempdir()`: a
#'   temporary subdirectory should be used?
#'
#' See also
#' - https://github.com/r-lib/usethis/blob/main/R/directory.R
#' - https://github.com/r-lib/usethis/blob/main/tests/testthat/test-directory.R
#' - https://docs.racket-lang.org/reference/windowspaths.html
#' - function `progutils::create_path()`
#' - packages `fs` and `usethis`.
#'
#' On existence and permissions, see `utils::file_test()` and references there.
#'
#' @seealso
#' [create_path()] to create a path, and references there about file paths,
#' [create_dir()] to create a directory if it does not yet exist,
#' [get_filename()] to check if a file exists and is a unique match to a pattern
#'
#' `fs::path_sanitize()` to *remove* invalid characters from potential paths,
#' looking for a wider range of invalid characters.
#'
#' @family functions to handle paths and directories
#'
#' @section References:
#' - https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
#' - https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits
#'
#' @examples
#' is_path(getwd())
#' try(is_path(file.path(getwd(), "ab|cd")))
#'
#' @export
is_path <- function(path, as_file = FALSE) {
  stopifnot(checkinput::is_character(path), checkinput::is_logical(as_file))
  path_comp <- unlist(strsplit(x = path, split = c("/", "\\"), fixed = TRUE))

  if(grepl(pattern = '["*?|<>]', x = path)) {
    stop(paste_quoted(deparse(substitute(path))),
         " should not contain any of \" * ? | < >:\n", path)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = path)) {
    stop("'path' should not contain any control character:\n", path)
  }

  if(any(!nzchar(path_comp)) ||
     any(endsWith(x = c(path, path_comp), suffix = "/") |
         endsWith(x = c(path, path_comp), suffix = "\\"))) {
    stop("Path components in ", paste_quoted(deparse(substitute(path))),
         " should not end in '/' or '\\':\n", path)
  }

  bool_invalid_dot <- endsWith(x = path_comp, suffix = ".")
  # allow "." as first path component denoting the working directory
  if(bool_invalid_dot[1] && path_comp[1] == ".") {
    bool_invalid_dot[1] <- FALSE
  }

  if(any(bool_invalid_dot | endsWith(x = path_comp, suffix = " "))) {
    stop("Path components in ", paste_quoted(deparse(substitute(path))),
         " should not end with ' ' or '.' (i.e., a space or a dot):\n",
         path)
  }

  Windows_reserved <- c("aux", paste0("com", 1:9), "con", paste0("lpt", 1:9),
                        "nul", "prn")
  if(any(Windows_reserved %in% tolower(path_comp))) {
    reserved_comp <- path_comp[tolower(path_comp) %in% Windows_reserved]
    stop("'path' components should not contain Windows-reserved names (",
         paste_quoted(reserved_comp), "):\n", path)
  }

  TRUE
}
