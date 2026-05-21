#' Check that `x` is a valid path
#'
#' Check that `x` is a path that s likely to be a valid path.
#'
#' @param path [character string][is_character()] with the path.
#' @param as_file `TRUE` or `FALSE`: is `path` intended to point to a file
#' instead of to a folder?
#'
#' @details
#' `is_path()` puts some restrictions on paths so it can be used to check for
#' valid paths before creating a directory:
#'
#' - `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`, `>`,
#'   nor any of the control characters (`ASCII` octal codes 000 through 037 and
#'   177, see `help("regex")`).
#' - `path` components should **not** end with a slash, backslash, space or dot
#'   (`"."` and `".."` are allowed as first component to indicate the working
#'   directory and the parent directory, respectively).
#' - `path` components should **not** be `CON`, `PRN`, `AUX`, `NUL`,
#'   `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive variants of
#'   these names, and these names followed by an extension.
#' - `path` should not point to `tempdir()`: a temporary subdirectory should be
#'   used instead.
#'
#' These restrictions consider characters that would lead to an error in Windows
#' because they are not allowed; characters that  would lead to a mismatch
#' between the created directory and the returned path because they are silently
#' removed in Windows; and words that are reserved names in Windows.
#'
#' In contrast to functions from `checkinput`, `is_path` will produce an error
#' if `path` is not a valid path.
#'
#' @returns
#' `TRUE`: an error occurs if `path` is not a valid path.
#'
#' @section Programming notes:
#'
#' To do:
#' - Also disallow path components to **start** with a dot?
#'
#' See also
#' - https://github.com/r-lib/usethis/blob/main/R/directory.R
#' - https://github.com/r-lib/usethis/blob/main/tests/testthat/test-directory.R
#' - https://docs.racket-lang.org/reference/windowspaths.html
#' - function `progutils::create_path()`
#' - packages `fs`.
#'
#' @seealso
#' [create_path()] to create a path (with references there about file paths),
#' [create_dir()] to create a directory if it does not yet exist,
#' [get_filename()] to check if a file exists and is a unique match to a pattern
#'
#' `fs::path_sanitize()` to *remove* invalid characters from potential paths.
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
         " should not contain '\"', '*', '?', '|', '<' or '>':\n", path)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = path)) {
    stop("'path' should not contain control characters:\n", path)
  }

  # '[-1]' allows path to start with '/', needed on MacOS and Ubuntu.
  if(any(!nzchar(path_comp[-1])) ||
     any(endsWith(x = c(path, path_comp), suffix = "/") |
         endsWith(x = c(path, path_comp), suffix = "\\"))) {
    warning("Repeated '/' or '\\' in ", paste_quoted(deparse(substitute(path))),
            " will be ignored:\n", path)
  }

  bool_invalid_dot <- endsWith(x = path_comp, suffix = ".")
  # "." and ".." are allowed as first path component to denote the working
  # directory and the parent directory, respectively
  if(bool_invalid_dot[1] && path_comp[1] %in% c(".", "..")) {
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

  if(normalizePath(path, winslash = "/", mustWork = FALSE) ==
     normalizePath(tempdir(), winslash = "/", mustWork = FALSE)) {
    stop(wrap_text(paste0(
      "'path' should not point to 'tempdir()': instead, point to a subdirectory",
      " in tempdir() through 'file.path(tempdir(), \"subdir\")', or create such",
      " a subdirectory through 'create_tempdir(subdir = \"subdir\")':\n", path)))
  }

  TRUE
}
