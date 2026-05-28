#' Check that `x` is a valid path
#'
#' Check that `x` is a path that is likely to be valid.
#'
#' @param path [character string][checkinput::is_character()] with the path.
#' @param to_file `TRUE` or `FALSE`: is `path` intended to point to a file?
#'
#' @details
#' `is_path()` puts some restrictions on paths so it can be used to check for
#' valid paths before creating a directory or a file:
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
#'   used instead (see [create_tempdir()]).
#'
#' If `to_file` is `TRUE`, the part after the last slash is considered the
#' filename. The filename should adhere to the same restrictions as the path.
#'
#' In addition:
#' - the filename should include a file extension. The compression extensions
#'   `".gz"`, `".bz2"` or `".xz"` are **not** considered file extensions but are
#'   allowed to be present.
#' - the filename should **not** start with a space.
#' - the filename should **not** contain `:`.
#'
#' These restrictions on the path and the filename consider characters that
#' would lead to an error in Windows because they are not allowed; characters
#' that would lead to a mismatch between the created directory and the returned
#' path because they are silently removed in Windows; and words that are
#' reserved names in Windows.
#'
#' In contrast to functions from `checkinput`, `is_path` will produce an error
#' if `path` is not a valid path or, if `to_file` is `TRUE`, `basename(path)` is
#' not a valid filename that includes an extension.
#'
#' @returns
#' `TRUE`: an error occurs if `path` is not a valid path or, if `to_file` is
#' `TRUE`, `basename(path)` is not a valid filename that includes an extension.
#'
#' @section Programming notes:
#' On MacOS, the output of `tempdir()` is preceded by duplicated forward slashes
#' in R cmd checks (e.g., `/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`),
#' leading to a spurious warning from `is_path()`.
#'
#' The file separator is a backslash (`\`) on Windows but a forward slash (`/`)
#' on other operating systems ([.Platform$file.sep][.Platform] gives the file
#' separator used on the current platform). Furthermore, the backslash is used
#' as [escape character][regex] in \R, such that backslashes need to be escaped
#' in \R code. Thus, to warn if a [string][checkinput::is_character()] contains
#' a file separator, one should write the warning message as
#' `warning("Repeated '/' or '\\'")` which will be printed as
#' `Repeated '/' or '\'`. Checks on the presence of slashes and backslashes
#' should use `grepl(pattern = "/", x = string)` and
#' `grepl(pattern = "\\\\", x = string)` (!). This makes it cumbersome to get
#' the correct type and number of slashes to compare with the path recorded in a
#' warning message, such that it is more robust to check only for fixed parts of
#' the message (e.g., `"Repeated"`), possibly followed by a check like
#' `tinytest::expect_true(dir.exists(string))`.
#'
#' @section References:
#' - Naming files, paths, and namespaces from
#'   [Microsoft](https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file)
#' - Comparison of file systems from
#'   [Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
#'
#' Although not enforced by `is_path()`, it is good practice to also avoid
#' the characters `+`, `,`, `;`, `=`, `[`, `]`, `!`, `$`, `#`, `@`, and possibly
#' `{`, `}`, `(`, `)`, `'`, `%`, `&`, <backtick>, `^`, `~`.
#'
#' Ways to make `is_path()` even stricter:
#'
#' - do not allow filenames to start with a hyphen
#' - do not allow filenames to end with a hyphen
#' - case-insensitive matching to `filename` to determine if it exists?
#'   `filename` should **not** point to a directory (see `utils::file_test()`,
#'   [get_file_path()], [create_tempdir()], [create_file_path()]).
#' - Impose a limit on the length:
#'   https://blog.r-project.org/2023/03/07/path-length-limit-on-windows/
#' - See also functions `create_file_path()` and `fs::path_real()`.
#'
#' @seealso
#' `utils::file_test()` and references there on file existence and permissions,
#' `fs::path_real()`.
#' [create_file_path()] to create a path (with references there about file
#' paths), [create_dir()] to create a directory if it does not yet exist,
#' [get_file_path()] to check if a file exists and is a unique match to a pattern
#'
#' `fs::path_sanitize()` to *remove* invalid characters from potential paths.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' is_path(getwd())
#' try(is_path(file.path(getwd(), "ab|cd")))
#'
#' is_path(file.path(getwd(), "abcd.txt"), to_file = TRUE)
#' is_path(file.path(getwd(), "abcd.txt.gz"), to_file = TRUE)
#'
#' try(is_path(file.path(getwd(), "abcd"), to_file = TRUE))
#' try(is_path(file.path(getwd(), "abcd.gz"), to_file = TRUE))
#' try(is_path(file.path(getwd(), "ab:cd.txt"), to_file = TRUE))
#' try(is_path(file.path(getwd(), "ab|cd.txt"), to_file = TRUE))
#'
#' @export
is_path <- function(path, to_file = FALSE) {
  stopifnot(checkinput::is_character(path), checkinput::is_logical(to_file))

  # 'split = c("/", "\\")' does not work because that recycles 'split' along 'x'
  path_comp <- unlist(strsplit(x = path, split = "/", fixed = TRUE))
  path_comp <- unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE))
  if(length(path_comp) < 2L) {
    stop("'path' (", paste_quoted(deparse(substitute(path))),
         ") should contain path separators ('/' or '\\\\'):\n", path)
  }

  if(grepl(pattern = "//", x = path, fixed = TRUE) ||
     grepl(pattern = "\\\\", x = path, fixed = TRUE)) {
    warning("Repeated '/' or '\\' in ", paste_quoted(deparse(substitute(path))),
            " will be ignored:\n", path)
  }

  if(grepl(pattern = '["*?|<>]', x = path)) {
    stop(paste_quoted(deparse(substitute(path))),
         " should not contain '\"', '*', '?', '|', '<' or '>':\n", path)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = path)) {
    stop("'path' should not contain control characters:\n", path)
  }

  Windows_reserved <- c("aux", paste0("com", 1:9), "con", paste0("lpt", 1:9),
                        "nul", "prn")
  if(any(Windows_reserved %in% tolower(path_comp))) {
    reserved_comp <- path_comp[tolower(path_comp) %in% Windows_reserved]
    stop("'path' components should not contain Windows-reserved names (",
         paste_quoted(reserved_comp), "):\n", path)
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

  if(!to_file) {
    to_tempdir <-
      basename(normalizePath(path, winslash = "/", mustWork = FALSE)) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
  } else {
    to_tempdir <-
      basename(dirname(normalizePath(path, winslash = "/", mustWork = FALSE))) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))

    filename <- basename(path)

    if(startsWith(x = filename, prefix = " ")) {
      stop("'filename' should not start with ' ' (i.e., a space):\n", filename)
    }

    if(grepl(pattern = ':', x = filename, fixed = TRUE)) {
      stop("'filename' should not contain ':':\n", filename)
    }

    # See section 'Details' in file_path_no_ext() on why not using
    # tools::file_path_sans_ext() nor tools::file_ext().
    filename_no_ext <- file_path_no_ext(x = filename, compression = TRUE)
    file_ext <- file_path_ext(x = filename, compression = TRUE)
    if(!nzchar(filename_no_ext) || !nzchar(file_ext) ||
       length(filename_no_ext) == 0L || length(file_ext) == 0L) {
      stop("Empty filename or missing extension while 'to_file' is TRUE:\n",
           filename)
    }

    if(endsWith(x = filename_no_ext, suffix = " ") ||
       endsWith(x = filename_no_ext, suffix = ".")) {
      stop("'filename' should not end with ' ' or '.' (i.e., a space or a dot):\n",
           filename)
    }
  }

  if(to_tempdir) {
    stop(wrap_text(paste0(
      "'path' should not point to 'tempdir()': instead, point to a subdirectory",
      " in tempdir() through 'file.path(tempdir(), \"subdir\")', or create such",
      " a subdirectory through 'create_tempdir(subdir = \"subdir\")':\n", path)))
  }

  TRUE
}
