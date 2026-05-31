#' Check that `x` is a valid path
#'
#' Check that `x` is a path that is likely to be valid.
#'
#' @param path [character string][checkinput::is_character()] with the path.
#'
#' @details
#' `is_path()` puts some restrictions on paths so it can be used to check for
#' valid paths before creating a directory or a file:
#'
#' - `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`, `>`,
#'   nor any of the control characters (`ASCII` octal codes 000 through 037 and
#'   177, see `help("regex")`).
#' - `path` components (i.e., parts separated by file separators `/` or `\\`)
#'   should **not** be the Windows-reserved terms `CON`, `PRN`, `AUX`, `NUL`,
#'   `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive variants of
#'   these names, or these names followed by an extension.
#' - `path` components
#'   should **not** end with a space or dot (`"."` and `".."` are allowed as
#'   first component to indicate the working directory and the parent directory,
#'   respectively).
#' - `path` should not point to `tempdir()`: a temporary subdirectory should be
#'   used instead (see [create_tempdir()]).
#'
#' Furthermore, if `path` contains a file extension or compression extension,
#' the part after the last
#' slash is considered the filename, which should adhere to the same
#' restrictions as the path but should **not** contain `:` **nor**
#' start with a space, while it might contain Windows-reserved terms.
#'
#' These restrictions on the path and the filename consider characters that
#' would lead to an error in Windows because they are not allowed; characters
#' that would lead to a mismatch between the created directory and the returned
#' path because they are silently removed in Windows; and words that are
#' reserved names in Windows.
#'
#' `path` does **not** have to point to an existing directory: `path` even does
#' **not** have to contain a file separator (e.g., `/` or `\\`), such that
#' `is_path()` can be used to check that input to [fs::path()] only contains
#' allowed characters. Repeated file separators (e.g., `//` or `\\\\`) are
#' treated as single separators, with a warning.
#'
#' @returns
#' `TRUE`: an error occurs if `path` is not a valid path or if `path` contains a
#' file extension but the filename is not valid.
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
#' `grepl(pattern = "\\\\", x = string)`. This makes it cumbersome to get the
#' correct type and number of slashes to compare with the path recorded in a
#' warning message, such that it is more robust to check only for fixed parts of
#' the message (e.g., `"Repeated"`), possibly followed by a check like
#' `tinytest::expect_true(dir.exists(string))`.
#'
#' Trailing slashes or backslashes are allowed, even though these might be
#' removed in some operations.
#'
#' @section References:
#' - Naming files, paths, and namespaces from
#'   [Microsoft](https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file)
#' - Comparison of file systems from
#'   [Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
#'
#' @seealso
#' `utils::file_test()` and references there on file existence and permissions,
#' `help(fs::path_math)` for various operations on paths,
#' [create_file_path()] to create a path (with references there about file
#' paths), [create_dir()] to create a directory if it does not yet exist,
#' [get_file_path()] to check if a file exists and is a unique match to a pattern
#'
#' `fs::path_sanitize()` to **remove** invalid characters from potential paths.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' is_path(getwd())
#' is_path(fs::path_wd("abcd"))
#' try(is_path(fs::path_wd("ab|cd")))
#'
#' is_path(fs::path_wd("abcd.txt"))
#' is_path(fs::path_wd("abcd.txt.gz"))
#' is_path(fs::path_wd("abcd.gz"))
#'
#' try(is_path(fs::path_wd("ab:cd.txt")))
#' try(is_path(fs::path_wd("ab|cd.txt")))
#'
#' @export
is_path <- function(path) {
  stopifnot(checkinput::is_character(path))

  # Notes:
  # - split = c("/", "\\") does not work because that recycles 'split' along 'x'
  # - fs::path_split() does not work because it tidies the path using
  #   fs::path_tidy() before splitting, which removes repeated slashes
  # - The if-else construct is needed because strsplit() discards empty quotes
  #   in the input.
  path_comp <- unlist(strsplit(x = path, split = "/", fixed = TRUE))
  if(any(!nzchar(path_comp))) {
    path_comp <- c(unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE)), "")
  } else {
    path_comp <- unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE))
  }

  # The check for zero-character (i.e., "") is needed to catch patterns like "/\\"
  if(any(!nzchar(path_comp)) || grepl(pattern = "//", x = path, fixed = TRUE) ||
     grepl(pattern = "\\\\", x = path, fixed = TRUE)) {
    warning("Repeated '/' or '\\\\' in ", paste_quoted(deparse(substitute(path))),
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

  filename <- basename(path)
  file_ext <- fs::path_ext(path = filename)
  # To catch case where filename ends in a dot, e.g., "ff..txt"
  end_dot <- filename != "." && filename != ".." && endsWith(
    sub(pattern = paste0("[.]", file_ext, "$"), replacement = "", x = filename),
    suffix = ".")
  if(!end_dot && (length(file_ext) == 0L || !nzchar(file_ext))) {
    to_tempdir <-
      basename(normalizePath(path, winslash = "/", mustWork = FALSE)) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
  } else {
    to_tempdir <-
      basename(dirname(normalizePath(path, winslash = "/", mustWork = FALSE))) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))

    filename_no_ext <- fs::path_ext_remove(path = filename)
    if(length(filename_no_ext) == 0L || !nzchar(filename_no_ext)) {
      stop("filename without extension (", paste_quoted(filename_no_ext),
           ") should not be empty:\n", path)
    }

    if(startsWith(x = filename, prefix = " ")) {
      stop("'filename' should not start with ' ' (i.e., a space):\n", filename)
    }

    if(grepl(pattern = ":", x = filename, fixed = TRUE)) {
      stop("'filename' should not contain ':':\n", filename)
    }

    if(endsWith(x = filename_no_ext, suffix = " ") ||
       endsWith(x = filename_no_ext, suffix = ".") ||
       # To catch case where filename ends in a dot, e.g., "ff..txt"
       end_dot) {
      stop("'filename' should not end with ' ' or '.' (i.e., a space or a dot):\n",
           filename)
    }
  }

  if(to_tempdir) {
    stop(wrap_text(paste0(
      "'path' should not point to 'tempdir()': instead, point to a subdirectory",
      " in tempdir() through 'fs::path(tempdir(), \"subdir\")', or create such",
      " a subdirectory through 'create_tempdir(subdir = \"subdir\")':\n", path)))
  }

  TRUE
}
