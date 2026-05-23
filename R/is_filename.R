#' Check that `x` is a valid filename
#'
#' Check that `x` is a filename that includes an extension and is likely to be
#' valid.
#'
#' @param filename [character string][checkinput::is_character()] with the
#' filename including the file extension.
#'
#' @details
#' `is_filename()` puts some restrictions on filenames so it can be used to
#' check for valid filenames before creating a file:
#'
#' - `filename` should include a file extension. The compression extensions
#'   `".gz"`, `".bz2"` or `".xz"` are **not** considered file extensions but are
#'   allowed to be present.
#' - `filename` should **not** contain characters `"`, `*`, `/`, `:`, `?`, `\`,
#'   `|`, `<`, `>`, nor any of the control characters (`ASCII` octal codes 000
#'   through 037 and 177, see `help("regex")`).
#' - `filename` should **not** start with a space.
#' - The part of `filename` before the extension should **not** end with a space
#'   nor with a period.
#'
#' These restrictions consider characters that would lead to an error in Windows
#' because they are not allowed; characters that  would lead to a mismatch
#' between the created directory and the returned path because they are silently
#' removed on Windows; and words that are reserved names in Windows.
#'
#' In contrast to functions from `checkinput`, `is_filename` will produce an
#' error if `filename` is not a valid filename.
#'
#' @returns
#' `TRUE`: an error occurs if `filename` is not a valid filename.
#'
#' @section Programming notes:
#' Although not enforced by `is_filename()`, it is good practice to also avoid
#' the characters `+`, `,`, `;`, `=`, `[`, `]`, `!`, `$`, `#`, `@`, and possibly
#' `{`, `}`, `(`, `)`, `'`, `%`, `&`, <backtick>, `^`, `~`.
#'
#' Ways to make `is_filename()` even stricter:
#'
#' - do not allow filenames to start with a hyphen
#' - do not allow filenames to end with a hyphen
#' - case-insensitive matching to `filename` to determine if it exists?
#'   `filename` should **not** point to a directory (see `utils::file_test()`,
#'   [get_filename()], [create_tempdir()], [create_file_path()]).
#' - Impose a limit on the length:
#'   https://blog.r-project.org/2023/03/07/path-length-limit-on-windows/
#' - See also functions `create_file_path()`, `make_filename()` and
#'   `fs::path_real()`.
#'
#' @section References:
#' - https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
#' - https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits
#'
#' @seealso
#' On file existence and permissions, see `utils::file_test()` and references
#' there and `fs::path_real()`.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' is_filename("abcd.txt")
#' is_filename("abcd.txt.gz")
#'
#' try(is_filename("abcd"))
#' try(is_filename("abcd.gz"))
#' try(is_filename("ab|cd.txt"))
#'
#' @export
is_filename <- function(filename) {
  stopifnot(checkinput::is_character(filename))

  # See section 'Details' in file_path_no_ext() on why not using
  # tools::file_path_sans_ext() nor tools::file_ext().
  filename_no_ext <- file_path_no_ext(x = filename, compression = TRUE)
  file_ext <- file_path_ext(x = filename, compression = TRUE)
  if(!nzchar(filename_no_ext) || !nzchar(file_ext) ||
     length(filename_no_ext) == 0L || length(file_ext) == 0L) {
    stop("Empty filename or missing extension:\n", filename)
  }

  if(grepl(pattern = '[/\\]', x = filename_no_ext)) {
    stop("'filename' should not contain '/' or '\\' (use 'is_path()' or",
         " argument 'dir' in 'create_file_path()' to allow for directories):\n",
         filename)
  }

  # Slashes and backslashes will have been catched above.
  if(grepl(pattern = '["*/:?\\|<>]', x = filename_no_ext)) {
    stop("'filename' should not contain '\"', '*', '/', ':', '?', '\', '|',",
         " '<' or '>':\n", filename)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = filename_no_ext)) {
    stop("'filename' should not contain control characters:\n", filename)
  }

  if(startsWith(x = filename_no_ext, prefix = " ")) {
    stop("'filename' should not start with ' ' (i.e., a space):\n", filename)
  }

  if(endsWith(x = filename_no_ext, suffix = " ") ||
     endsWith(x = filename_no_ext, suffix = ".")) {
    stop("'filename' should not end with ' ' or '.' (i.e., a space or a dot):\n",
         filename)
  }

  TRUE
}
