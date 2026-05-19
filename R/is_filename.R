#' Check that `x` is a valid filename
#'
#' Check that `x` is a filename that includes an extension and is likely to be a
#' valid filename.
#'
#' @param filename [character string][checkinput::is_character()] with the
#' filename, including the file extension.
#'
#' @details
#' This function puts some restrictions on filenames because it is used to check
#' for valid filenames before creating a file:
#'
#' - `filename` should include a file extension. The compression extensions
#'   `".gz"`, `".bz2"` or `".xz"` are **not** considered file extensions but are
#'   allowed to be present.
#' - `filename` should **not** contain any of the characters `"`, `*`, `/`, `:`,
#'    `?`, `\`, `|`, `<`, or `>`: these are not allowed on Windows.
#' - `filename` should **not** contain any of the control characters (`ASCII`
#'   octal codes 000 through 037 and 177, see `help("regex")`).
#'    `?`, `\`, `|`, `<`, or `>`: these are not allowed on Windows.
#' - `filename` should **not** start with a space: the space is removed on
#'   Windows.
#' - The part of `filename` before the extension should **not** end in a space
#'   nor in a period: these are not handled well on Windows.
#'
#' Although not enforced by `is_filename()`, it is good practice to avoid the
#' characters `+`, `,`, `;`, `=`, `[`, `]`, `!`, `$`, `#`, `@`
#'
#'
#' Also avoid the characters `{`, `}`, `(`, `)`, `'`, `%`, `&`, <backtick>, `^`, `~`.
#'
#' Ways to make `is_filename()` even stricter:
#'
#' - do not allow filenames to start with a hyphen or underline
#' - do not allow filenames to end with a hyphen or underline
#' - case-insensitive matching to `filename` to determine if it exists?
#'   `filename` should **not** point to a directory (see `get_filename.R`,
#'   `create_tempdir.R`, `create_path.R`).
#' - Impose a limit on the length:
#'   https://blog.r-project.org/2023/03/07/path-length-limit-on-windows/
#' - See also function `progutils::create_path()` and packages `fs` and `usethis`.
#'
#' In contrast to functions from `checkinput`, `is_filename` will produce an
#' error if `filename` is not a valid filename.
#'
#' @returns
#' `TRUE`: an error occurs if `filename` is not a valid filename.
#'
#' @section References:
#' - https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
#' - https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits
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
  # To do:
  # - Remove redundant those conditions
  if(!nzchar(filename_no_ext) || !nzchar(file_ext) ||
     filename == filename_no_ext || length(filename_no_ext) == 0L ||
     length(file_ext) == 0L) {
    stop("Empty filename or missing extension:\n", filename)
  }

  if(grepl(pattern = '[/\\]', x = filename_no_ext)) {
    stop("'filename' should not contain '/' or '\\' (use 'is_path()' to allow",
         " for directories):\n", filename)
  }

  # Slashes and backslashes will have been catched above.
  if(grepl(pattern = '["*/:?\\|<>]', x = filename_no_ext)) {
    stop("'filename' should not contain any of \" * / : ? \ | < >:\n", filename)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = filename_no_ext)) {
    stop("'filename' should not contain any of the control characters:\n",
         filename)
  }

  if(startsWith(x = filename_no_ext, prefix = " ")) {
    stop("'filename' should not start with ' ' (i.e., a space):\n", filename)
  }

  if(endsWith(x = filename_no_ext, suffix = ".")) {
    stop("'filename' should not end with '.':\n", filename)
  }

  if(endsWith(x = filename_no_ext, suffix = " ")) {
    stop("'filename' should not end with ' ' (i.e., a space):\n", filename)
  }

  TRUE
}
