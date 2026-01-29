#' Check if only a single matching file is present
#'
#' Check if only a single file in `dir` matches `pattern`, for example before
#' attempting to [read a file][read.table()].
#'
#' @param dir Character string giving the [path][file.path()] to a directory.
#' @param pattern Character string containing a [regular expression][base::regex]
#' used to select file names in the directory indicated by `dir`.
#' @param ignore_case `TRUE` or `FALSE`: use case-insensitive pattern matching?
#' @param quietly `TRUE` or `FALSE`: suppress the message with the found file
#' name?
#'
#' @returns
#' If there is exactly one file with a name matching `pattern` in directory
#' `dir`: a character string with the matching file name. Otherwise an error is
#' thrown.
#'
#' @details
#' The default `"."` for `dir` indicates the [working directory][getwd()].
#'
#' If `ignore_case` is `FALSE` and no case-sensitive match is found, it is
#' checked if any case-insensitive match is present. The error message indicates
#' if such a match was found or not.
#'
#' @section Programming note / To do:
#' - Should paths be [normalized][normalizePath()] (to use in messages, or
#'   before checking existence)? See how that is done in [create_dir()].
#'
#' @seealso
#' [dir.exists()], [file.exists()], and [list.files()] to check for existence of
#' files or directories without checking that it is a unique match to a pattern;
#' [file.info()] and [file.access()] to extract information about files or
#' directories.
#'
#' @family
#' functions to check and modify paths and directories
#'
#' @examples
#' # Create filenames in a temporary directory so we know what is present.
#' my_tempfiles <- tempfile(pattern = c("FirstFile", "SecondFile"), fileext = ".txt")
#' # Create the files
#' file.create(my_tempfiles)
#'
#' check_file(dir = tempdir(), pattern = "First")
#' # The same file is found if case-insensitive matching is used:
#' check_file(dir = tempdir(), pattern = "FIRST", ignore_case = TRUE)
#' # Error reporting presence of case-insensitive match.
#' try(check_file(dir = tempdir(), pattern = "FIRST", ignore_case = FALSE))
#' # Error reporting no match found.
#' try(check_file(dir = tempdir(), pattern = "abc", ignore_case = FALSE))
#' # Error because multiple matches are present.
#' try(check_file(dir = tempdir(), pattern = "File"))
#'
#' # Deleting the created temporary files
#' unlink(x = my_tempfiles)
#'
#' @export
check_file <- function(dir = ".", pattern, ignore_case = TRUE, quietly = FALSE) {
  stopifnot(checkinput::is_character(dir), checkinput::is_character(pattern),
            checkinput::is_logical(ignore_case), checkinput::is_logical(quietly))

  if(!dir.exists(dir)) {
    stop("'", dir, "' does not exist!")
  }

  if(!file.info(dir)$isdir) {
    stop("'dir' ('", dir, "') exists but is not a directory!")
  }

  # 'include.dirs' is TRUE to be able to find, and warn about, matches to a
  # directory instead of to a file.
  files_present <- list.files(path = dir, pattern = pattern, all.files = TRUE,
                              ignore.case = ignore_case, include.dirs = TRUE)

  msg_match <- paste0(
    "case-", if(ignore_case) {"in"}, "sensitive matches to pattern '",
    pattern, "' are present in directory '", dir, "'")

  if(length(files_present) > 1L) {
    stop(wrap_text(x = paste0(
      "Multiple ", msg_match, ": ", paste_quoted(files_present), "!"
    )))
  }

  if(length(files_present) == 0L) {
    if(!ignore_case) {
      # Check if any case-insensitive match is present
      match_case_insensitive <- list.files(path = dir, pattern = pattern,
                                           ignore.case = TRUE)

      if(length(match_case_insensitive) == 0L) {
        msg_match <- paste0(
          msg_match, ". No case-insensitive match is present either")
      } else {
        if(length(match_case_insensitive) == 1L) {
          msg_match <- paste0(
            msg_match,
            ". However, a case-insensitive match to 'pattern' is present: ",
            paste_quoted(match_case_insensitive))
        } else {
          msg_match <- paste0(
            msg_match,
            ". However, case-insensitive matches to 'pattern' are present: ",
            paste_quoted(match_case_insensitive))
        }
      }
    }
    stop(wrap_text(x = paste0("No ", msg_match, ".")))
  }

  # Need isTRUE() to work around 'isdir' being NA for temporary files
  if(isTRUE(file.info(files_present)$isdir)) {
    stop("A single match to 'pattern' ('", pattern, "') is present in 'dir' (",
         dir, ")\nbut that match points to a directory, not to a file!")
  }

  if(!quietly) {
    message("Using file ", paste_quoted(files_present))
  }

  files_present
}
