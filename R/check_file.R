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
#' A character string with the matching file name if there is exactly one file
#' with a name matching `pattern` in directory `dir`. Otherwise an error is
#' thrown.
#'
#' @details
#' The default `"."` for `dir` indicates the [working directory][getwd()].
#'
#' If `ignore_case` is `FALSE` and no case-sensitive match is found, the error
#' message indicates if any case-insensitive match is present.
#'
#' In contrast to the default of [list.files()], `check_file` also finds
#' 'hidden' files, i.e., files which names start with a dot.
#'
#' Paths will be [normalized][normalizePath()] before use, so the form of paths
#' reported in messages might differ from the input to `dir`.
#'
#' @seealso
#' [create_dir()] to create a directory if does not yet exist; [file.exists()]
#' and [list.files()] to check for existence of files without checking they are
#' a unique match to a pattern; [file.info()] and [file.access()] to extract
#' information about files or directories; [file.path()] to construct file paths
#' in a platform-independent way; [normalizePath()] to create absolute paths.
#'
#' @family
#' functions to check paths and create directories
#'
#' @examples
#' # Create files in a temporary directory so we know what is present.
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
#' try(check_file(dir = tempdir(), pattern = "abcde", ignore_case = TRUE))
#' try(check_file(dir = tempdir(), pattern = "abcde", ignore_case = FALSE))
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

  dir <- normalizePath(dir, mustWork = FALSE)
  if(!dir.exists(dir)) {
    if(file.exists(dir)) {
      stop("The path in 'dir' ('", dir,
           "') points to a file but should point to a directory!")
    } else {
      stop("'", dir, "' does not exist!")
    }
  }

  files_present <- list.files(path = dir, pattern = pattern, all.files = TRUE,
                              ignore.case = ignore_case)
  # 'list.files()' also returns directories (even though 'include.dirs' is FALSE
  # by default) because 'recursive' is also FALSE.
  if(length(files_present) > 0L) {
    files_present <- not_in(files_present,
                            list.dirs(path = dir, full.names = FALSE,
                                      recursive = FALSE))
  }

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

  if(!quietly) {
    message("Using file ", paste_quoted(files_present))
  }

  files_present
}
