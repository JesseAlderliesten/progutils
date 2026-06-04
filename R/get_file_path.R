#' Check that only a single matching file is present
#'
#' Check that only one file in a directory has a name matching `pattern`, for
#' example before attempting to [read a file][utils::read.table()].
#'
#' @param dir Character string with the [path][checkinput::is_path()] to a
#' directory.
#' @param pattern Character string containing a [regular expression][base::regex]
#' used to select names of files that are present in `dir`.
#' @param ignore_case `TRUE` or `FALSE`: use case-insensitive pattern matching?
#' @param quietly `TRUE` or `FALSE`: suppress the message with the found file
#' name?
#'
#' @details
#' The default `dir` (`"."`) indicates the [working directory][getwd()].
#'
#' If `ignore_case` is `FALSE` and no case-sensitive match is found, the error
#' message indicates if any case-insensitive match is present.
#'
#' In contrast to the default of [list.files()], `get_file_path()` also finds
#' 'hidden' files, i.e., files with names that start with a dot.
#'
#' Paths will be [normalized][fs::path_abs()] to ensure they still work if the
#' [working directory][getwd()] changes.
#'
#' @returns
#' A character string with the [absolute normalized][fs::path_abs()] path to the
#' file with a name matching `pattern` if there is exactly one such file in
#' directory `dir`. Otherwise an error is thrown. Use [basename()] on the result
#' to obtain the filename itself.
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, and the 'Note on paths'
#' in its documentation;
#' [create_dir()] to create a directory if does not yet exist;
#' [file.exists()] and [list.files()] to check for existence of files without
#' checking they are a unique match to a pattern;
#' [file.info()] and [file.access()] to extract information about files or
#' directories;
#' [fs::path()] to construct file paths in a platform-independent way;
#' [fs::path_abs()] to create absolute paths.
#'
#' @family functions to handle paths and directories
#' @family functions to check equality
#'
#' @examples
#' # Create files in a temporary directory so we know what is present.
#' my_tempfiles <- tempfile(pattern = c("some_filename", "another_filename"),
#'                          fileext = ".txt")
#' # Create the files
#' file.create(my_tempfiles)
#'
#' get_file_path(dir = tempdir(), pattern = "some_file")
#'
#' # The same file is found if case-insensitive matching is used:
#' get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = TRUE)
#'
#' # Error reporting the presence of a case-insensitive match.
#' try(get_file_path(dir = tempdir(), pattern = "SOME_FILE", ignore_case = FALSE))
#'
#' # Error reporting no match found.
#' try(get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
#'                  ignore_case = TRUE))
#' try(get_file_path(dir = tempdir(), pattern = "missing_filename_abcde",
#'                  ignore_case = FALSE))
#'
#' # Error if multiple matches are present.
#' try(get_file_path(dir = tempdir(), pattern = "_filename"))
#'
#' # Clean up
#' unlink(x = my_tempfiles)
#' rm(my_tempfiles)
#'
#' @export
get_file_path <- function(dir = ".", pattern, ignore_case = TRUE,
                          quietly = FALSE) {
  stopifnot(checkinput::is_character(dir), checkinput::is_path(dir),
            checkinput::is_character(pattern),
            checkinput::is_logical(ignore_case), checkinput::is_logical(quietly))

  dir <- fs::path_abs(dir)
  if(!fs::dir_exists(dir)) {
    stop("Directory does not exist:\n", paste_quoted(dir))
  }

  files_present <- fs::path_abs(
    list.files(path = dir, pattern = pattern, all.files = TRUE,
               full.names = TRUE, ignore.case = ignore_case)
  )

  # 'list.files()' also returns directories (even though 'include.dirs' is FALSE
  # by default) because 'recursive' is also FALSE.
  if(length(files_present) > 0L) {
    dirs_present <- list.dirs(path = dir, full.names = TRUE, recursive = FALSE)
    if(length(dirs_present) > 0L) {
      files_present <- not_in(files_present, dirs_present)
    }
  }

  msg_match <- paste0(
    if(!ignore_case) {"case-sensitive "}, "matches to pattern '",
    pattern, "' are present in directory\n'", dir, "'")

  if(length(files_present) > 1L) {
    stop(paste0("Multiple ", msg_match, ": ",
                paste_quoted(basename(files_present)), "!"))
  }

  if(length(files_present) == 0L) {
    if(!ignore_case) {
      # Check if any case-insensitive match is present
      match_case_insensitive <- list.files(path = dir, pattern = pattern,
                                           ignore.case = TRUE)

      if(length(match_case_insensitive) == 0L) {
        msg_match <- paste0(
          msg_match, ".\nNo case-insensitive match is present either")
      } else {
        if(length(match_case_insensitive) == 1L) {
          msg_match <- paste0(
            msg_match,
            ".\nHowever, a case-insensitive match to 'pattern' is present: ",
            paste_quoted(basename(match_case_insensitive)))
        } else {
          msg_match <- paste0(
            msg_match,
            ".\nHowever, case-insensitive matches to 'pattern' are present: ",
            paste_quoted(basename(match_case_insensitive)))
        }
      }
    }
    stop("No ", msg_match, ".")
  }

  if(!quietly) {
    message("Using file ", paste_quoted(files_present))
  }

  files_present
}
