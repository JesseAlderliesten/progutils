#' Check if only a single matching file is present
#'
#' Check if only a single matching file is present. This function is intended to
#' be used before reading a file. Throwing an error if multiple matches are
#' present, instead of using the first matching file, is intended to prevent
#' erroneous matches.
#'
#' @param path Character string with a full path name. Passed to [list.files()].
#' @param pattern NULL, or a character string containing a
#' [regular expression][base::regex] to match to file names in the path
#' indicated by `path`. Passed to [list.files()].
#' @param ignore_case `TRUE` or `FALSE`: should matching file names be
#' case-insensitive?
#' @param quietly `TRUE` or `FALSE`: should the message with the found file name
#' be suppressed?
#'
#' @returns A character string with the file name in `dir` that matches
#' `pattern` if there is exactly one match to `pattern`. An error is thrown if
#' no file name in `dir` matches `pattern` or if multiple file names in `dir`
#' match `pattern`.
#'
#' @details
#' The default `"."` for `path` indicates the [working directory][getwd()].
#'
#' If `ignore_case` is `FALSE` and no case-sensitive match is found, it is
#' checked if a case-insensitive match would be found. It is reported in the
#' error message if such a match is found or not.
#'
#' @seealso [list.files()] that is used by this function. [create_dir()] to
#' create a directory.
#'
#' @examples
#' # Create filenames in a temporary directory so we know what is present.
#' my_tempfiles <- tempfile(pattern = c("FirstFile", "SecondFile"), fileext = ".txt")
#' # Create the files
#' file.create(my_tempfiles)
#'
#' check_file(path = tempdir(), pattern = "First")
#' # Default for 'ignore_case' is TRUE, so the same file is found:
#' check_file(path = tempdir(), pattern = "FIRST")
#' # Error reporting presence of case-insensitive match.
#' try(check_file(path = tempdir(), pattern = "FIRST", ignore_case = FALSE))
#' # Error reporting no match found.
#' try(check_file(path = tempdir(), pattern = "abc", ignore_case = FALSE))
#' # Error because multiple matches are present.
#' try(check_file(path = tempdir(), pattern = "File"))
#'
#' # Deleting the created temporary files
#' unlink(x = my_tempfiles)
#'
#' @export
check_file <- function(path = ".", pattern = NULL, ignore_case = TRUE,
                       quietly = FALSE) {
  stopifnot(checkinput::is_character(path),
            is.null(pattern) || checkinput::is_character(pattern),
            checkinput::is_logical(ignore_case),
            checkinput::is_logical(quietly))

  files_present <- list.files(path = path, pattern = pattern,
                              ignore.case = ignore_case)

  msg_match <- paste0(
    "case-", if(ignore_case) {"in"}, "sensitive matches to pattern ",
    progutils::paste_quoted(pattern), " are present in directory '",
    path, "')")

  if(length(files_present) == 0L) {
    if(!ignore_case) {
      match_case_insensitive <- list.files(path = path, pattern = pattern,
                                           ignore.case = TRUE)

      if(length(match_case_insensitive) > 0L) {
        msg_match <- paste0(
          msg_match,
          ". However, a case-insensitive match to 'pattern' is present: ",
          progutils::paste_quoted(match_case_insensitive))
      } else {
        msg_match <- paste0(
          msg_match, ". No case-insensitive match is present either")
      }
    }
    stop(progutils::wrap_text(x = paste0("No ", msg_match, ".")))
  }

  if(length(files_present) > 1L) {
    stop(progutils::wrap_text(x = paste0(
      "Multiple ", msg_match, ": ", progutils::paste_quoted(files_present),
      "!")))
  }

  if(!quietly) {
    message("Using file ", progutils::paste_quoted(files_present))
  }

  files_present
}
