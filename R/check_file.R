#' Check if a file is present
#'
#' Check if a file is present in the working directory.
#'
#' @param pattern Character string containing a [regular expression][base::regex]
#' to match to file names in the [working directory][getwd()].
#' @param ignore_case `TRUE` or `FALSE`: should matching file names be
#' case-insensitive?
#' @param quietly `TRUE` or `FALSE`: should the message with the found file name
#' be suppressed?
#'
#' @returns A character string with a file name matching `pattern`. The first
#' match is used if `pattern` matches multiple names, with a warning. An error
#' is thrown if `pattern` does not match any names.
#'
#' @details
#' If `ignore_case` is `FALSE` and no case-sensitive match is found, it is
#' checked if a case-insensitive match would be found. If such a match is found,
#' it is reported in error message.
#'
#' @seealso [is_file()] [create_dir()]
#'
#' @examples
#' check_file(pattern <- "AMESP") # Using file 'NAMESPACE'
#' check_file(pattern <- "amesp", ignore_case = TRUE) # Using file 'NAMESPACE'
#' check_file(pattern <- "amesp", ignore_case = FALSE) # Error reporting presence of case-insensitive match 'NAMESPACE'
#' check_file(pattern <- "abc", ignore_case = FALSE) # Error reporting no match found.
#' check_file(pattern <- "ICEN") # Multiple matches, using the first.
#'
#' @export
check_file <- function(pattern, ignore_case = TRUE, quietly = FALSE) {
  stopifnot(checkinput::is_character(pattern),
            checkinput::is_logical(ignore_case),
            checkinput::is_logical(quietly))

  files_present <- list.files(pattern = pattern, ignore.case = ignore_case)

  msg_match <- paste0(
    "case-", if(ignore_case) {"in"}, "sensitive matches to pattern ",
    progutils::paste_quoted(pattern), " are present in the current working directory ('",
    getwd(), "')")

  if(length(files_present) == 0L) {
    match_case_insensitive <- NULL
    if(!ignore_case) {
      match_case_insensitive <- list.files(pattern = pattern, ignore.case = TRUE)
    }

    stop(progutils::wrap_text(x = paste0(
      "No ", msg_match,
      if(length(match_case_insensitive) > 0L) {
        paste0(". However, a case-insensitive match to 'pattern' is present: ",
               progutils::paste_quoted(match_case_insensitive))}, "."
    )))
  }

  if(length(files_present) > 1L) {
    warning(progutils::wrap_text(x = paste0(
      "Multiple ", msg_match, ": ", progutils::paste_quoted(files_present),
      ". The first of those will be used!")))
    files_present <- files_present[1L]
  }

  if(!quietly) {
    message("Using file ", progutils::paste_quoted(files_present))
  }

  files_present
}
