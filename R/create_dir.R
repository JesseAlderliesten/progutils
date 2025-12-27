#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir Non-empty character string containing the path to the directory,
#' not ending with a (back)slash. See `Details`.
#' @param add_date `TRUE` or `FALSE`: should a subdirectory be created with the
#' current date?
#' @param quietly `TRUE` or `FALSE`: should the printing of messages indicating
#' which directory was created be suppressed?
#'
#' @details
#' The default `dir` uses [file.path()] to use the correct (platform-dependent)
#' path separator when indicating subdirectory `output` in the
#' [working directory][getwd()]. That is not the case if a character string with
#' (back)slashes (`/` or `\`) is used to indicate subdirectories.
#'
#' @returns A character string with the created path, returned
#' [invisibly][invisible]. The path will be composed of the elements in `dir`,
#' separated with the platform-dependent [file separator][file.path()] and, if
#' `add_date` is `TRUE`, a subdirectory with the current date in the format
#' `YYYY_MM_DD`. The [working directory][getwd()] is returned if the attempt to
#' create a directory fails, with a warning.
#'
#' @section Side effects:
#' The requested directory is created if does not yet exist.
#'
#' @section Wishlist:
#' Use a temporary directory for the unit tests, to solve the problem that tests
#' create directories that are left. See the suggestions in test_create_dir.R.
#'
#' @seealso [file.path()] and [dir.create()] that are used in this function.
#' [check_file()] to check if a file exists.
#'
#' @examples
#'
#' @export
create_dir <- function(dir = file.path(".", "output"), add_date = TRUE,
                       quietly = FALSE) {
  stopifnot(checkinput::is_character(dir),
            # 'dir' should not end with a (back)slash: that would result in a
            # mismatch between the created directory and the returned path
            # because trailing (back)slashes are removed when the directory is
            # created.
            "'dir' should not end with '/'" =
              substring(text = dir, first = nchar(dir)) != "/",
            "'dir' should not end with '\\'" =
              substring(text = dir, first = nchar(dir)) != "\\",
            checkinput::is_logical(add_date), checkinput::is_logical(quietly))

  if(add_date) {
    dir <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  if(dir.exists(dir)) {
    if(!quietly) {
      message("Directory '", dir, "' already exists.")
    }
  } else {
    # Notes:
    # - This branch is only used if the directory did not yet exist, so it is
    #   not a problem that dir.create() returns FALSE if a directory already
    #   exists.
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating './output/<date>' if
    #   './output' does not yet exist).
    if(dir.create(path = dir, recursive = TRUE)) {
      if(!quietly) {
        message("Created directory '", dir, "'.")
      }
    } else {
      warning(wrap_text(paste0(
        "Attempt to create directory '", dir, "' failed",
        if(file.exists(dir)) {": it points to a file, not to a directory"},
        "!\nReturning working directory ('", getwd(), "') instead.")))
      dir <- getwd()
    }
  }

  invisible(dir)
}
