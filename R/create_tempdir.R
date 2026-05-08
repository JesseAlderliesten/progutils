#' Create a temporary directory
#'
#' Create a temporary directory that can safely be removed.
#'
#' @param subdir A [character string][checkinput::is_character()] with the
#' subdirectory to create inside [tempdir()].
#' @param error_if_exists `TRUE` or `FALSE`: throw an error if the temporary
#' directory already exists? The default `TRUE` prevents deleting files if the
#' created temporary directory is programmatically deleted later on.
#'
#' @details
#' The temporary directory is created as a subdirectory inside `tempdir()` to
#' ensure that [removing][unlink()] the created directory does not give problems:
#' removing the directory returned by `tempdir()` can give problems, for example
#' because `RStudio` also uses that directory.
#'
#' It is possible to create subdirectories inside a not-yet existing directory
#' (e.g., to create `<tempdir>/output/<date>` if `<tempdir>/output` does not yet
#' exist.
#'
#' @returns
#' The [normalized][normalizePath()] path to the created temporary directory,
#' returned [invisibly][invisible].
#'
#' @section Side effects:
#' The requested temporary directory is created if does not yet exist.
#'
#' @seealso
#' [create_dir()] to create (non-temporary) directories.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' tempdir()
#' # Create a directory inside the directory returned by 'tempdir()'
#' (create_tempdir())
#'
#' # Error if the directory already exists and 'error_if_exists' is 'TRUE'
#' try(create_tempdir(error_if_exists = TRUE))
#' (create_tempdir(error_if_exists = FALSE))
#'
#' @export
create_tempdir <- function(subdir = "subdir", error_if_exists = TRUE) {
  stopifnot(checkinput::is_character(subdir),
            checkinput::is_logical(error_if_exists))
  dir <- normalizePath(path = file.path(tempdir(), subdir),
                       winslash = "/",
                       mustWork = FALSE)

  # dir.exists() returns FALSE if 'dir' is a file instead of a directory.
  if(!dir.exists(dir)) {
    # Notes:
    # - This branch is only used if the directory did not yet exist as directory,
    #   so it is not a problem that dir.create() returns FALSE if a directory
    #   already exists. However, the path can already exist as a file: then the
    #   attempt to create it as a directory will fail, resulting in an error.
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating '<tempdir>/output/<date>' if
    #   '<tempdir>/output' does not yet exist).
    if(!dir.create(path = dir, recursive = TRUE, showWarnings = TRUE)) {
      stop("Attempt to create a subdirectory in the temporary directory failed: ",
           dir)
    }
  } else (
    if(error_if_exists) {
      stop("Temporary directory ", dir, " already exists!\nAdjust 'subdir', or",
           " set 'error_if_exists' to 'FALSE' if you know that is safe.")
    }
  )
  invisible(dir)
}
