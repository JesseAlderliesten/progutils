#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir Non-empty [character string][checkinput::is_character()]
#' containing a [valid path][is_path()] to a directory that should be created if
#' it does not yet exist. [file.path()] ensures the correct
#' ([platform][.Platform]-dependent) file separator is used to indicate
#' subdirectories and the dot (`"."`) indicates the [working directory][getwd()],
#' such that by default a subdirectory with the current date in the
#' [format][strftime()] `YYYY_mm_dd` in directory `output` below the working
#' directory is created.
#'
#' @param add_date `TRUE` or `FALSE`: create a subdirectory in `dir` with the
#' current date in the [format][strftime()] `YYYY_mm_dd`?
#'
#' @details
#' If creating the directory fails, the working directory is returned instead.
#' This happens if `dir` points to an existing file instead of an directory.
#'
#' The absolute [normalised][normalizePath()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. On
#' case-insensitive file systems (e.g., Windows and macOS), normalization
#' adjusts the case to match case-insensitive names of directories that are
#' already present (see the `Examples`). `"/"` instead of `"\\"` is used as
#' [winslash][normalizePath()] during normalisation, such that the returned path
#' can be used in Windows' file system. Trailing `\\` in the input is replaced
#' by a trailing `/`, but a trailing `/` in the input is removed.
#'
#' @returns
#' A character string with the absolute [normalized][normalizePath()] path to
#' the requested directory, returned [invisibly][invisible]. The
#' [working directory][getwd()] is returned if an attempt to create a directory
#' fails, with a warning.
#'
#' @section Side effects:
#' The directory indicated by the returned path is [created][create_dir()] if it
#' does not yet exist.
#'
#' @seealso
#' [create_file_path()] to create a file path and creating the indicated
#' directory if it does not yet exist; [create_tempdir()] for a safe way
#' to create temporary directories; [is_path()] and references there about file
#' paths and directories; [dir.exists()] and [dir.create()] used by this
#' function; [get_file_path()] to check if a file exists and is a unique match to
#' a pattern.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary subdirectory to not write in the user's directory
#' my_tempdir <- file.path(tempdir(), "testcreatedir")
#'
#' # Create directory 'dir_one' inside this temporary directory
#' res_dir_one <- create_dir(dir = file.path(my_tempdir, "dir_one"),
#'                           add_date = FALSE)
#' dir.exists(res_dir_one) # TRUE
#'
#' # An attempt to create a directory that already exists does not change any
#' # directory and the same directory is returned.
#' res_dir_one_v2 <- create_dir(dir = file.path(my_tempdir, "dir_one"),
#'                              add_date = FALSE)
#' identical(res_dir_one, res_dir_one_v2) # TRUE
#'
#' # On case-insensitive file systems such as Windows and macOS, adding
#' # 'dir_ONE' to the directory gives the same result as adding 'dir_one' as
#' # done above for 'res_dir_one'
#' res_dir_one_v3 <- create_dir(dir = file.path(my_tempdir, "dir_ONE"),
#'                              add_date = FALSE)
#' # TRUE on Windows and macOS, FALSE on Ubuntu
#' identical(res_dir_one, res_dir_one_v3)
#'
#' # Create directory 'dir_two' with a subdirectory containing the current date
#' res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
#'                           add_date = TRUE)
#' dir.exists(res_dir_two) # TRUE
#'
#' # Cleaning up
#' unlink(dirname(res_dir_one), recursive = TRUE)
#' rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two, res_dir_one_v3)
#'
#' @export
create_dir <- function(dir = file.path(".", "output"), add_date = TRUE) {
  stopifnot(is_path(dir), checkinput::is_logical(add_date))

  if(add_date) {
    dir <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)

  # dir.exists() returns FALSE if 'dir' is a file instead of a directory.
  if(!dir.exists(dir)) {
    # Notes:
    # - This branch is only used if the directory did not yet exist as directory,
    #   so it is not a problem that dir.create() returns FALSE if a directory
    #   already exists. However, the path can already exist as a file: then the
    #   attempt to create it as a directory will fail (indicated by a warning)
    #   and the working directory will be used instead (indicated by an
    #   additional warning).
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating './output/<date>' if
    #   './output' does not yet exist).
    if(!dir.create(path = dir, recursive = TRUE)) {
      warning("Attempt to create directory failed (perhaps it points to an",
              " existing file?):\n", dir,
              "\nReturning the working directory instead:\n", getwd())
      dir <- normalizePath(getwd(), winslash = "/", mustWork = NA)
    }
  }

  invisible(dir)
}
