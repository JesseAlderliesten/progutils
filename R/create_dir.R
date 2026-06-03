#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir [Character string][checkinput::is_character()]
#' containing a [valid path][checkinput::is_path()] to a directory that should be created if
#' it does not yet exist. [fs::path()] adds file separators and the dot (`"."`)
#' indicates the [working directory][getwd()],
#' such that by default a subdirectory with the current date in the
#' [format][strftime()] `YYYY_mm_dd` in directory `output` below the working
#' directory is created.
#'
#' @param add_date `TRUE` or `FALSE`: create a subdirectory in `dir` with the
#' current date in the [format][strftime()] `YYYY_mm_dd`?
#'
#' @details
#' The [absolute normalised][fs::path_abs()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. On
#' case-insensitive file systems (e.g., Windows and macOS), normalization
#' adjusts the case to match case-insensitive names of directories that are
#' already present (see the `Examples`).
#'
#' @returns
#' A character string with the [absolute normalized][fs::path_abs()] path to
#' the requested directory, returned [invisibly][invisible]. An error is thrown
#' if the attempt to create a directory fails. This happens if `dir` points to
#' an existing file instead of an directory.
#'
#' @section Side effects:
#' The directory indicated by the returned path is [created][create_dir()] if it
#' does not yet exist.
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, and the 'Note on paths'
#' in its documentation;
#' [create_file_path()] to create a file path and creating the indicated
#' directory if it does not yet exist;
#' [create_tempdir()] for a safe way to create temporary directories;
#' [checkinput::is_path()] and references there about file paths and directories;
#' [fs::dir_exists()] and [fs::dir_create()] used by this function (and the
#' base-equivalent of the latter: [dir.create()]);
#' [get_file_path()] to check if a file exists and is a unique match to a
#' pattern.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary subdirectory to not write in the user's directory
#' my_tempdir <- fs::path(tempdir(), "testcreatedir")
#'
#' # Create directory 'dir_one' inside this temporary directory
#' res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
#'                           add_date = FALSE)
#' fs::dir_exists(res_dir_one) # TRUE
#'
#' # An attempt to create a directory that already exists does not change any
#' # directory and the same directory is returned.
#' res_dir_one_v2 <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
#'                              add_date = FALSE)
#' identical(res_dir_one, res_dir_one_v2) # TRUE
#'
#' # On case-insensitive file systems such as Windows and macOS, adding
#' # 'dir_ONE' to the directory gives the same result as adding 'dir_one' as
#' # done above for 'res_dir_one'
#' res_dir_one_v3 <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
#'                              add_date = FALSE)
#' # TRUE on Windows and macOS, FALSE on Ubuntu
#' identical(res_dir_one, res_dir_one_v3)
#'
#' # Create directory 'dir_two' with a subdirectory containing the current date
#' res_dir_two <- create_dir(dir = fs::path(my_tempdir, "dir_two"),
#'                           add_date = TRUE)
#' fs::dir_exists(res_dir_two) # TRUE
#'
#' # Cleaning up
#' unlink(dirname(res_dir_one), recursive = TRUE)
#' rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_two, res_dir_one_v3)
#'
#' @export
create_dir <- function(dir = fs::path(".", "output"), add_date = TRUE) {
  stopifnot(checkinput::is_character(dir), checkinput::is_path(dir),
            checkinput::is_logical(add_date))

  if(add_date) {
    dir <- fs::path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  dir <- fs::path_abs(dir)

  # fs::dir_exists() returns FALSE if 'dir' is a file instead of a directory but
  # fs::dir_create() then throws an informative error.
  if(!fs::dir_exists(dir)) {
    # By default 'recurse' is TRUE to allow creation of subdirectories inside a
    # not-yet existing directory (e.g., creating './output/<date>' if './output'
    # does not yet exist).
    fs::dir_create(path = dir)
  }

  invisible(dir)
}
