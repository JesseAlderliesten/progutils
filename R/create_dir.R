#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir [Character string][checkinput::is_character()]
#' containing a [valid path][checkinput::is_path()] to a directory that should
#' be created if it does not yet exist. [fs::path()] adds file separators and
#' the dot (`"."`) indicates the [working directory][getwd()], such that by
#' default a subdirectory with the current date in the [format][strftime()]
#' `YYYY_mm_dd` in directory `output` below the working directory is created.
#' @param add_date `TRUE` or `FALSE`: create a subdirectory in `dir` with the
#' current date in the [format][strftime()] `YYYY_mm_dd`?
#'
#' @details
#' The [absolute, normalised][fs::path_abs()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. On
#' case-insensitive file systems, normalization also adjusts the case to match
#' names of directories that are already present (see the `Examples`).
#'
#' @returns
#' A character string with the [absolute normalized][fs::path_abs()] path to the
#' requested directory, returned [invisibly][invisible]. An error is thrown if
#' the attempt to create a directory fails, for example because `dir` points to
#' an existing file instead of to an directory.
#'
#' @section Side effects:
#' The directory indicated by the returned path is [created][fs::dir_create()]
#' if it does not yet exist.
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, with a `Note on paths`
#' and extensive references about file paths and directories;
#' [create_tempdir()] for a safe way to create temporary directories;
#' [create_file_path()] to create a file path and creating the indicated
#' directory if it does not yet exist;
#' [fs::dir_exists()] and [fs::dir_create()] (with its base-equivalent
#' [dir.create()]) used by this function;
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary subdirectory to not write in the user's directory
#' my_tempdir <- create_tempdir(pattern = "examplecreatedir")
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
#' # On case-insensitive file systems, the directory 'res_dir_ONE' is the same as
#' # 'res_dir_one'. On case-sensitive file systems it differs in case from
#' # 'res_dir_one'.
#' res_dir_ONE <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
#'                           add_date = FALSE)
#' identical(res_dir_one, res_dir_ONE)
#'
#' # Create directory 'dir_two' with a subdirectory containing the current date
#' res_dir_date <- create_dir(dir = fs::path(my_tempdir, "dir_date"),
#'                            add_date = TRUE)
#' fs::dir_exists(res_dir_date) # TRUE
#'
#' # Cleaning up
#' unlink(my_tempdir, recursive = TRUE)
#' rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_date, res_dir_ONE)
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
