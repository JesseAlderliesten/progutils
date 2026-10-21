#' Create a Directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir [character string][checkinput::is_character()] containing a
#' [valid path][checkinput::is_path()], see `Details` and the section
#' `Notes on Paths` below.
#' @param add_date `TRUE` or `FALSE`: create a subdirectory in `dir` with the
#' current date in the [format][strftime()] `YYYY_mm_dd`?
#'
#' @details
#' The path in `dir` is interpreted as a **relative** path to the directory that
#' should be created, starting from the [working directory][getwd()], see the
#' section `Notes on Paths` below.
#'
#' @returns
#' A character string with the [absolute normalised][fs::path_abs()] path to the
#' requested directory, returned [invisibly][invisible], see the section
#' `Notes on Paths` and the `Examples` below.
#'
#' By default a subdirectory with the current date in the [format][strftime()]
#' `YYYY_mm_dd` is created in directory `output` below the
#' [working directory][getwd()].
#'
#' An error is thrown if the attempt to create a directory fails, for example
#' because `dir` points to an existing file instead of to an directory.
#'
#' @section Side Effects:
#' The directory indicated by the returned path is [created][fs::dir_create()]
#' if it does not yet [exist][fs::dir_exists()].
#'
#' @inheritSection checkinput::is_path Notes on Paths
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, with extensive
#' references about file paths and directories;
#' [create_tempdir()] for a safe way to create temporary directories;
#' [create_file_path()] to create a file path and creating the indicated
#' directory if it does not yet exist;
#' [fs::dir_exists()] and [fs::dir_create()] (with its base-equivalent
#' [dir.create()]) used by this function.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary directory to not write in the user's directory
#' my_tempdir <- create_tempdir(prefix = "examplecreatedir")
#'
#' # Create directory 'dir_one' inside this temporary directory
#' res_dir_one <- create_dir(dir = fs::path(my_tempdir, "dir_one"),
#'                           add_date = FALSE)
#' fs::dir_exists(res_dir_one) # TRUE
#'
#' # An attempt to create a directory that already exists does
#' # not change any directory and the same directory is returned.
#' res_dir_one_v2 <- create_dir(
#'   dir = fs::path(my_tempdir, "dir_one"),
#'   add_date = FALSE)
#' identical(res_dir_one, res_dir_one_v2) # TRUE
#'
#' # On case-insensitive file systems, the directories
#' # 'res_dir_ONE' and 'res_dir_one' are the same directory. On
#' # case-sensitive file systems they are different directories.
#' res_dir_ONE <- create_dir(dir = fs::path(my_tempdir, "dir_ONE"),
#'                           add_date = FALSE)
#' identical(res_dir_one, res_dir_ONE)
#'
#' # Create directory 'dir_two' with a subdirectory containing
#' # the current date
#' res_dir_date <- create_dir(
#'   dir = fs::path(my_tempdir, "dir_date"), add_date = TRUE)
#' fs::dir_exists(res_dir_date) # TRUE
#'
#' # Cleaning up
#' unlink(my_tempdir, recursive = TRUE)
#' rm(my_tempdir, res_dir_one, res_dir_one_v2,
#'    res_dir_date, res_dir_ONE)
#'
#' @export
create_dir <- function(dir = fs::path(".", "output"),
                       add_date = TRUE) {
  stopifnot(checkinput::is_path(dir),
            checkinput::is_logical(add_date))

  if(add_date) {
    dir <- fs::path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  dir <- fs::path_abs(dir)

  # fs::dir_exists() returns FALSE if 'dir' is a file instead of a
  # directory but fs::dir_create() then throws an informative error.
  if(!fs::dir_exists(dir)) {
    # By default 'recurse' is TRUE to allow creation of subdirectories inside a
    # not-yet existing directory (e.g., creating './output/<date>' if './output'
    # does not yet exist).
    dir <- fs::dir_create(path = dir)
  }

  invisible(dir)
}
