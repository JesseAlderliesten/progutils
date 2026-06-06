#' Create a temporary directory
#'
#' Create a temporary directory that can safely be removed.
#'
#' @param subdir A [character string][checkinput::is_character()] with the name
#' of a **not-yet** existing temporary subdirectory to be created inside
#' [tempdir()]. `subdir` should be a [valid path][checkinput::is_path()].
#'
#' @details
#' `subdir` is created inside [tempdir()] and an error is thrown if it already
#' exists or creating the directory fails. This ensures that [removing][unlink()]
#' the created directory does not remove files that are still needed by other
#' processes (see `Usage in practice` below).
#'
#' It is possible to create subdirectories inside a not-yet existing directory
#' (e.g., to create `<tempdir>/output/outputsub` if `<tempdir>/output` does not
#' yet exist.
#'
#' @returns
#' The [absolute normalized][fs::path_abs()] path to the created temporary
#' directory, returned [invisibly][invisible].
#'
#' @section Side effects:
#' The temporary directory indicated by the returned path is
#' [created][dir.create()].
#'
#' @section Usage in practice:
#' Examples and tests should **not** write to the [working directory][getwd()]
#' but to a temporary directory that is cleaned up afterwards. Although
#' [tempdir()] points to a temporary directory, that directory should **not** be
#' removed because [RStudio](https://posit.co/products/open-source/rstudio) also
#' uses it. Instead, store the paths to temporary files written to [tempdir()]
#' and [unlink][unlink()] those paths when cleaning up, or create a temporary
#' subdirectory in `tempdir()` which can be completely be removed when cleaning
#' up. `use_tempdir()` follows the latter approach. It is good practice to
#' create the complete temporary directory with all required files and folders
#' before running any example or test: this ensures no spurious matches occur if
#' examples or tests are removed or added.
#'
#' @seealso
#' [create_dir()] to create (non-temporary) directories;
#' [checkinput::is_path()] to check if a path is valid, and the 'Note on paths'
#' in its documentation.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' tempdir()
#' # Create a directory inside the directory returned by 'tempdir()'
#' (tempdir_std <- create_tempdir(subdir = "examplesubtempdir"))
#'
#' # Error if the directory already exists
#' try(create_tempdir(subdir = "examplesubtempdir"))
#'
#' # It is possible to create recursive directories
#' (tempdir_recursive <- create_tempdir(subdir = fs::path("abc", "def")))
#'
#' # Clean up
#' unlink(c(tempdir_std, dirname(tempdir_recursive)), recursive = TRUE)
#' rm(tempdir_recursive, tempdir_std)
#'
#' @export
create_tempdir <- function(subdir = "subdir") {
  stopifnot(checkinput::is_character(subdir),
            checkinput::is_path(subdir, require_sep = FALSE))
  subdir_target <- fs::path_abs(path = fs::path(tempdir(), subdir))
  tempdir_normalised <- fs::path_abs(tempdir())
  if(!grepl(pattern = basename(tempdir()), x = subdir_target, fixed = TRUE)) {
    stop("Using ", paste_quoted(subdir),
         " as 'subdir' would write above 'tempdir()' which is not safe: ",
         subdir_target)
  }

  if(basename(subdir_target) == basename(tempdir())) {
    stop("Using ", paste_quoted(subdir),
         " as 'subdir' would write to 'tempdir()' which is not safe: ",
         tempdir_normalised)
  }

  # fs::dir_exists() returns FALSE if 'subdir_target' is a file instead of a
  # directory.
  if(!fs::dir_exists(subdir_target)) {
    # Notes:
    # - This branch is only used if the directory did not yet exist as directory,
    #   so it is not a problem that dir.create() returns FALSE if a directory
    #   already exists. However, the path can already exist as a file: then the
    #   attempt to create it as a directory will fail, resulting in an error.
    # - Not using fs::dir_create() because that returns the path instead of a
    #   boolean vector indicating if creation succeeded.
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating '<tempdir>/output/<date>' if
    #   '<tempdir>/output' does not yet exist).
    if(!dir.create(path = subdir_target, recursive = TRUE,
                   showWarnings = TRUE)) {
      stop("Attempt to create a subdirectory in the temporary directory failed:\n",
           subdir_target)
    }
  } else {
    stop("Temporary directory already exists: change 'subdir' (",
         paste_quoted(subdir), "):\n", subdir_target)
  }
  invisible(subdir_target)
}
