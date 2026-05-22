#' Create a temporary directory
#'
#' Create a temporary directory that can safely be removed.
#'
#' @param subdir A [character string][checkinput::is_character()] with the name
#' of a **not-yet** existing temporary subdirectory to be created inside
#' [tempdir()]. `subdir` should meet the requirements for a valid
#' [path][is_path()].
#'
#' @details
#' `subdir` is created inside [tempdir()] and an error is thrown if it already
#' exists. This ensures that [removing][unlink()] the created directory does not
#' remove files that are still needed by other processes (see `Usage` below).
#'
#' It is possible to create subdirectories inside a not-yet existing directory
#' (e.g., to create `<tempdir>/output/outputsub` if `<tempdir>/output` does not
#' yet exist.
#'
#' @returns
#' The [normalized][normalizePath()] path to the created temporary directory,
#' returned [invisibly][invisible].
#'
#' @section Side effects:
#' The requested temporary directory is created if does not yet exist. An error
#' is thrown if the directory already exists or creating the directory fails.
#'
#' @section Usage:
#' Examples and tests should **not** write to the [working directory][getwd()]
#' but to a temporary directory that is cleaned up afterwards. Although
#' [tempdir()] points to a temporary directory, that directory should **not** be
#' removed because [RStudio](https://posit.co/products/open-source/rstudio) also
#' uses it. Instead, store the paths to temporary files written to [tempdir()]
#' and [unlink][unlink()] those paths when cleaning up, or create a temporary
#' subdirectory in `tempdir()` which can be completely be removed when cleaning
#' up. `use_tempdir()` follows the latter approach.
#'
#' @seealso
#' [create_dir()] to create (non-temporary) directories.
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
#' (tempdir_recursive <- create_tempdir(subdir = file.path("abc", "def")))
#'
#' # Clean up
#' unlink(c(tempdir_std, dirname(tempdir_recursive)), recursive = TRUE)
#' rm(tempdir_recursive, tempdir_std)
#'
#' @export
create_tempdir <- function(subdir = "subdir") {
  stopifnot(checkinput::is_character(subdir))
  is_path(subdir)
  subdir_target <- normalizePath(path = file.path(tempdir(), subdir),
                                 winslash = "/",
                                 mustWork = FALSE)

  tempdir_normalised <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  if(!grepl(pattern = basename(tempdir()), x = subdir_target, fixed = TRUE)
      # startsWith(subdir_target, prefix = tempdir_normalised)
     ) {
    stop("Using ", paste_quoted(subdir),
         " as 'subdir' would write above 'tempdir()' which is not safe: ",
         subdir_target)
  }

  if(subdir_target == tempdir_normalised) {
    stop("Using ", paste_quoted(subdir),
         " as 'subdir' would write to 'tempdir()' which is not safe: ",
         tempdir_normalised)
  }

  # dir.exists() returns FALSE if 'subdir_target' is a file instead of a directory.
  if(!dir.exists(subdir_target)) {
    # Notes:
    # - This branch is only used if the directory did not yet exist as directory,
    #   so it is not a problem that dir.create() returns FALSE if a directory
    #   already exists. However, the path can already exist as a file: then the
    #   attempt to create it as a directory will fail, resulting in an error.
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating '<tempdir>/output/<date>' if
    #   '<tempdir>/output' does not yet exist).
    if(!dir.create(path = subdir_target, recursive = TRUE, showWarnings = TRUE)) {
      stop("Attempt to create a subdirectory in the temporary directory failed: ",
           subdir_target)
    }
  } else {
    stop("Temporary directory already exists: change 'subdir' ('", subdir,
         "'): ", subdir_target)
  }
  invisible(subdir_target)
}
