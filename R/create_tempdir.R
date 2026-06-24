#' Create a temporary directory
#'
#' Create a temporary directory that can safely be removed.
#'
#' @param prefix [character string][checkinput::is_character()] with the
#' initial part of the name of the new temporary directory, only containing
#' characters that are valid in a [path][checkinput::is_path()].
#'
#' @details
#' The new directory is [created][dir.create()] inside [tempdir()]. Its
#' [name][tempfile()] starts with the string given by `prefix` and is followed
#' by a random string in hex. This random string **might** contain a dot (`.`)
#' such that the directory might appear to have a file extension, see the
#' section `Source` in `help("tempdir")`. An error is thrown if creating the
#' directory fails.
#'
#' It is **not** possible to create recursive subdirectories (e.g.,
#' `tempdir/subtempdir/otherdir`, see the `Examples`) because
#' [removing][unlink()] such directories might remove files that are still
#' needed by other processes.
#'
#' Although the temporary directory given by [tempdir()] will normally be
#' automatically [removed][unlink()] when \R [quits][quit()] (and operating
#' systems might
#' [periodically](https://cran.r-project.org/doc/manuals/R-admin.html#Running-R)
#' empty the temporary directory), the created subdirectories should be
#' [removed][unlink()] once they are not needed anymore, see the section
#' `Usage in practice` below.
#'
#' @returns
#' The [absolute normalized][fs::path_abs()] path to the created temporary
#' directory, returned [invisibly][invisible].
#'
#' @inheritSection create_dir Side effects
#'
#' @section Usage in practice:
#' `Examples` and `tests` should write to a temporary directory that is cleaned
#' up afterwards (otherwise [`R cmd check`](https://r-pkgs.org/R-CMD-check.html)
#' will issue a `Note` about [`detritus in the temp directory`](
#' https://contributor.r-project.org/cran-cookbook/code_issues.html#leaving-files-in-the-temporary-directory)
#' and CRAN will not accept your package, see section `Source packages` from the
#' [CRAN policies](https://cran.r-project.org/web/packages/policies.html)).
#' Although [tempdir()] points to a temporary
#' directory, that directory should **not** be removed because other processes
#' in \R and [RStudio](https://posit.co/products/open-source/rstudio) also use
#' it. Instead, create a temporary subdirectory in [tempdir()] and afterwards
#' clean up by [removing][unlink()] that subdirectory:
#'
#' ```
#' my_tempdir <- create_tempdir(prefix = "subtempdir")
#' < do stuff >
#' unlink(my_tempdir, recursive = TRUE)
#' ```
#'
#' It is good practice to create the complete temporary directory with all
#' required files and folders before running any test for the presence or
#' absence of particular files or folders: this ensures no spurious matches
#' occur if examples or tests are removed or added.
#'
#' @inheritSection checkinput::is_path Programming notes
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, with a `Note on paths`
#' and extensive references about file paths and directories;
#' [create_dir()] to create a (non-temporary) directory if it does not yet exist;
#' [local()] and
#' [withr::local_tempdir()](https://withr.r-lib.org/reference/with_tempfile.html)
#' for automated deletion of temporary directories;
#' [tempfile()] used in this function to create the paths for the temporary
#' directory;
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' tempdir(check = TRUE)
#' # Create a directory inside the directory returned by 'tempdir()'
#' (my_subtempdir_ex1 <- create_tempdir(prefix = "subtempdir"))
#'
#' # Using the same 'prefix' again creates another directory
#' (my_subtempdir_ex2 <- create_tempdir(prefix = "subtempdir"))
#'
#' # It is not possible to create recursive subdirectories
#' try(no_subtempdir <- create_tempdir(prefix = "subtempdir/otherdir"))
#' try(no_subtempdir <- create_tempdir(prefix = "subtempdir\\otherdir"))
#'
#' # Clean up
#' unlink(c(my_subtempdir_ex1, my_subtempdir_ex2), recursive = TRUE)
#' rm(my_subtempdir_ex1, my_subtempdir_ex2)
#'
#' @export
create_tempdir <- function(prefix = "tempdir") {
  stopifnot(
    "'prefix' should be a non-empty, non-NA_character_ character string" =
      checkinput::is_character(prefix),
    # Using 'prefix == basename(prefix)' to detect slashes does not work on
    # Ubuntu and MacOS
    "'prefix' should not include file separators" =
      !grepl(pattern = "\\", x = prefix, fixed = TRUE) &&
      !grepl(pattern = "/", x = prefix, fixed = TRUE))

  # Inspired by withr::local_tempdir()
  tempdir_target <- tempfile(pattern = prefix, tmpdir = tempdir(check = TRUE))
  stopifnot(checkinput::is_path(tempdir_target))
  tempdir_target <- as.character(fs::path_abs(path = tempdir_target))

  # fs::file_exists(tempdir_target) returns TRUE if 'tempdir_target' is an
  # existing file or an existing directory.
  if(fs::file_exists(tempdir_target)) {
    # This error should not occur if 'tempfile()' worked correctly when creating
    # 'tempdir_target' but makes it possible to detect problems that might arise.
    stop("Temporary directory already exists (possibly as a file): change",
         " 'prefix' (", paste_quoted(prefix), "):\n", tempdir_target)
  }

  # Notes:
  # - It was checked above that the target directory does not yet exist as a
  #   directory nor as a file, so it is not a problem that fs::dir_exists() does
  #   not complain if a directory already exists.
  # - Using 'recurse = FALSE' to not allow the creation of recursive
  #   subdirectories.
  tempdir_target <- as.character(
    fs::path_abs(fs::dir_create(path = tempdir_target, recurse = FALSE)))
  if(!fs::dir_exists(tempdir_target)) {
    stop("Attempt to create a subdirectory in the temporary directory failed:\n",
         tempdir_target)
  }

  invisible(tempdir_target)
}
