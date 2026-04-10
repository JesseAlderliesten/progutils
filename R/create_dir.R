#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir Non-empty character string containing the path to a directory that
#' should be created if it does not yet exist. A dot (i.e., ".") indicates the
#' current working directory.
#' @param add_date `TRUE` or `FALSE`: create a subdirectory with the current
#' date in the [format][strftime()] `YYYY_mm_dd`?
#'
#' @details
#' The default `dir` is a subdirectory with the current date in the
#' [format][strftime()] `YYYY_mm_dd` in directory `output` below the working
#' directory. [file.path()] ensures the correct ([platform][.Platform]-dependent)
#' file separator is used to indicate subdirectories, and `"."` indicates the
#' [working directory][getwd()].
#'
#' Several limitations are imposed on `dir` to facilitate handling of paths by
#' Windows, see [dir.create()]: `dir` should not end in a slash, backslash, or
#' space because those characters would be removed when the directory is created,
#' leading to a mismatch between the created directory and the returned path.
#' `dir` should also not end in a dot. Finally, `dir` should not contain the
#' characters `"`, `*`, `?`, `|`, `<`, or `>`.
#'
#' If creating the directory fails, the working directory is returned instead.
#' This happens if `dir` points to an existing file instead of an directory.
#'
#' The absolute [normalised][normalizePath()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. `"/"`
#' instead of `"\\"` is used as [winslash][normalizePath()] during normalisation,
#' such that the returned path can be used in Windows' file system.
#'
#' @returns
#' A character string with the absolute [normalized][normalizePath()] path to
#' the requested directory, returned [invisibly][invisible]. The
#' [working directory][getwd()] is returned if an attempt to create a directory
#' fails, with a warning.
#'
#' @section Side effects:
#' The requested directory is created if does not yet exist.
#'
#' @seealso
#' [create_path()] to create a path, and references there about file paths,
#' [dir.exists()] and [dir.create()] used by this function,
#' [get_filename()] to check if a file exists and is a unique match to a pattern
#'
#' `fs::path_sanitize()` to *remove* invalid characters from potential paths,
#' looking for a wider range of invalid characters.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary directory to not write in the user's directory
#' my_tempdir <- tempdir()
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
#' # Directories are case-insensitive, so adding 'dir_ONE' gives the same result
#' # as above:
#' res_dir_one_v3 <- create_dir(dir = file.path(my_tempdir, "dir_ONE"),
#'                              add_date = FALSE)
#' identical(res_dir_one, res_dir_one_v3) # TRUE
#'
#' # Create directory 'dir_two' with a subdirectory containing the current date
#' res_dir_two <- create_dir(dir = file.path(my_tempdir, "dir_two"),
#'                           add_date = TRUE)
#' dir.exists(res_dir_two) # TRUE
#'
#' # Cleaning up
#' unlink(c(res_dir_one, dirname(res_dir_two)), recursive = TRUE)
#' rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_one_v3, res_dir_two)
#'
#' @export
create_dir <- function(dir = file.path(".", "output"), add_date = TRUE) {
  # See 'Details' about the restrictions imposed on 'dir'.
  stopifnot(checkinput::is_character(dir),
            "'dir' should not end with '/'" =
              substring(text = dir, first = nchar(dir)) != "/",
            "'dir' should not end with '\\'" =
              substring(text = dir, first = nchar(dir)) != "\\",
            "'dir' should not end with '.'" =
              basename(dir) == "." ||
              substring(text = dir, first = nchar(dir)) != ".",
            "'dir' should not end with ' ' (i.e., a space)" =
              substring(text = dir, first = nchar(dir)) != " ",
            "'dir' should not contain any of the following characters: \" * ? | < >" =
              !grepl(pattern = '[<>"|?*]', x = dir),
            checkinput::is_logical(add_date))

  if(add_date) {
    dir <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)

  # dir.exists() returns FALSE if 'dir' is a file instead of a directory.
  if(!dir.exists(dir)) {
    # Notes:
    # - This branch is only used if the directory did not yet exist as directory,
    #   so it is not a problem that dir.create() returns FALSE if a directory
    #   already exists. However, the path can already exist as a file: then
    #   creating the attempt to create it as a directory will fail (indicated by
    #   a warning) and the working directory will be used instead (indicated by
    #   an additional warning).
    # - Using 'recursive = TRUE' to allow creation of subdirectories inside a
    #   not-yet existing directory (e.g., creating './output/<date>' if
    #   './output' does not yet exist).
    if(!dir.create(path = dir, recursive = TRUE, showWarnings = TRUE)) {
      warning(wrap_text(paste0(
        "Attempt to create directory '", dir, "' failed",
        "!\nReturning the working directory ('", getwd(), "') instead.")))
      dir <- normalizePath(getwd(), winslash = "/", mustWork = NA)
    }
  }

  invisible(dir)
}
