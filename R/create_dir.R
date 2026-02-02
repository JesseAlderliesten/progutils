#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir Non-empty character string containing the path to a directory.
#' @param add_date `TRUE` or `FALSE`: create a subdirectory with the current
#' date (format: `YYYY_MM_DD`)?
#' @param quietly `TRUE` or `FALSE`: suppress the message indicating which
#' directory was created?
#'
#' @details
#' Using [file.path()] ensures the correct (platform-dependent) file separator
#' is used to indicate subdirectories. The `"."` in the default for `dir`
#' indicates the [working directory][getwd()].
#'
#' Several limitations are imposed on `dir` to facilitate handling of paths by
#' Windows, see [dir.create()]: `dir` should not end in a slash, backslash, or
#' space because those characters would be removed when the directory is created,
#' leading to a mismatch between the created directory and the returned path.
#' `dir` should also not end in a dot. Finally, `dir` should not contain the
#' characters `"`, `*`, `?`, `|`, `<`, or `>`.
#'
#' @returns
#' A character string with the [normalized][normalizePath()] path to the
#' requested directory, returned [invisibly][invisible]. The
#' [working directory][getwd()] is returned if an attempt to create a directory
#' fails, with a warning.
#'
#' @section Side effects:
#' The requested directory is created if does not yet exist.
#'
#' @seealso
#' [check_file()] to check if a file exists and is a unique match to a pattern;
#' [file.path()] to construct file paths in a platform-independent way;
#' [normalizePath()] to create absolute paths; [dir.exists()] and [dir.create()]
#' used by this function.
#'
#' `fs::path_sanitize()` to *remove* invalid characters from potential paths,
#' looking for a wider range of invalid characters.
#'
#' @family
#' functions to check paths and create directories
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
#' # directory and the same directory is returned. However, the message now
#' # indicates the directory already exists.
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
create_dir <- function(dir = file.path(".", "output"), add_date = TRUE,
                       quietly = FALSE) {
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
            checkinput::is_logical(add_date), checkinput::is_logical(quietly))

  if(add_date) {
    dir <- file.path(dir, format(Sys.time(), format = "%Y_%m_%d"))
  }

  dir <- normalizePath(dir, mustWork = FALSE)

  # dir.exists() returns FALSE if 'dir' is a file instead of a directory.
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
    if(dir.create(path = dir, recursive = TRUE, showWarnings = FALSE)) {
      if(!quietly) {
        message("Created directory '", dir, "'.")
      }
    } else {
      warning(wrap_text(paste0(
        "Attempt to create directory '", dir, "' failed",
        "!\nReturning the working directory ('", getwd(), "') instead.")))
      dir <- normalizePath(getwd(), mustWork = NA)
    }
  }

  invisible(dir)
}
