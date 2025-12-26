#' Create a directory
#'
#' Create a directory if it does not yet exist.
#'
#' @param dir Non-empty character string containing the first part of the path,
#' see `Details`. The default `"."` indicates the working directory.
#' @param subdir Character string containing the last part of the path, possibly
#' empty (`""`) to not create a subdirectory (a subdirectory with the date will
#' still be added if `add_date` is `TRUE`). See `Details`.
#' @param add_date `TRUE` or `FALSE`: should a subdirectory be created with the
#' current date? If created, it has format format `YYYY_MM_DD`.
#' @param quietly `TRUE` or `FALSE`: should printing of messages be suppressed?
#'
#' @details
#' `dir` and `subdir` can contain (back)slashes (`/` or `\`) to indicate
#' subdirectories but they should not end with a (back)slash because that would
#' result in mismatches between the created directory and the returned path
#' because trailing (back)slashes are removed when the directory is created.
#'
#' In addition, `dir` and `subdir` cannot be zero-length because, as documented
#' in [file.path()], that would result in `character(0)` as path, even if one of
#' them is a character string; (3) `dir` cannot be an empty character string
#' (`""`); (4) `subdir` cannot be `"."` because it does not make sense to specify the current
#' [working directory][getwd()] as subdirectory.
#'
#' @returns A character string with the created path, returned
#' [invisibly][invisible]. The path will be composed of `dir` and `subdir`, with
#' a subdirectory with the current date if `add_date` is `TRUE`. The
#' [working directory][getwd()] is returned if the attempt to create a directory
#' fails, with a warning.
#'
#' @section Side effects:
#' The requested directory is created if does not yet exist.
#'
#' @section Wishlist:
#' Should a warning be issued if a directory is created that only differs from
#' an existing directory through case changes?
#'
#' @section Programming notes:
#' - Status: code OK, annotation OK, tests OK.
#' - Is the problem that tests create folders that are left instead of temporary
#'   folders also present with package `tinytest`?
#'
#' @seealso [check_file()] [file.path()] [is_file()]
#'
#' @examples
#'
#' @export
create_dir <- function(dir = ".", subdir = "output", add_date = TRUE,
                       quietly = FALSE) {
  stopifnot(checkinput::is_character(dir),
            checkinput::is_character(subdir, allow_empty = TRUE),
            "'dir' should not end with '/'" =
              substring(text = dir, first = nchar(dir)) != "/",
            "'dir' should not end with '\'" =
              substring(text = dir, first = nchar(dir)) != "\\",
            "'subdir' should not be \".\" (the current working directory)" =
              subdir != ".",
            "'subdir' should not end with '/'" =
              substring(text = subdir, first = nchar(subdir)) != "/",
            "'subdir' should not end with '\'" =
              substring(text = subdir, first = nchar(subdir)) != "\\",
            checkinput::is_logical(add_date), checkinput::is_logical(quietly))

  path_to_create <- file.path(dir, subdir)

  if(add_date) {
    path_to_create <- file.path(path_to_create,
                                format(Sys.time(), format = "%Y_%m_%d"))
  }

  if(dir.exists(path_to_create)) {
    if(!quietly) {
      message("Directory '", path_to_create, "' already exists.")
    }
  } else {
    # This branch is only used if the directory did not yet exist, so it is not
    # a problem that dir.create() returns FALSE if a directory already exists.
    # Using 'recursive = TRUE' to allow creation of subdirectories inside a
    # not-yet existing directory (e.g., creating ./output/some_date if ./output
    # does not yet exist).
    creation_OK <- dir.create(path = path_to_create, recursive = TRUE)
    if(creation_OK) {
      if(!quietly) {
        message("Created directory '", path_to_create, "'.")
      }
    } else {
      if(file.exists(path_to_create)) {
        warning("'dir' and 'subdir' point to a file, not to a directory!")
      }
      warning("Attempt to create directory '", path_to_create, "' failed!",
              "\nReturning working directory ('", getwd(), "') instead.")
      path_to_create <- getwd()
    }
  }

  invisible(path_to_create)
}
