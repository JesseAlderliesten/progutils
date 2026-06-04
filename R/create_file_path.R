#' Create a file path
#'
#' Create a file path, creating the indicated directory if it does not yet exist.
#'
#' @inheritParams create_dir dir add_date
#' @param filename A character string with the file name, including the file
#' extension like `.csv` or `.txt`. Should adhere to the restrictions described
#' in [checkinput::is_path()].
#' @param format_stamp A character string indicating the [format][strftime()] of
#' the stamp to be added in front of the file name. No stamp is added if
#' `format_stamp` is an empty string (i.e., `""`). The formatted stamp is
#' treated as part of the filename, such that the same restrictions apply, see
#' `Details`.
#'
#' @returns
#' The created file path, returned [invisibly][invisible()].
#'
#' @details
#' `filename` **should** contain a file extension (i.e., a dot followed by any
#' character until the end of the file name) and should **not** contain slashes
#' or backslashes: use `dir` to indicate subdirectories.
#'
#' The [absolute normalised][fs::path_abs()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes.
#'
#' A warning is issued if the **file** indicated by the returned path already
#' exists. To prevent this when creating files in quick succession, use `"%OSn"`
#' as part of `format_stamp` to create precise stamps by truncating seconds to
#' `0 <= n <= 6` decimal places, see [strftime()] for details.
#'
#' @section Side effects:
#' The directory indicated by the returned file path is [created][create_dir()]
#' if it does not yet exist.
#'
#' @seealso
#' [checkinput::is_path()] to check if a path is valid, and the 'Note on paths'
#' in its documentation;
#' [get_file_path()] to check if a file exists and is a unique match to a pattern,
#' [fs::path()] to construct file paths in a platform-independent way,
#' [fs::path_abs()] to create absolute normalised paths,
#' [create_dir()] to create a directory if it does not yet exist
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary directory to not write in the user's directory
#' my_tempdir <- fs::path_abs(path = fs::path(tempdir(), "subdir"))
#'
#' (create_file_path(filename = "abc.txt", format_stamp = "",
#'                   dir = my_tempdir, add_date = TRUE))
#' (create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
#'                   dir = my_tempdir, add_date = TRUE))
#' (create_file_path(filename = "def.html", format_stamp = "",
#'                   dir = my_tempdir, add_date = FALSE))
#' (create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
#'                   dir = my_tempdir, add_date = FALSE))
#' (create_file_path(filename = "abc.txt", format_stamp = "",
#'                   dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
#'                   dir = fs::path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_file_path(filename = "def.html", format_stamp = "",
#'                   dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#' (create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
#'                   dir = fs::path(my_tempdir, "subdir"), add_date = FALSE))
#'
#' # Cleaning up
#' unlink(x = my_tempdir, recursive = TRUE)
#' rm(my_tempdir)
#'
#' @export
create_file_path <- function(filename, format_stamp = "%Y_%m_%d_%H_%M_%S",
                             dir = fs::path_wd("output"), add_date = TRUE) {
  filename_label <- deparse1(substitute(filename))

  stopifnot(checkinput::is_character(filename), checkinput::is_path(filename),
            checkinput::is_character(format_stamp, allow_empty = TRUE),
            checkinput::is_character(dir), checkinput::is_path(dir),
            checkinput::is_logical(add_date))

  filename_no_ext <- fs::path_ext_remove(path = filename)
  file_ext <- fs::path_ext(path = filename)
  if(length(filename_no_ext) == 0L || !nzchar(filename_no_ext) ||
     length(file_ext) == 0L || !nzchar(file_ext)) {
    stop("Empty filename or missing extension:\n", filename)
  }

  if(nzchar(format_stamp)) {
    filename <- paste0(format(Sys.time(), format = format_stamp), "_", filename)
  }

  if(grepl(pattern = "/", x = filename, fixed = TRUE) ||
     grepl(pattern = "\\", x = filename, fixed = TRUE)) {
    stop("'filename' (", paste_quoted(filename_label),
         ") should not contain '/' or '\\':\n", filename)
  }

  file_path <- fs::path(create_dir(dir = dir, add_date = add_date), filename)
  file_path <- fs::path_abs(path = file_path)
  is_valid_file_path <- try(expr = checkinput::is_path(file_path), silent = TRUE)
  if(inherits(x = is_valid_file_path, what = "try-error")) {
    stop("No path created: ", attr(is_valid_file_path, "condition")$message)
  }

  if(fs::is_file(file_path)) {
    warning("File already exists:\n", file_path)
  }
  invisible(file_path)
}
