#' Create a file path
#'
#' Create a file path, creating the indicated directory if it does not yet exist.
#'
#' @inheritParams create_dir
#' @param filename A character string with the file name, including the file
#' extension like `.csv` or `.txt`.
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
#' `filename` should contain a file extension (i.e., a dot followed by
#' alphanumeric characters until the end of the file name). It should not
#' contain slashes or backslashes: use `dir` to indicate (sub)directories.
#' Non-alphanumeric characters other than underscores before the file extension
#' are replaced by dots, with a warning.
#'
#' The default `dir` is a subdirectory with the current date in the
#' [format][strftime()] `YYYY_mm_dd` in directory `output` below the working
#' directory. [file.path()] ensures the correct ([platform][.Platform]-dependent)
#' file separator is used to indicate subdirectories, and `"."` indicates the
#' [working directory][getwd()].
#'
#' `dir` should point to a [valid path][is_path()]. The directory for the
#' returned path is [created][create_dir()] if it does not yet exist.
#'
#' The absolute [normalised][normalizePath()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. `"/"`
#' instead of `"\\"` is used as argument [winslash][normalizePath()] such that
#' the returned path can be used in Windows' file system.
#'
#' A warning is issued if the **file** indicated by the returned
#' path already exists. To prevent this when creating files in quick succession,
#' use `"%OSn"` as part of `format_stamp` to create precise stamps by truncating
#' seconds to `0 <= n <= 6` decimal places, see [strftime()] for details.
#'
#' @section Side effects:
#' The directory indicated by the returned file path is created if it does not
#' yet exist.
#'
#' @seealso
#' [get_filename()] to check if a file exists and is a unique match to a pattern,
#' [file.path()] to construct file paths in a platform-independent way,
#' [normalizePath()] to create absolute normalised paths,
#' [create_dir()] to create a directory if it does not yet exist
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' # Use a temporary directory to not write in the user's directory
#' my_tempdir <- normalizePath(path = file.path(tempdir(), "subdir"),
#'                             winslash = "/", mustWork = FALSE)
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
#'                   dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_file_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
#'                   dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_file_path(filename = "def.html", format_stamp = "",
#'                   dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#' (create_file_path(filename = "def.html", format_stamp = "%d_%m_%Y",
#'                   dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#'
#' # Cleaning up
#' unlink(x = my_tempdir, recursive = TRUE)
#' rm(my_tempdir)
#'
#' @export
create_file_path <- function(filename, format_stamp = "%Y_%m_%d_%H_%M_%S",
                             dir = file.path(".", "output"), add_date = TRUE) {
  stopifnot(checkinput::is_character(filename),
            checkinput::is_character(format_stamp, allow_empty = TRUE),
            checkinput::is_character(dir))

  if(grepl(pattern = "/|\\\\", x = filename)) {
    stop("No path created because 'filename' contains slashes or backslashes:",
         " use argument\n'dir' to indicate the directory: ", filename)
  }

  is_valid_filename <- try(expr = is_filename(filename = filename), silent = TRUE)
  if(inherits(x = is_valid_filename, what = "try-error")) {
    stop("No path created: ", attr(is_valid_filename, "condition")$message)
  }

  if(nzchar(format_stamp)) {
    filename <- paste0(format(Sys.time(), format = format_stamp), "_", filename)
  }

  # Replace non-alphanumeric characters other than underscores with dots.
  file_no_ext <- file_path_no_ext(x = filename)
  file_no_ext_gsub <- gsub(pattern = "[^[:alnum:]_]", replacement = ".",
                           x = file_no_ext)
  # is_filename(filename = filename) above ensures that a file extension is
  # present, such that this way or re-creating the filename works
  filename_gsub <- paste0(file_no_ext_gsub, ".", file_path_ext(x = filename))
  if(file_no_ext != file_no_ext_gsub) {
    warning("Replaced non-alphanumeric characters other than underscores in",
            " filename\n'", filename, "' with dots: ", filename_gsub)
    # To do:
    # - This is probably unwanted?
    is_valid_filename_gsub <- try(expr = is_filename(filename_gsub), silent = TRUE)
    if(inherits(x = is_valid_filename_gsub, what = "try-error")) {
      stop("No path created because 'filename' is not valid after replacing",
           " non-alphanumeric\ncharacters: ",
           attr(is_valid_filename_gsub, "condition")$message)
    }
  }

  is_path(dir)

  file_path <- file.path(create_dir(dir = dir, add_date = add_date),
                         filename_gsub)
  file_path <- normalizePath(path = file_path, winslash = "/", mustWork = FALSE)

  if(file.exists(file_path)) {
    if(dir.exists(file_path)) {
      warning(paste0("Path already exists but points to a directory, not a file:\n",
                     file_path))
    } else {
      warning(paste0("File already exists:\n", file_path))
    }
  }
  invisible(file_path)
}
