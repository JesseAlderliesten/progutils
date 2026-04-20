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
#' @inherit create_dir details
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
#' Various restrictions are imposed on `dir`, see [create_dir()].
#'
#' The absolute [normalised][normalizePath()] path is returned such that the
#' returned path still works if the [working directory][getwd()] changes. `"/"`
#' instead of `"\\"` is used as argument [winslash][normalizePath()] such that
#' the returned path can be used in Windows' file system.
#'
#' The *directory* for the returned path is [created][create_dir()] if it does
#' not yet exist. A warning is issued if the *file* indicated by the returned
#' path already exists. Use `"%OSn"` as part of `format_stamp` to create precise
#' stamps by truncating seconds to `0 <= n <= 6` decimal places to prevent this,
#' see [strftime()] for details.
#'
#' @section Side effects:
#' The directory indicated by the returned file path is created if it does not
#' yet exist.
#'
#' @section Programming notes:
#' [tools::file_path_sans_ext()] does *not* recognise the extension of file names
#' that end in a dot, whereas [tools::file_ext()] *does* recognise such
#' extensions. Using [progutils::file_path_sans_ext()] from `progutils` prevents
#' problems caused by this discrepancy, see the `Examples` of
#' [progutils::file_path_sans_ext()] from `progutils`.
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
#' my_tempdir <- normalizePath(path = tempdir(), winslash = "/", mustWork = NA)
#'
#' (create_path(filename = "abc.txt", format_stamp = "",
#'             dir = my_tempdir, add_date = TRUE))
#' (create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
#'             dir = my_tempdir, add_date = TRUE))
#' (create_path(filename = "def.html", format_stamp = "",
#'             dir = my_tempdir, add_date = FALSE))
#' (create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
#'             dir = my_tempdir, add_date = FALSE))
#' (create_path(filename = "abc.txt", format_stamp = "",
#'             dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_path(filename = "abc.txt", format_stamp = "%d_%m_%Y",
#'             dir = file.path(my_tempdir, "subdir"), add_date = TRUE))
#' (create_path(filename = "def.html", format_stamp = "",
#'             dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#' (create_path(filename = "def.html", format_stamp = "%d_%m_%Y",
#'             dir = file.path(my_tempdir, "subdir"), add_date = FALSE))
#'
#' # Cleaning up
#' unlink(x = file.path(my_tempdir,
#'                      c(format(Sys.time(), format = "%Y_%m_%d"), "subdir")),
#'        recursive = TRUE)
#' rm(my_tempdir)
#'
#' @export
create_path <- function(filename, format_stamp = "%Y_%m_%d_%H_%M_%S",
                        dir = file.path(".", "output"), add_date = TRUE) {
  stopifnot(checkinput::is_character(filename),
            checkinput::is_character(format_stamp, allow_empty = TRUE),
            checkinput::is_character(dir))

  file_ext <- tools::file_ext(filename)
  # See the 'Programming note' why not using tools::file_path_sans_ext()
  file_sans_ext <- progutils::file_path_sans_ext(filename)
  if(file_ext == "" || file_sans_ext == "" || filename == file_sans_ext) {
    stop("'filename' should include the name and the file extension:\n", filename)
  }

  if(format_stamp != "") {
    file_stamp <- paste0(format(Sys.time(), format = format_stamp), "_")
    filename <- paste0(file_stamp, filename)
    file_sans_ext <- paste0(file_stamp, file_sans_ext)
  }

  if(grepl(pattern = "/|\\\\", x = file_sans_ext)) {
    stop("Filename '", filename, "' contains (back)slashes: use argument 'dir'",
         " to indicate the directory!")
  }

  # Replace non-alphanumeric characters other than underscores with dots.
  file_sans_ext_vld <- gsub(pattern = "[^[:alnum:]_]", replacement = ".",
                            x = file_sans_ext)
  filename_vld <- paste0(file_sans_ext_vld, ".", file_ext)
  if(file_sans_ext != file_sans_ext_vld) {
    warning("Replaced non-alphanumeric characters other than underscores in",
            " filename\n'", filename, "' with dots: ", filename_vld)
  }

  file_path <- file.path(progutils::create_dir(dir = dir, add_date = add_date),
                         filename_vld)
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
