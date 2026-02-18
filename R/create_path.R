#' Create a file path
#'
#' Create a file path, creating the indicated directory if it does not yet exist.
#'
#' @inheritParams create_dir
#' @param filename A character string with the file name, including the file
#' extension like `.csv` or `.txt`.
#' @param format_stamp A character string indicating the [format][strftime()] of
#' the stamp to be added in front of the file name. No stamp is added if
#' `format_stamp` is an empty string (i.e., `""`).
#'
#' @details
#' `filename` should contain a file extension: a dot followed by alphanumeric
#' characters until the end of the file name. An error occurs if no file
#' extension is present. Non-alphanumeric characters in `filename` before the
#' file extension are replaced by underscores, with a warning.
#'
#' A warning is issued if the file indicated by the returned path already exists.
#' Use `"%OSn"` as part of `format_stamp` to truncate seconds to `0 <= n <= 6`
#' decimal places for precise stamps, see [strftime()] for details.
#'
#' The directory for the returned path is [created][create_dir()] if it does not
#' yet exist. By default it is a subdirectory with the current date in format
#' `YYYY_mm_dd` in directory `output` below the current working directory.
#'
#' The returned path is [normalised][normalizePath()] such that it still works
#' if the [working directory][getwd()] changes. `"/"` instead of `"\\"` is used
#' as [winslash][normalizePath()], such that the returned path can be used in
#' Windows' file system.
#'
#' @returns
#' The created file path, returned [invisibly][invisible()].
#'
#' @section Side effects:
#' The directory indicated by the returned file path is created if it does not
#' yet exist.
#'
#' @section Programming notes:
#' File names that end in a dot are handled incorrectly by `create_path()`:
#' [tools::file_path_sans_ext()] does not recognise the extension of file names
#' that end in a dot, whereas [tools::file_ext()] does recognise such extension.
#' Therefore, `create_path("ff..txt")` produces the nonsense result ending in
#' `"ff__txt.txt"` instead of ending in `"ff_.txt"`.
#'
#' @seealso
#' [get_filename()] to check if a file exists and is a unique match to a pattern;
#' [file.path()] to construct file paths in a platform-independent way;
#' [normalizePath()] to create absolute paths; [create_dir()] used by this
#' function to create a directory if it does not yet exists.
#'
#' @family
#' functions to check paths and create directories
#'
#' @examples
#'
#' @export
create_path <- function(filename, format_stamp = "%Y_%m_%d_%H_%M_%S",
                        dir = file.path(".", "output"), add_date = TRUE) {
  stopifnot(checkinput::is_character(filename),
            checkinput::is_character(format_stamp, allow_empty = TRUE))

  file_ext <- tools::file_ext(filename)
  if(file_ext == "") {
    stop(wrap_text(paste0(
      "Filename '", filename, "' should contain a file extension: a dot followed",
      " by only alphanumeric characters until the end of the file name.")))
  }

  file_sans_ext <- tools::file_path_sans_ext(filename)
  if(grepl(pattern = "/|\\\\", x = file_sans_ext)) {
    file_sans_ext <- gsub(pattern = "/|\\\\", replacement = "_", x = file_sans_ext)
    warning("Replaced (back)slashes in filename '", filename, "' with",
            " underscores!\nUse argument 'dir' to indicate the directory:\n",
            paste0(file_sans_ext, ".", file_ext))
  }

  # Replace non-alphanumeric characters (including spaces) with underscores
  file_sans_ext_vld <- gsub(pattern = "[^[:alnum:]]", replacement = "_",
                            x = file_sans_ext)
  filename_vld <- paste0(file_sans_ext_vld, ".", file_ext)
  if(file_sans_ext != file_sans_ext_vld) {
    warning(progutils::wrap_text(paste0(
      "Replaced non-alphanumeric characters in filename '", filename,
      "' with underscores!")))
  }

  # See section 'Details'
  file_path <- normalizePath(
    path = file.path(progutils::create_dir(dir = dir, add_date = add_date),
                     paste0(
                       if(format_stamp != "") {
                         paste0(format(Sys.time(), format = format_stamp), "_")
                       }, filename_vld)
    ),
    winslash = "/", mustWork = FALSE)
  if(file.exists(file_path)) {
    # To do:
    # - Add 'call. = FALSE'?
    warning(paste0("File already exists:\n", file_path))
  }
  invisible(file_path)
}
