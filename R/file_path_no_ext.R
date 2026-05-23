#' Extract filenames or extensions
#'
#' Distinguish filenames and extensions, correctly handling filenames that end
#' in a dot.
#'
#' @inheritParams tools::file_path_sans_ext
#'
#' @details
#' `file_path_no_ext()` returns the file paths without extensions and leading
#' dot; `file_path_ext()` returns the file extensions without compression
#' extension and leading dot. Only purely alphanumeric extensions are recognized.
#'
#' `file_path_no_ext()` replaces [tools::file_path_sans_ext()] because prior to
#' \R 4.6.0 the latter did **not** recognise the extension of file names ending
#' in a dot, whereas [tools::file_ext()] **did** recognise such extensions. This
#' discrepancy led to `filename` **not** being re-created by
#' `paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))`
#' for filenames like `"ab..txt"`, instead producing the nonsense result
#' `"ab..txt.txt"`, see the `Examples`.
#'
#' `file_path_ext()` replaces [tools::file_ext()] because the latter does not
#' have argument `compression`.
#'
#' @family functions to handle paths and directories
#'
#' @examples
#' filename <- "ab..txt"
#'
#' # Should be "ab." but was "ab..txt" prior to R 4.6.0.
#' tools::file_path_sans_ext(filename)
#' tools::file_ext(filename) # "txt"
#'
#' # The next line produced the nonsense-result "ab..txt.txt" prior to R 4.6.0.
#' paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#'
#' # Using functions from `progutils` recreates filename 'ab..txt':
#' paste0(file_path_no_ext(filename), ".", file_path_ext(filename))
#'
#' @export
file_path_no_ext <- function (x, compression = FALSE) {
  x <- as.character(x)
  if(!length(x))
    return(character(0))
  if(compression) {
    x <- sub(pattern = "[.](gz|bz2|xz)$", replacement = "", x = x)
  }
  sub(pattern = "\\.[[:alnum:]]+$", replacement = "", x = x)
}

#' @rdname file_path_no_ext
#' @export
file_path_ext <- function (x, compression = FALSE) {
  x <- as.character(x)
  if(!length(x))
    return(character(0))
  if(compression)
    x <- sub(pattern = "[.](gz|bz2|xz)$", replacement = "", x = x)
  if(grepl("^(.*[^.]+.*)[.]([[:alnum:]]+)$", basename(x))) {
    sub(pattern = ".*[.]([[:alnum:]]+)$", replacement = "\\1", x = x)
  } else {
    ""
  }
}
