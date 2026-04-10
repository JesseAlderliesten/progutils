#' Remove extension from file paths
#'
#' A drop-in replacement for tools::file_path_sans_ext, see `Details`. Returns
#' the file path without the extensions (and the leading dot). Only purely
#' alphanumeric extensions are recognized.
#'
#' @inheritParams tools::file_path_sans_ext
#'
#' @details
#' [tools::file_path_sans_ext()] does *not* recognise the extension of file names
#' that end in a dot, whereas [tools::file_ext()] *does* recognise such
#' extensions.
#'
#' Using [progutils::file_path_sans_ext()] from `progutils` ensures that
#' `filename` is recreated by
#' `paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename))`,
#' such that, e.g., [create_path]`("ab..txt")` produces the correct result
#' ending in `"ab..txt"` instead of the nonsense result ending in `"ab..txt.txt"`,
#' see the `Examples`.
#'
#' @family functions to handle paths and directories
#' @family functions to modify character vectors
#'
#' @examples
#' filename <- "ab..txt"
#' tools::file_path_sans_ext(filename) # "ab..txt"
#' tools::file_ext(filename) # "txt"
#' # So the next line produces the nonsense-result "ab..txt.txt"
#' paste0(tools::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#'
#' # The updated version recreates filename 'ab..txt':
#' paste0(progutils::file_path_sans_ext(filename), ".", tools::file_ext(filename))
#'
#' @export
file_path_sans_ext <- function (x, compression = FALSE) {
  if (compression)
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("\\.[[:alnum:]]+$", "\\1", x)
}
