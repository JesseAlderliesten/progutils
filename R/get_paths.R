#' Get paths to directories
#'
#' Get the paths \R knows about, with paths containing the currently used \R
#' version number given before other paths.
#'
#' @param paths The paths to use.
#'
#' @details
#' This is a wrapper around [.libPaths()] to return paths containing the
#' currently used \R [version number][getRversion()] in the form of directory
#' `R-x.y.z` or directory `x.y`, with a warning if none such path is present,
#' before other paths.
#'
#' A warning is issued if the returned vector is of length zero or only contains
#' empty paths (`""`).
#'
#' @returns
#' A character vector containing the same elements as [.libPaths()], possibly in
#' a different order.
#'
#' @family
#' functions to check paths and create directories
#'
#' @examples
#' .libPaths()
#' get_paths() # The same elements as above, possibly in a different order
#' getRversion() # The currently used R version
#'
#' @export
get_paths <- function(paths = .libPaths()) {
  if(length(paths) == 0L || all(!nzchar(paths))) {
    warning("'.libPaths()' did not return any non-empty paths!")
    return(paths)
  }

  current_full <- paste0("R-", as.character(getRversion()))
  current_minor <- substr(x = current_full, start = 3, stop = 5)
  # .libPaths() uses normalizePath(winslash = "/") instead of the default "\\"
  bool_current <- grepl(pattern = paste0("/", current_full, "|/", current_minor),
                        x = paths)

  if(!any(bool_current)) {
    warning(wrap_text(paste0(
      "The currently used R version (", substring(current_full, first = 3),
      ") is not present as directory '", current_full, "' or '", current_minor,
      "' in any of the retrieved paths: ", paste_quoted(unname(paths)), "!")))
  }

  c(paths[bool_current], paths[!bool_current])
}
