#' Get paths to directories
#'
#' Wrapper around [.libPaths()] to get the paths \R knows about, with paths
#' containing the same \R [version number][getRversion()] (`x.y.z`) as the
#' currently running \R session given before other paths.
#'
#' @returns
#' A character vector containing the same elements as [.libPaths()], possibly in
#' a different order.
#'
#' @section Programming note:
#' See `InstallPkgs::prepare_install()` where `get_paths()` is checked and used.
#'
#' @family
#' functions to check and modify paths and directories
#'
#' @examples
#' .libPaths()
#' get_paths() # The same elements as above, possibly in a different order
#' getRversion() # The currently used R version
#'
#' @export
get_paths <- function() {
  paths_libPaths <- .libPaths()
  if(length(paths_libPaths) == 0L || all(!nzchar(paths_libPaths))) {
    warning("'.libPaths' does not contain any non-empty path!")
  }
  bool_Rversion <- grepl(pattern = paste0("R-", as.character(getRversion())),
                         x = paths_libPaths, fixed = TRUE)
  c(paths_libPaths[bool_Rversion], paths_libPaths[!bool_Rversion])
}
