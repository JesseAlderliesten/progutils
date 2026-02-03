#' Check if the operating system is Windows
#'
#' @param action Character string indicating the action if the operating system
#' is *not* Windows.
#'
#' @returns
#' `TRUE` or `FALSE`, indicating if the operating system is Windows, returned
#' [invisibly][invisible].
#'
#' @export
check_os_is_windows <- function(action = c("warn", "message", "quiet")) {
  action <- match.arg(action, several.ok = FALSE)

  if(.Platform$OS.type == "windows") {
    OS_is_Windows <- TRUE
  } else {
    OS_is_Windows <- FALSE
    text_msg <- wrap_text(paste0(
      "This function might fail because it is meant to be used on Windows,",
      " whereas you are using ", .Platform$OS.type, ": ", utils::osVersion, "."))

    switch(action,
           message = message(text_msg),
           quiet = NULL,
           warning(text_msg))
  }
  invisible(OS_is_Windows)
}
