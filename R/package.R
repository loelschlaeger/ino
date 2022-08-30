#' ino: Initialization strategies for numerical optimization
#'
#' @description
#' This package implements tools for analyzing the initialization of
#' numerical optimization.
#'
#' @docType package
#'
#' @name ino
#'
#' @keywords
#' internal
#'
#' @import optimizeR
"_PACKAGE"

#' @noRd
#' @keywords internal

.onLoad <- function(lib, pkg) {
  options("ino_progress" = TRUE)
  options("ino_ncores" = 1)
  options("ino_save_failures" = FALSE)
  options("cli.progress_clear" = FALSE)
}

#' @noRd
#' @importFrom utils packageVersion
#' @keywords internal

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "Thanks for using {ino} ", utils::packageVersion("ino"), "."
  )
  packageStartupMessage(msg)
  invisible()
}

#' @noRd
#' @importFrom cli cli_alert_info
#' @keywords internal

ino_status <- function(msg, verbose = getOption("ino_progress")) {
  if (verbose) cli::cli_alert_info(msg)
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal

ino_stop <- function(event, debug = character()) {
  cli::cli_abort(c("x" = event, "i" = debug), call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

ino_warn <- function(event, debug = character()) {
  cli::cli_warn(c(event, "i" = debug), call = NULL)
}
