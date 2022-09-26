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

ino_stop <- function(...) {
  msg <- list(...)
  names(msg) <- c("x", rep(">", length(msg)))[1:length(msg)]
  cli::cli_abort(unlist(msg), call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

ino_warn <- function(...) {
  msg <- list(...)
  names(msg) <- c("!", rep(">", length(msg)))[1:length(msg)]
  cli::cli_warn(unlist(msg), call = NULL)
}
