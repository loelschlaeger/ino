#' ino: Analysis of initialization for numerical optimization
#'
#' @description
#' This package implements tools for analyzing the initialization of
#' numerical optimization.
#'
#' @docType package
#'
#' @name ino
#'
#' @keywords internal
#'
#' @import optimizeR
"_PACKAGE"

#' @noRd
#' @keywords internal

.onLoad <- function(lib, pkg) {
  options("ino_verbose" = TRUE)
  options("ino_ncores" = 1)
}

#' @noRd
#' @importFrom utils packageVersion
#' @importFrom glue glue
#' @keywords internal

.onAttach <- function(lib, pkg) {
  msg <- glue::glue("Thanks for using {{ino}} {utils::packageVersion('ino')}.")
  packageStartupMessage(msg)
  invisible()
}

#' @noRd
#' @importFrom cli cli_alert_info
#' @importFrom glue glue
#' @keywords internal

ino_status <- function(msg, verbose = getOption("ino_verbose")) {
  if (verbose) cli::cli_alert_info(glue::glue(msg))
}

#' @noRd
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @keywords internal

ino_success <- function(msg, verbose = getOption("ino_verbose"), delay = 0.1) {
  if (verbose) cli::cli_alert_success(glue::glue(msg))
  Sys.sleep(delay)
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal

ino_stop <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "x"
  names(msg)[-1] <- ">"
  cli::cli_abort(msg, call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

ino_warn <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "!"
  names(msg)[-1] <- ">"
  cli::cli_warn(unlist(msg))
}
