#' ino: Initialization strategies for numerical optimization
#'
#' @description
#' This package implements tools for the analysis of the initialization of
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

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' @noRd
#' @keywords
#' internal

.onLoad <- function(lib, pkg) {
  options("ino_progress" = TRUE)
  options("ino_ncores" = 1)
  options("ino_depth" = 0)
}

#' @noRd
#' @importFrom utils packageVersion
#' @keywords
#' internal

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "Thanks for using {ino} ", utils::packageVersion("ino"), "."
  )
  packageStartupMessage(msg)
  invisible()
}

#' @noRd
#' @importFrom cli cli_alert_info
#' @keywords
#' internal

ino_status <- function(msg, verbose = getOption("ino_progress")) {
  if (verbose) cli::cli_alert_info(msg)
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords
#' internal

ino_stop <- function(event, debug = character()) {
  cli::cli_abort(c("x" = event, "i" = debug), call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords
#' internal

ino_warn <- function(event, debug = character()) {
  cli::cli_warn(c(event, "i" = debug), call = NULL)
}

#' @noRd
#' @keywords
#' internal

ino_inc_depth <- function(i) {
  depth <- getOption("ino_depth")
  if (is.null(depth)) depth <- 0
  depth <- max(0, depth + i)
  options("ino_depth" = depth)
}

ino_set_depth <- function(msg) {
  depth <- getOption("ino_depth")
  if (is.null(depth)) depth <- 0
  paste(rep("*", depth), msg)
}
