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
#' @importFrom progress progress_bar
#' @keywords
#' internal

ino_pb <- function(title = character(), total) {
  progress::progress_bar$new(
    format = paste0(title, ":current/:total"),
    total = total,
    show_after = 0,
    clear = FALSE
  )
}

#' @noRd
#' @keywords
#' internal

ino_pp <- function(pb, verbose = getOption("ino_progress")) {
  if (verbose) {
    if (pb$.__enclos_env__$private$total > 1) {
      pb$tick()
    }
  }
}

#' @noRd
#' @importFrom cli cli_art_info
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
  cli::cli_abort(c("x" = event, "i" = debug))
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords
#' internal

ino_warn <- function(event, debug = character()) {
  cli::cli_warn(c(event, "i" = debug))
}
