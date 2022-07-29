#' ino: Initialization strategies for numerical optimization
#'
#' @description
#' This package implements tools for the analysis of the initialization of
#' numerical optimization.
#'
#' @docType package
#' @name ino
#' @keywords
#' internal
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
#' @keywords
#' internal

ino_status <- function(msg, verbose = getOption("ino_progress")) {
  if (verbose) {
    message("* ", msg)
  }
}

#' @noRd
#' @keywords
#' internal

ino_stop <- function(event, debug = character(), call. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  stop(msg, call. = call.)
}

#' @noRd
#' @keywords
#' internal

ino_warn <- function(event, debug = character(),
                     call. = FALSE, immediate. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  warning(msg, call. = call., immediate. = immediate.)
}
