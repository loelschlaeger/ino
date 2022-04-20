#' ino: A package for initialization for numerical optimization
#'
#' This package provides tools for the analysis of the initialization of
#' numerical optimization.
#'
#' @docType package
#' @name ino
#' @keywords
#' internal
NULL

#' @noRd
#' @importFrom progress progress_bar

ino_pb <- function(title, total) {
  progress::progress_bar$new(
    format = paste(":spin", title, ":percent [ETA: :eta]"),
    total = total,
    clear = TRUE
  )
}

#' @noRd

ino_pp <- function(pb) {
  if (identical(getOption("ino_progress"), TRUE)) {
    pb$tick()
  }
}

#' @noRd

.onLoad <- function(lib, pkg) {
  options("ino_progress" = TRUE)
}

#' @noRd
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  ### startup message
  msg <- paste0(
    "Thanks for using {ino} ", utils::packageVersion("ino"), "."
  )
  packageStartupMessage(msg)
  invisible()
}
