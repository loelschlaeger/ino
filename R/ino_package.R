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

ino_pb <- function(title = "", total) {
  progress::progress_bar$new(
    format = paste0(title, ":current/:total"),
    total = total,
    show_after = 0,
    clear = FALSE
  )
}

#' @noRd

ino_pp <- function(pb, verbose = getOption("ino_progress")) {
  if (verbose) pb$tick()
}

#' @noRd

ino_status <- function(msg, verbose = getOption("ino_progress")) {
  if (verbose) message("* ", msg)
}

#' @noRd

.onLoad <- function(lib, pkg) {
  options("ino_progress" = TRUE)
  options("ino_ncores" = 1)
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

#' @noRd

ino_call <- function(call) {
  call$ncores <- 1
  call$verbose <- FALSE
  class(call) <- c("ino_call", class(call))
  return(call)
}

#' @noRd
#' @export

print.ino_call <- function(x, ...) {
  cat("<ino_call>")
}

#' @noRd

ino_check_inputs <- function(...) {
  stop0 <- function(msg) stop(msg, call. = FALSE)
  inputs <- list(...)

  within(inputs, {
    n <- names(inputs)
    if ("x" %in% n) {
      if (!inherits(x, "ino")) {
        stop0("'x' must be of class 'ino'.")
      }
    }
    if ("runs" %in% n) {
      if (!length(runs) == 1 && is_number(runs)) {
        stop0("'runs' must be an integer.")
      }
    }
    if ("sampler" %in% n) {
      if (!is.function(sampler)) {
        stop0("'sampler' must be a function.")
      }
    }
    if ("at" %in% n) {
      if (!is.numeric(at)) {
        stop0("'at' must be a numeric vector.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) > x$f$npar) {
        stop0("'at' has more entries than the function has parameters.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) < x$f$npar) {
        stop0("'at' has less entries than the function has parameters.")
      }
    }
  })

  return(invisible(NULL))
}
