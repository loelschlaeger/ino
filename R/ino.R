#' ino: Initial Values for Numerical Optimization
#'
#' @description
#' This package implements tools for analyzing the initialization of
#' numerical optimization, i.e., the impact of the initial value on the outcome.
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
#' @importFrom utils packageVersion
#' @importFrom glue glue
#' @keywords internal

.onAttach <- function(lib, pkg) {
  # msg <- glue::glue("Thanks for using {{ino}} {utils::packageVersion('ino')}.")
  # packageStartupMessage(msg)
  invisible()
}

#' @noRd
#' @importFrom cli cli_alert_info
#' @importFrom glue glue
#' @keywords internal

ino_status <- function(
    msg, verbose = getOption("ino_verbose", default = TRUE)
  ) {
  if (verbose) {
    cli::cli_alert_info(msg)
  }
}

#' @noRd
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @keywords internal

ino_success <- function(
    msg, verbose = getOption("ino_verbose", default = TRUE)
  ) {
  if (verbose) {
    cli::cli_alert_success(msg)
  }
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal

ino_stop <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "x"
  names(msg)[-1] <- "*"
  cli::cli_abort(msg, call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

ino_warn <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "!"
  names(msg)[-1] <- "*"
  cli::cli_warn(msg, call = NULL)
}

#' @noRd
#' @keywords internal

ino_seed <- function(
    seed, verbose = getOption("ino_verbose", default = FALSE)
  ) {
  if (!is.null(seed)) {
    is_count(seed)
    set.seed(seed)
    ino_status(glue::glue("Set a seed ({seed})."), verbose = verbose)
  }
}

#' @noRd
#' @keywords internal

ino_ask <- function (question, default = FALSE) {
  if (!interactive()) {
    return(default)
  }
  repeat {
    selection <- if (default) {
      "[Y/n]"
    } else {
      "[y/N]"
    }
    prompt <- sprintf("%s %s: ", question, selection)
    response <- tryCatch(
      tolower(trimws(readline(prompt))),
      interrupt = identity
    )
    if (inherits(response, "interrupt")) {
      stop()
    }
    if (!nzchar(response)) {
      return(default)
    }
    if (response %in% c("y", "yes")) {
      cat("")
      return(TRUE)
    }
    if (response %in% c("n", "no")) {
      cat("")
      return(FALSE)
    }
    ino_status(
      "Unrecognized response, please enter 'y' or 'n', or type Ctrl + C to cancel."
    )
  }
}
