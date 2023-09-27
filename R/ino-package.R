#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom optimizeR apply_optimizer
## usethis namespace: end
NULL

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

ino_status <- function(msg, verbose = getOption("ino_verbose", default = TRUE)) {
  if (verbose) {
    cli::cli_alert_info(msg)
  }
}

#' @noRd
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @keywords internal

ino_success <- function(msg, verbose = getOption("ino_verbose", default = TRUE), delay = 0.05) {
  if (verbose) {
    cli::cli_alert_success(msg)
    Sys.sleep(delay)
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

ino_seed <- function(seed, verbose = getOption("ino_verbose", default = FALSE)) {
  if (!is.null(seed)) {
    is_count(seed)
    set.seed(seed)
    ino_status(glue::glue("Set a seed ({seed})."), verbose = verbose)
  }
}
