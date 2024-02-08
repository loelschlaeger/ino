#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom cli style_italic
#' @importFrom crayon underline
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom forcats fct_reorder
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom mvtnorm pmvnorm
#' @importFrom optimizeR apply_optimizer
#' @importFrom optimizeR optimizer_nlm
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @importFrom scales label_percent
#' @importFrom scales percent
#' @importFrom stats complete.cases
#' @importFrom stats dnorm
#' @importFrom stats median
#' @importFrom stats nlm
#' @importFrom stats rnorm
#' @importFrom utils tail
## usethis namespace: end
NULL

#' @noRd
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  doc_link <- "https://loelschlaeger.de/ino"
  msg <- c(
    paste0(
      "Thanks for using {ino} version ", utils::packageVersion("ino")
    ),
    ", happy optimization!\n",
    "Documentation: ",
    cli::style_hyperlink(doc_link, doc_link)
  )
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
