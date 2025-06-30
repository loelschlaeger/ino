#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate assert_count
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_number
#' @importFrom checkmate assert_numeric
#' @importFrom checkmate assert_string
#' @importFrom checkmate check_choice
#' @importFrom checkmate check_class
#' @importFrom checkmate check_count
#' @importFrom checkmate check_flag
#' @importFrom checkmate check_function
#' @importFrom checkmate check_int
#' @importFrom checkmate check_integerish
#' @importFrom checkmate check_names
#' @importFrom checkmate check_number
#' @importFrom checkmate check_numeric
#' @importFrom checkmate check_string
#' @importFrom checkmate test_atomic_vector
#' @importFrom checkmate test_character
#' @importFrom checkmate test_integerish
#' @importFrom checkmate test_list
#' @importFrom checkmate test_number
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_bullets
#' @importFrom cli cli_h2
#' @importFrom cli cli_ol
#' @importFrom cli cli_warn
#' @importFrom cli style_hyperlink
#' @importFrom cli style_italic
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice_max
#' @importFrom dplyr slice_min
#' @importFrom dplyr summarize
#' @importFrom future multicore
#' @importFrom future multisession
#' @importFrom future plan
#' @importFrom future sequential
#' @importFrom future.apply future_apply
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 autoplot
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_contour_filled
#' @importFrom ggplot2 geom_function
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom normalize normalize
#' @importFrom oeli check_missing
#' @importFrom oeli check_numeric_vector
#' @importFrom oeli input_check_response
#' @importFrom oeli match_arg
#' @importFrom oeli Storage
#' @importFrom optimizeR Objective
#' @importFrom optimizeR Optimizer
#' @importFrom portion portion
#' @importFrom progressr handlers
#' @importFrom progressr progressor
#' @importFrom purrr flatten_chr
#' @importFrom purrr map
#' @importFrom purrr map_dfc
#' @importFrom R6 R6Class
#' @importFrom rlang .data
#' @importFrom rlang set_names
#' @importFrom stats complete.cases
#' @importFrom stats median
#' @importFrom stats nlm
#' @importFrom stats optim
#' @importFrom stats rnorm
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest_wider
#' @importFrom utils packageVersion
## usethis namespace: end
NULL

utils::globalVariables(c("x", "y", "z", ".optimization_label", "name", "value", "n"))

#' @noRd

.onAttach <- function(lib, pkg) {
  if(interactive()) {
    doc_link <- "https://loelschlaeger.de/ino"
    issues_link <- "https://github.com/loelschlaeger/ino/issues"
    msg <- c(
      paste0("This is {ino} ", utils::packageVersion("ino")),
      ", happy initialization!\n",
      "Documentation? ",
      cli::style_hyperlink(doc_link, doc_link), "\n",
      "Any issues? ",
      cli::style_hyperlink(issues_link, issues_link)
    )
    packageStartupMessage(msg)
  }
  invisible()
}

#' Example application to HMM likelihood
#'
#' @description
#' This object is saved for reproducibility and to save computation time when
#' building the vignettes.
#'
#' See the vignette about the HMM likelihood for details on how this object was
#' built: <https://loelschlaeger.de/ino/articles/example_hmm.html>
#'
#' @docType data
#'
#' @usage data("Nop_hmm")
#'
#' @format
#' A \code{\link{Nop}} object.
#'
#' @keywords internal
"Nop_hmm"

