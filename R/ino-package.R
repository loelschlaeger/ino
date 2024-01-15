#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate assert_count
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom forcats fct_reorder
#' @importFrom future.apply future_apply
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom normalize normalize
#' @importFrom numDeriv grad
#' @importFrom optimizeR Objective
#' @importFrom portion portion
#' @importFrom progressr progressor
#' @importFrom rlang .data
#' @importFrom scales label_percent
#' @importFrom scales percent
#' @importFrom stats complete.cases
#' @importFrom stats median
#' @importFrom stats nlm
#' @importFrom stats rnorm
## usethis namespace: end
NULL

#' @noRd
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0("Thanks for using {ino} version ", utils::packageVersion("ino")), "!\n",
    "See ", cli::style_hyperlink("https://loelschlaeger.de/ino", "https://loelschlaeger.de/ino") ," for help."
  )
  packageStartupMessage(msg)
  invisible()
}
