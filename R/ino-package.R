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

.onAttach <- function(lib, pkg) {
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
  invisible()
}
