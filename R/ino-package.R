#' @keywords internal
"_PACKAGE"

## usethis namespace: start
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
