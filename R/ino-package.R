#' @keywords internal
"_PACKAGE"

#' @keywords internal
.onAttach <- function(libname, pkgname){
  options("ino_connection" = stdin())
}

## usethis namespace: start
#' @importFrom optimizeR define_optimizer
#' @importFrom optimizeR optimizer_nlm
#' @importFrom optimizeR optimizer_optim
#' @importFrom TestFunctions TF_ackley
## usethis namespace: end
NULL

#' Example application to mixture likelihood
#'
#' @description
#' See the introduction vignette for details:
#' <https://loelschlaeger.de/ino/articles/ino.html>
#'
#' @docType data
#'
#' @usage data("ino_mixture")
#'
#' @format
#' A \code{\link{Nop}} object.
#'
#' @keywords demos
"ino_mixture"

#' Example application to HMM likelihood
#'
#' @description
#' See the vignette about the HMM likelihood for details:
#' <https://loelschlaeger.de/ino/articles/example_hmm.html>
#'
#' @docType data
#'
#' @usage data("ino_hmm")
#'
#' @format
#' A \code{\link{Nop}} object.
#'
#' @keywords demos
"ino_hmm"

#' Example application to probit likelihood
#'
#' @description
#' See the vignette about the probit likelihood for details:
#' <https://loelschlaeger.de/ino/articles/example_probit.html>
#'
#' @docType data
#'
#' @usage data("ino_probit")
#'
#' @format
#' A \code{\link{Nop}} object.
#'
#' @keywords demos
"ino_probit"
