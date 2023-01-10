#' Specify ao optimizer
#'
#' @description
#' This function is a wrapper for \code{\link[optimizeR]{set_optimizer}} with
#' the \code{\link[ao]{ao}} optimizer.
#'
#' @inheritParams optimizeR::set_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @export
#'
#' @importFrom ao ao
#' @importFrom optimizeR set_optimizer

optimizer_ao <- function(..., out_ign = character(), test_par = list()) {
  if ("partition" %in% names(list(...)) && identical(test_par, list())) {
    test_par$validate <- FALSE
  }
  optimizeR::set_optimizer(
    opt_fun = ao::ao,
    f = "f",
    p = "p",
    v = "optimum",
    z = "estimate",
    ...,
    out_ign = out_ign,
    test_par = test_par
  )
}
