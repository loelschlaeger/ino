#' Specify ao optimizer
#'
#' @description
#' This function is a wrapper for \code{\link[optimizeR]{define_optimizer}} with
#' the \code{\link[ao]{ao}} optimizer.
#'
#' @inheritParams optimizeR::define_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @export
#'
#' @importFrom ao ao
#' @importFrom optimizeR define_optimizer

optimizer_ao <- function(
    ..., output_ignore = character(), validate = TRUE,
    validation_settings = list()) {
  optimizeR::define_optimizer(
    optimizer = ao::ao,
    objective = "f",
    initial = "p",
    value = "optimum",
    parameter = "estimate",
    ...,
    output_ignore = output_ignore,
    validate = validate,
    validation_settings = validation_settings
  )
}
