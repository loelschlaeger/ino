#' Constructor of \code{ino} objects.
#'
#' @description
#' This function constructs an object of class \code{ino}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

new_ino <- function() {
  out <- list()

  ### components
  out$f <- NA
  out$data <- list()
  out$optimizer <- list()
  out$optimizations <- list()

  class(out) <- "ino"
  return(out)
}

#' @export
#' @noRd

print.ino <- function(x, ...) {
  cat("<ino>")
}

#' @export
#' @noRd

summary.ino <- function(object, ...) {
  out <- list("f_set" = !identical(object[["f"]],NA),
              "data_set" = !identical(object[["data"]],list()),
              "no_data" = length(object[["data"]]),
              "optimizer_set" = !identical(object[["optimizer"]],list()),
              "no_optimizer" = length(object[["optimizer"]]),
              "no_optimizations" = length(object[["optimizations"]]))
  class(out) <- "summary.ino"
  return(out)
}

#' @export
#' @noRd
#' @importFrom crayon underline

print.summary.ino <- function(x, ...) {

  yes_no <- function(bool) {
    ifelse(bool, crayon::green("\U2713"), crayon::red("\U274C"))
  }

  ### summarize specifications
  cat(crayon::underline("Specifications\n"))
  cat("* f:", yes_no(x$f_set), "\n")
  cat("* data:", yes_no(x$data_set),
      ifelse(x$data_set,paste0("(",x$no_data,")"),""),"\n")
  cat("* optimizer:", yes_no(x$optimizer_set),
      ifelse(x$optimizer_set,paste0("(",x$no_optimizer,")"),""),"\n")

  ### summarize optimization
  cat(crayon::underline("\nOptimizations\n"))

  ### summarize evaluations
  cat(crayon::underline("\nEvaluations\n"))
}
