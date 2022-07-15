#' Setup
#'
#' @description
#' Use this function to specify the numerical optimization problem. The function
#' returns an object of class \code{ino} that contains all specifications.
#'
#' @details
#' ### Specifying a function
#' One function \code{f} must be specified per \code{ino} object. The function
#' is optimized over its first argument, which should be a numeric vector of
#' length \code{npar}, followed by any other arguments specified via the
#' \code{...} argument.
#'
#' ### Specifying an optimizer
#' The numerical optimizer must be specified via the \code{opt} argument as the
#' output of \code{\link{set_optimizer}}. You can specify multiple optimizer for
#' comparison, see below.
#'
#' ### Specifying multiple parameter values
#' You can specify multiple values for each \code{...} parameter. Such arguments
#' must be in \code{list} format, where each list element must be a valid
#' parameter value. The names of these arguments must be added to the
#' \code{mpvs} input to make clear that you want to iterate over them.
#'
#' ### Specifying multiple optimizer
#' Specifying multiple optimizer is analogue to specifying multiple parameter
#' values: Submit a list of \code{optimizer} objects (i.e. outputs of
#' \code{\link{set_optimizer}}) to the \code{opt} argument.
#'
#' ### An example
#' Say that \code{f} is a likelihood function of \code{npar} parameters that has
#' the additional argument \code{data}. Say furthermore that you want to conduct
#' a simulation experiment of the initialization effect for \code{f} for two
#' different data sets. And last, say that you want to compare the
#' \code{\link[stats]{nlm}} optimizer and the \code{\link[stats]{optim}}
#' optimizer. Then, specify
#' \preformatted{
#' setup_ino(
#'   f = f,
#'   npar = npar,
#'   data = list("data1" = <data set 1>,
#'               "data2" = <data set 2>),
#'   opt = list("nlm"   = set_optimizer_nlm(),
#'              "optim" = set_optimizer_optim()),
#'   mpvs = c("data", "opt")
#' )
#' }
#'
#' @param f
#' An object of class \code{function}, the function to be optimized.
#' @param npar
#' The length of the first argument of \code{f}, i.e. the argument over which
#' \code{f} is optimized.
#' @param ...
#' Additional and named arguments to be passed to \code{f} (optional).
#' @param opt
#' The output of \code{\link{set_optimizer}}, which is an object of class
#' \code{optimizer}. Per default, \code{opt = set_optimizer_nlm()}, which
#' specifies the \code{\link[stats]{nlm}} optimizer. Can also be a list of
#' different \code{optimizer} objects, see the details.
#' @param mpvs
#' A character vector of the argument names with multiple parameter values, see
#' the details. Per default, \code{mpvs = character(0)}.
#' @param skip_test
#' Set to \code{TRUE} to skip the specification tests.
#' @inheritParams test_ino
#'
#' @return
#' An object of class \code{ino}.
#'
#' @seealso
#' [set_optimizer()] to specify an optimizer.
#'
#' @export
#'
#' @examples
#' setup_ino(
#'   f = f_ll_hmm,
#'   npar = 4,
#'   data = earthquakes,
#'   N = 2,
#'   neg = TRUE,
#'   opt = set_optimizer_nlm()
#' )
#'
#' @keywords
#' specification

setup_ino <- function(
  f, npar, ..., opt = set_optimizer_nlm(), mpvs = character(0),
  verbose = getOption("ino_progress"), skip_test = FALSE
  ) {

  ### check inputs
  if (missing(f))
    stop("Please specify 'f'.",
         call. = FALSE)
  if (missing(npar))
    stop("Plase specify 'npar'.",
         call. = FALSE)
  if (length(verbose) != 1 || !is.logical(verbose))
    stop("'verbose' must be a boolean.",
         call. = FALSE)

  ### build ino object
  x <- list()
  class(x) <- c("ino","list")
  add <- list(...)
  for(add_name in names(add)) {
    if(!add_name %in% mpvs) {
      add[[add_name]] <- list(add[[add_name]])
    }
  }
  x$f <- list(
    f = f, npar = npar, add = list(...), name = deparse(substitute(f)),
    target_arg = names(formals(f))[1], mpvs = mpvs)
  if("optimizer" %in% class(opt)) {
    x$opt <- list("opt" = opt)
  } else {
    x$opt <- opt
  }
  x$runs <- list("table" = data.frame(), "pars" = list())

  ### test ino object
  if(!skip_test) x <- test_ino(x, verbose = verbose)

  ### return ino object
  return(x)
}

#' @exportS3Method
#' @noRd
#' @importFrom crayon underline
#' @importFrom utils str

print.ino <- function(x, show_arguments = FALSE, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  cat("- name:", x$f$name, "\n")
  cat("- npar:", x$f$npar, "\n")
  if(length(x$f$mpvs) > 0)
    cat("- mpvs:", x$f$mpvs, "\n")
  if(show_arguments){
    cat("- arguments:\n")
    utils::str(x$f$add, no.list = TRUE, give.head = FALSE)
  }
  cat(crayon::underline("Numerical optimizer\n"))
  for(i in seq_along(x$opt)){
    if(length(x$opt) > 1) cat(paste0(i,":\n"))
    cat("- name:", x$opt[[i]]$name, "\n")
    cat("- crit:", x$opt[[i]]$crit, "\n")
    if(show_arguments){
      cat("- arguments:\n")
      utils::str(x$opt[[i]]$args, no.list = TRUE, give.head = FALSE)
    }
  }
}

#' Specify numerical optimizer
#'
#' @description
#' Use this function to specify a numerical optimizer for the optimization
#' problem.
#'
#' @details
#' Numerical optimizer specified for the \code{opt} argument must fulfill the
#' following requirements:
#' \itemize{
#'   \item it must have an input \code{f} for the function to be optimized,
#'   \item it must have an input \code{p} for the starting parameter values,
#'   \item it must have a \code{...} argument for additional parameters to
#'         \code{f},
#'   \item the output must be a named list, including the optimal function value
#'         (named \code{v}) and parameter vector (named \code{z}).
#' }
#'
#' @param opt
#' An object of class \code{function}, a numerical optimizer.
#' @param f
#' The name of the function input of \code{opt}.
#' @param p
#' The name of the starting parameter values input of \code{opt}.
#' @param v
#' The name of the optimal function value in the output list of \code{opt}.
#' @param z
#' The name of the optimal parameter vector in the output list of \code{opt}.
#' @param ...
#' Additional arguments to be passed to the optimizer \code{opt}. Without
#' specifications, the default values of \code{opt} are used.
#' @param crit
#' The names of additional elements in the output of \code{opt} to be saved
#' after the optimization.
#'
#' @return
#' An object of class \code{optimizer}, which can be passed to
#' \code{\link{setup_ino}}.
#'
#' @seealso
#' [set_optimizer_nlm()] and [set_optimizer_optim()], two wrappers for the
#' \code{\link[stats]{nlm}} and \code{\link[stats]{optim}} optimizer.
#'
#' @export
#'
#' @examples
#' set_optimizer(
#'   opt = pracma::nelder_mead,
#'   f = "fn",
#'   p = "x0",
#'   v = "fmin",
#'   z = "xmin",
#'   tol = 1e-6,
#'   crit = c("xmin", "fcount")
#' )
#'
#' @keywords
#' specification

set_optimizer <- function(opt, f, p, v, z, ..., crit = character(0)) {

  ### check inputs
  if (missing(opt)) {
    stop("'opt' must be specified.", call. = FALSE)
  }
  if (!inherits(opt,"function")) {
    stop("'opt' must be of class function.", call. = FALSE)
  }
  if (missing(f)) {
    stop("'f' must be specified.", call. = FALSE)
  }
  if (!is.character(f) || length(f) != 1) {
    stop("'f' must be a character.", call. = FALSE)
  }
  if (missing(p)) {
    stop("'p' must be specified.", call. = FALSE)
  }
  if (!is.character(p) || length(p) != 1) {
    stop("'p' must be a character.", call. = FALSE)
  }
  if (missing(v)) {
    stop("'v' must be specified.", call. = FALSE)
  }
  if (!is.character(v) || length(v) != 1) {
    stop("'v' must be a character.", call. = FALSE)
  }
  if (missing(z)) {
    stop("'z' must be specified.", call. = FALSE)
  }
  if (!is.character(z) || length(z) != 1) {
    stop("'z' must be a character.", call. = FALSE)
  }
  if (!is.character(crit)) {
    stop("'crit' must be a character (vector).", call. = FALSE)
  }
  if (v %in% crit) {
    crit <- crit[-which(v==crit)]
  }
  if (z %in% crit) {
    crit <- crit[-which(z==crit)]
  }

  ### build and return optimizer object
  optimizer <- list()
  optimizer$f <- opt
  optimizer$base_arg_names <- c(f,p,v,z)
  optimizer$args <- list(...)[[1]]
  optimizer$crit <- crit
  optimizer$name <- deparse(substitute(opt))
  class(optimizer) <- c("optimizer","list")
  return(optimizer)
}

#' @exportS3Method
#' @noRd

print.optimizer <- function(x, ...) {
  cat("<optimizer ",x$name,">", sep = "")
}

#' Specify \code{\link[stats]{nlm}} optimizer
#'
#' @inheritParams set_optimizer
#'
#' @return
#' An object of class \code{optimizer}, which can be passed to
#' \code{\link{setup_ino}}.
#'
#' @seealso
#' [set_optimizer()] for specifying a different optimizer.
#'
#' @export
#'
#' @keywords
#' specification

set_optimizer_nlm <- function(..., crit = c("code", "iterations")) {
  set_optimizer(
    opt = stats::nlm, f = "f", p = "p", v = "minimum" , z = "estimate",
    list(...), crit = crit
  )
}

#' Specify \code{\link[stats]{optim}} optimizer
#'
#' @inheritParams set_optimizer
#'
#' @return
#' An object of class \code{optimizer}, which can be passed to
#' \code{\link{setup_ino}}.
#'
#' @seealso
#' [set_optimizer()] for specifying a different optimizer.
#'
#' @export
#'
#' @keywords
#' specification

set_optimizer_optim <- function(..., crit = c("convergence")) {
  set_optimizer(
    opt = stats::optim, f = "fn", p = "par", v = "value", z = "par", list(...),
    crit = crit
  )
}

#' Clear initialization runs
#'
#' @description
#' This function clears initialization runs saved in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param which
#' Either \code{"all"} to clear all initialization runs, or alternatively a
#' numeric vector of row numbers in \code{x$runs$table}.
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' specification

clear_ino <- function(x, which = "all") {
  ino_check_inputs("x" = x, "which" = which)
  if(identical(which, "all")) {
    x[["runs"]][["table"]] <- data.frame()
    x[["runs"]][["pars"]] <- list()
  } else {
    x[["runs"]][["table"]] <- x[["runs"]][["table"]][-which, , drop = FALSE]
    rownames(x[["runs"]][["table"]]) <- NULL
    x[["runs"]][["pars"]] <- x[["runs"]][["pars"]][-which, drop = FALSE]
  }
  return(x)
}

#' Merge initialization runs
#'
#' @description
#' This function merges multiple \code{ino} objects.
#'
#' @param ...
#' Arbitrary many \code{ino} objects, of which the initialization results are
#' merged into the first object, which is then returned.
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' specification

merge_ino <- function(...) {
  ino_objects <- list(...)
  if(length(ino_objects) == 0) {
    return()
  }
  class <- sapply(lapply(ino_objects, class), function(x) any("ino" %in% x))
  if(any(!class)){
    stop("Object(s) at position(s) ", paste(which(!class), collapse = ", "),
         " not of class 'ino'.", call. = FALSE)
  }
  base <- ino_objects[[1]]
  if(length(ino_objects) > 1) {
    for(i in 2:length(ino_objects)) {
      base$runs$table <- rbind(base$runs$table, ino_objects[[i]]$runs$table)
      base$runs$pars <- c(base$runs$pars, ino_objects[[i]]$runs$pars)
    }
  }
  return(base)
}
