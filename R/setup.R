#' Setup of an \code{ino} object
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
#' output of \code{set_optimizer()}. Specifying multiple optimizer is possible,
#' see below.
#'
#' ### Specifying multiple parameter values
#' You can specify multiple values for each \code{...} parameter. Such arguments
#' must be a list, where each list element must be a valid parameter value.
#' The names of these arguments must be added to the \code{mpvs} character
#' vector argument to make clear that you want to iterate over them.
#'
#' ### Specifying multiple optimizer
#' Specifying multiple optimizer is analogue to specifying multiple parameter
#' values: Submit a list of \code{optimizer} objects (i.e. outputs of
#' \code{\link{set_optimizer}}) to the \code{opt} argument. However, you
#' don't need to add \code{"opt"} to \code{mpvs}.
#'
#' ### An example
#' Say that \code{f} is a likelihood function of \code{npar} parameters that has
#' the additional argument \code{data}, which is supposed to be a set of data
#' values. Say furthermore that you want to conduct a simulation experiment of
#' the initialization effect for \code{f} for two different data sets.
#' And last, say that you want to use the \code{\link[stats]{nlm}} optimizer
#' with two gradient tolerance \code{gradtol = 1e-6} and \code{gradtol = 1e-10},
#' and save the number of performed iterations (see the documentation of
#' \code{\link{set_optimizer}}). Then, specify
#' \preformatted{
#' setup_ino(
#'   f = f,
#'   npar = npar,
#'   data = list("data1" = <data set 1>,
#'               "data2" = <data set 2>),
#'   opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-6, crit = "iterations"),
#'              "opt2" = set_optimizer_nlm(gradtol = 1e-10, crit = "iterations")),
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
#' The output of \code{set_optimizer()}, which is an object of class
#' \code{optimizer}. Per default, \code{opt = set_optimizer_nlm()}, which is a
#' wrapper for the \code{\link[stats]{nlm}} optimizer. Can also be a list of
#' \code{optimizer} objects, see the details.
#' @param mpvs
#' A character vector of the parameter names with multiple values, see the
#' details. Per default, \code{mpvs = character(0)}.
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
#'   f = ino:::f_ll_hmm,
#'   npar = 4,
#'   data = ino::earthquakes,
#'   N = 2,
#'   neg = TRUE,
#'   opt = set_optimizer_nlm()
#' )
#'
#' @keywords
#' specification

setup_ino <- function(f, npar, ..., opt = set_optimizer_nlm(),
                      mpvs = character(0), verbose = FALSE) {

  ### check inputs
  if (missing(f)) stop("Please specify 'f'.")
  if (missing(npar)) stop("Plase specify 'npar'.")

  ### build ino object
  ino <- list()
  class(ino) <- c("ino","list")
  ino$f <- list(f = f, npar = npar, add = list(...), name = deparse(substitute(f)))
  ino$opt <- opt
  ino$optimizations <- list()
  ino$mpvs <- mpvs

  ### test ino object
  ino <- test_ino(ino, verbose = verbose)

  ### make grid of parameter values
  grid_pars <- list()
  for(mpv in ino$mpvs){
    grid_pars[[mpv]] <- names(ino$f$add[[mpv]])
  }
  if(!"optimizer" %in% class(ino$opt)){
    grid_pars[[".ino_opt"]] <- names(ino$opt)
  }
  ino$grid <- expand.grid(grid_pars)

  ### return ino object
  return(ino)
}


#' Test of an \code{ino} object
#'
#' @description
#' This helper function tests the specification of an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param verbose
#' Set to \code{TRUE} to show the print the test results of the setup to the
#' console.
#'
#' @return
#' The updated object \code{x}.
#'
#' @keywords
#' specification

test_ino <- function(x, verbose = FALSE) {

  ### helper functions
  topic <- function(name) { cat("*", name) }
  step <- function(desc) { cat("**", desc, "") }
  succ <- function() { cat(crayon::green("\U2713"),"\n") }
  fail <- function(msg) { cat(crayon::red("X"),"\n"); stop(msg, call. = FALSE) }
  warn <- function(msg) { cat(crayon::yellow("(\U2713)"),"\n"); warning(msg, call. = FALSE, immediate. =  TRUE) }

  ### start tests
  if(!verbose){
    sink(tempfile())
    on.exit(sink())
  }
  cat("Started tests.\n")

  ### check data types
  topic("Check data types.\n")
  step("'f' is of class 'function'")
  if(!"function" %in% class(x$f$f)) fail() else succ()
  step("'npar' is a number")
  if(!is.numeric(x$f$npar) || length(x$f$npar) != 1 || x$f$npar %% 1 != 0 || x$f$npar < 1) fail() else succ()
  step("'mpvs' is a character (vector)")
  if(!is.character(x$mpvs)) fail() else succ()

  ### check names of parameters with mpvs
  if(length(x$mpvs) > 0) {
    topic("Check names for parameters with multiple values.\n")
    for(mpv in x$mpvs){
      step(paste0("Check names for parameter '",mpv,"'"))
      if(length(names(x$f$add[[mpv]])) != length(x$f$add[[mpv]])){
        names(x$f$add[[mpv]]) <- paste0(mpv,1:length(x$f$add[[mpv]]))
        warn(paste0("Named '",mpv,"' by '",mpv,"1:",length(x$f$add[[mpv]]),"'"))
      } else succ()
    }

  }

  ### return updated ino object
  return(x)

}

#' @export
#' @noRd
#' @importFrom crayon underline
#' @importFrom utils str

print.ino <- function(x, show_arguments = FALSE, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  cat("- name:", x$f$name, "\n")
  cat("- npar:", x$f$npar, "\n")
  cat("- mpvs:", x$mpvs, "\n")
  if(show_arguments){
    cat("- arguments:\n")
    utils::str(x$f$add, no.list = TRUE, give.head = FALSE)
  }
  cat(crayon::underline("Numerical optimizer\n"))
  if("opt" %in% x$mpvs){
    opts <- x$opt
  } else {
    opts <- list(x$opt)
  }
  for(i in seq_along(opts)){
    if("opt" %in% x$mpvs) {
      cat(paste0(i,":\n"))
    }
    cat("- name:", opts[[i]]$name, "\n")
    cat("- crit:", opts[[i]]$crit, "\n")
    if(show_arguments){
      cat("- arguments:\n")
      utils::str(opts[[i]]$args, no.list = TRUE, give.head = FALSE)
    }
  }
}

#' Specify a numerical optimizer for an \code{ino} object
#'
#' @description
#' Use this function to specify a numerical optimizer for the optimization
#' problem.
#'
#' @details
#' Numerical optimization functions specified for the \code{opt} argument must
#' fulfill the following requirements:
#' \itemize{
#'   \item function input name f
#'   \item starting parameter values input name p
#'   \item additional parameters to f via ...
#'   \item output is a named list.
#' }
#'
#' @param opt
#' An object of class \code{function}, a numerical optimizer, see the details.
#' @param f
#' The name of the function input of \code{opt}.
#' @param p
#' The name of the starting parameter values input of \code{opt}.
#' @param ...
#' Additional arguments to be passed to the optimizer \code{opt}. Without
#' specifications, the default values of \code{opt} are used.
#' @param crit
#' The names of the list elements in the output of \code{opt} to be saved after
#' the optimization.
#'
#' @return
#' An object of class \code{optimizer}, which can be passed to
#' \code{\link{setup_ino}}.
#'
#' @seealso
#' [set_optimizer_nlm()], a wrapper for the \code{\link[stats]{nlm}} optimizer.
#'
#' @export
#'
#' @examples
#' set_optimizer(
#'   opt = pracma::nelder_mead,
#'   f = "fn",
#'   p = "x0",
#'   tol = 1e-6,
#'   crit = c("fcount")
#' )
#'
#' @keywords
#' specification

set_optimizer <- function(opt, f, p, ..., crit = character(0)) {

  ### check inputs
  if (missing(opt)) {
    stop("'opt' must be specified.")
  }
  if (class(opt) != "function") {
    stop("'opt' must be of class function.")
  }
  if (missing(f)) {
    stop("'f' must be specified.")
  }
  if (!is.character(f) || length(f) != 1) {
    stop("'f' must be a character.")
  }
  if (missing(p)) {
    stop("'p' must be specified.")
  }
  if (!is.character(p) || length(p) != 1) {
    stop("'p' must be a character.")
  }
  if (!is.character(crit)) {
    stop("'crit' must be a character (vector).")
  }

  ### save arguments of 'opt'
  add_args <- list(...)
  all_args <- formals(opt)
  add_args <- add_args[which(names(add_args) %in% names(all_args))]
  exclude <- c(f, p, names(add_args), "...")
  def_args <- all_args[!names(all_args) %in% exclude]
  args <- c(add_args, def_args)

  ### build and return optimizer object
  optimizer <- list()
  optimizer$f <- opt
  optimizer$args <- args
  optimizer$crit <- crit
  optimizer$name <- deparse(substitute(opt))
  class(optimizer) <- c("optimizer","list")
  return(optimizer)

}

#' Specify the \code{\link[stats]{nlm}} optimizer
#'
#' @inheritParams set_optimizer
#'
#' @return
#' An object of class \code{optimizer}, which can be passed to
#' \code{\link{setup_ino}}.
#'
#' @seealso
#' [set_optimizer()], for specifying a different optimizer.
#'
#' @export
#'
#' @keywords
#' specification

set_optimizer_nlm <- function(..., crit = c("minimum", "estimate", "code", "iterations")) {
  set_optimizer(opt = stats::nlm, f = "f", p = "p", list(...), crit = crit)
}
