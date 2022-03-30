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
#' You can specify multiple values for each \code{...} parameter and multiple
#' optimizer (i.e. multiple values for \code{opt}). Such arguments
#' must be a list, where each list element must be a valid parameter value.
#' The names of these arguments must be added to the \code{mpvs} character
#' vector argument to make clear that you want to iterate over them.
#'
#' ### An example
#' Say that \code{f} is a likelihood function of \code{npar} parameters that has
#' the additional argument \code{data}, which is supposed to be a set of data
#' values. Say furthermore that you want to conduct a simulation experiment of
#' the initialization effect for \code{f} for two different data sets.
#' And last, say that you want to use the \code{\link[stats]{nlm}} optimizer
#' with a gradient tolerance of \code{gradtol = 1e-10} and save the number of
#' performed iterations (see the documentation of \code{\link{set_optimizer}}).
#' Then, specify
#' \preformatted{
#' setup_ino(
#'   f = f,
#'   npar = npar,
#'   data = list("data1" = <data set 1>,
#'               "data2" = <data set 2>),
#'   opt = set_optimizer_nlm(gradtol = 1e-10, crit = "iterations"),
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
#' The output of \code{set_optimizer()}.
#' Per default, \code{opt = set_optimizer("nlm")}.
#' Can also be a list, see the details.
#' @param mpvs
#' A character vector of the parameter names with multiple values, see the
#' details. Per default, \code{mpvs = character(0)}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @seealso
#' [set_optimizer()] to specify an optimizer.
#' [update_ino()] to replace or add specifications.
#' [test_ino()] to test the specifications.
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

setup_ino <- function(f, npar, ..., opt = set_optimizer("nlm"),
                      mpvs = character(0)) {

  ### check inputs
  if (missing(f)) {
    stop("'f' must be specified.")
  }
  if (class(f) != "function") {
    stop("'f' must be of class function.")
  }
  if (missing(npar)) {
    stop("'npar' must be specified.")
  }
  if (!is.numeric(npar) || length(npar) != 1 || npar %% 1 != 0 || npar < 1) {
    stop("'npar' must be a number.")
  }
  if (!is.character(mpvs)) {
    stop("'mpvs' must be a character (vector).")
  }

  ### build ino object
  ino <- list()
  class(ino) <- c("ino","list")
  ino$f <- list(f = f, npar = npar, add = list(...),
                name = deparse(substitute(f)))
  ino$opt <- opt
  ino$optimizations <- list()
  ino$mpvs <- mpvs

  ### make grid of parameter values
  ino$grid <- make_grid(ino)

  ### return ino object
  return(ino)
}

#' @export
#' @noRd
#' @importFrom crayon underline

print.ino <- function(x, show_arguments = FALSE, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  cat("- name:", x$f$name, "\n")
  cat("- npar:", x$f$npar, "\n")
  cat("- mpvs:", x$mpvs, "\n")
  if(show_arguments){
    cat("- arguments:\n")
    str(x$f$add, no.list = TRUE, give.head = FALSE)
  }
  cat(crayon::underline("Numerical optimizer\n"))
  cat("- name:", x$opt$name, "\n")
  cat("- crit:", x$opt$crit, "\n")
  if(show_arguments){
    cat("- arguments:\n")
    str(x$opt$args, no.list = TRUE, give.head = FALSE)
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

set_optimizer_nlm <- function(..., crit = c("minimum", "estimate", "code",
                                            "iterations")) {
  set_optimizer(opt = stats::nlm, f = "f", p = "p", list(...), crit = crit)
}


#' Update of an \code{ino} object
#'
#' @description
#' If you mis-specified an \code{ino} object or want to add specifications, you
#' do not have to start from scratch - use this function instead. The function
#' asks question in the console on whether specification in the \code{ino}
#' object should be replaced, removed, or added.
#'
#' @param x
#' An object of class \code{ino}.
#' @param ...
#' Any argument that could be passed to \code{\link{setup_ino}}.
#'
#' @return
#' The updated \code{ino} object \code{x}.
#'
#' @export
#'
#' @examples
#' # to be added
#'
#' @keywords
#' specification

update_ino <- function(x, ...) {



}


#' Test of an \code{ino} object
#'
#' @description
#' Use this function to test the specification of an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' No return value, prints the test results to the console.
#'
#' @export
#'
#' @keywords
#' specification

test_ino <- function(x) {

}



