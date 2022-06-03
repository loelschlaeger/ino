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
                      mpvs = character(0), verbose = getOption("ino_progress")) {

  ### check inputs
  if (missing(f)) stop("Please specify 'f'.")
  if (missing(npar)) stop("Plase specify 'npar'.")
  if (length(verbose) != 1 || !is.logical(verbose)) stop("'verbose' must be a boolean.")

  ### build ino object
  ino <- list()
  class(ino) <- c("ino","list")
  add <- list(...)
  for(add_name in names(add)) {
    if(!add_name %in% mpvs) {
      add[[add_name]] <- list(add[[add_name]])
    }
  }
  ino$f <- list(f = f, npar = npar, add = list(...), name = deparse(substitute(f)),
                target_arg = names(formals(f))[1], mpvs = mpvs)
  if("optimizer" %in% class(opt)) {
    ino$opt <- list("opt" = opt)
  } else {
    ino$opt <- opt
  }
  ino$runs <- list("table" = data.frame(), "pars" = list())

  ### test ino object
  ino <- test_ino(ino, verbose = verbose)

  ### return ino object
  return(ino)
}

#' Create grid of parameter combinations
#'
#' @description
#' This helper function creates a grid of all parameter combinations for an
#' \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' A list, where each element is a parameter set. Each parameter set contains
#' at least a placeholder for the target parameter, which is set to \code{NA}
#' and has to be filled by the initialization strategies. Additionally, each
#' parameter set contains further arguments for the target function if
#' available. In this case, the parameter names and identifier for the parameter
#' values are added as attributes \code{"par_name"} and \code{"par_id"} to the
#' parameter set.
#'
#' @keywords
#' specification

grid_ino <- function(x) {

  ### build grid of parameter identifiers
  grid_par <- as.list(names(x$f$add))
  names(grid_par) <- names(x$f$add)
  for(mpv in x$f$mpvs) grid_par[[mpv]] <- names(x$f$add[[mpv]])
  grid_par <- expand.grid(grid_par, stringsAsFactors = FALSE)

  ### build list of parameter sets
  par_sets <- list()
  for(i in 1:max(1,nrow(grid_par))) {
    target <- list(NA)
    names(target) <- x$f$target_arg
    par_set <- c(target, x$f$add)
    for(p in colnames(grid_par)) {
      if(inherits(par_set[[p]], "list")){
        par_set[p] <- par_set[[p]][grid_par[i,p]]
      } else {
        par_set[p] <- par_set[p][grid_par[i,p]]
      }
    }
    attr(par_set, "par_name") <- as.character(names(x$f$add))
    attr(par_set, "par_id") <- as.character(grid_par[i,])
    par_sets[[i]] <- par_set
  }

  ### return list of parameter sets
  return(par_sets)
}


#' Save results of optimization run
#'
#' @description
#' This helper function saves the results of an optimization run into the
#' submitted \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param strategy
#' A character, the name of the initialization strategy.
#' @param pars
#' A list of parameter values for the optimization run.
#' @param result
#' The output of \code{\link{do.call_timed}}.
#' @param opt_name
#' The identifier of the optimizer.
#'
#' @return
#' The updated object \code{x} (invisibly).
#'
#' @keywords
#' specification

result_ino <- function(x, strategy, pars, result, opt_name) {

  ### determine number of new optimization result
  nopt <- nrow(x$runs$table) + 1

  ### save optimization results
  x$runs$table[nopt, ".strategy"] <- strategy
  x$runs$table[nopt, ".time"] <- result$time
  x$runs$table[nopt, ".optimizer"] <- opt_name
  x$runs$table[nopt, attr(pars, "par_name")] <- attr(pars, "par_id")
  x$runs$pars[[nopt]] <- list()
  x$runs$pars[[nopt]][[".init"]] <- pars[[x$f$target_arg]]
  opt_crit <- x$opt[[opt_name]]$crit
  crit_val <- result$res[opt_crit]
  for(i in 1:length(opt_crit)) {
    if(is.numeric(crit_val[[i]]) && length(crit_val[[i]]) == 1){
      x$runs$table[nopt, opt_crit[i]] <- crit_val[[i]]
    } else {
      x$runs$pars[[nopt]][[opt_crit[i]]] <- crit_val[[i]]
    }
  }

  ### return (invisibly) updated ino object
  return(invisible(x))
}


#' Test of an \code{ino} object
#'
#' @description
#' This helper function tests the specification of an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param verbose
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) the test results of the
#' setup at the console.
#'
#' @return
#' The updated object \code{x} (invisibly).
#'
#' @keywords
#' specification
#'
#' @importFrom stats rnorm

test_ino <- function(x, verbose = getOption("ino_progress")) {

  ### helper functions
  ll <- NULL
  topic <- function(desc) pline(paste0("* ", desc))
  step <- function(desc) pline(desc)
  res <- function(msg = NULL, succ = FALSE, warn = FALSE) {
    if(succ) {
      cat(crayon::green("\U2713 "))
    } else if(warn){
      cat(crayon::yellow("X "))
      warning(msg, call. = FALSE, immediate. =  TRUE)
      ll <<- NULL
    } else {
      cat("\n")
      if(is.null(msg)) {
        msg <- paste("Please make sure that", ll)
      }
      stop(msg, call. = FALSE)
    }
  }
  pline <- function(line = NULL) {
    if(!is.null(ll)) cat(crayon::silver(ll), "\n", sep = "")
    ll <<- line
    cat(line, "\r")
    Sys.sleep(ifelse(verbose,0.1,0))
    return(line)
  }

  ### start tests
  if(!verbose) { sink(tempfile()); on.exit(sink()) }
  topic("start tests")

  ### check data types
  topic("check data types")
  step("'f' is of class 'function'")
  res(msg = "",
      succ = "function" %in% class(x$f$f))
  step("'npar' is a numeric")
  res(msg = "",
      succ = is.numeric(x$f$npar))
  step("'npar' is of length 1")
  res(succ = length(x$f$npar) == 1)
  step("'npar' is a whole number")
  res(msg = "",
      succ = x$f$npar %% 1 == 0)
  step("'npar' is non-negative")
  res(msg = "",
      succ = x$f$npar > 0)
  step("'opt' is of class 'optimizer' or a list of those")
  res(msg = "'opt' is not of class 'optimizer' or a list of those",
      succ = all(sapply(x$opt, function(x) "optimizer" %in% class(x))))
  step("'mpvs' is a character (vector)")
  res(msg = "",
      succ = is.character(x$f$mpvs))

  ### check names of parameters with mpvs
  topic("check names for parameters with multiple values")
  for(mpv in x$f$mpvs){
    step(paste0("check names for parameter '",mpv,"'"))
    if(length(names(x$f$add[[mpv]])) == length(x$f$add[[mpv]])) {
      res(succ = TRUE)
    } else {
      res(msg = paste0("Named '",mpv,"' by '",mpv,"1:",length(x$f$add[[mpv]]),"'"),
          succ = FALSE,
          warn = TRUE)
      names(x$f$add[[mpv]]) <- paste0(mpv,1:length(x$f$add[[mpv]]))
    }
  }

  ### check that function and optimizer can be called
  step("check name of target parameter in 'f'")
  res(succ = is.character(x$f$target_arg))
  topic(paste("name of target parameter in 'f':", x$f$target_arg))
  rvx <- round(rnorm(x$f$npar),1)
  step(paste0("draw value of length 'npar' = ", x$f$npar, ": ",
              paste(rvx, collapse = " ")))
  res(succ = (length(rvx) == x$f$npar))
  step("create grid of parameter sets")
  grid <- grid_ino(x)
  res(succ = is.list(grid))
  topic(paste("number of parameter sets:", length(grid)))
  if(length(grid) > 10) {
    topic("randomly select 10 parameter sets for testing")
    grid <- grid[sort(sample.int(length(grid), 10))]
  }
  topic("check that function 'f' can be called")
  for(i in seq_along(grid)){
    step(paste("parameter set:", paste(attr(grid[[i]], "par_id"), collapse = " ")))
    pars <- grid[[i]]
    pars[[x$f$target_arg]] <- rvx
    f_return <- try_silent(
      timed(expr = do.call(what = x$f$f, args = pars),
            secs = 1)
    )
    res(msg = "msg",
        succ = !inherits(f_return, "fail"))
  }

  ### return (invisibly) updated ino object
  step("completed test cases")
  return(invisible(x))

}

#' @export
#' @noRd
#' @importFrom crayon underline
#' @importFrom utils str

print.ino <- function(x, show_arguments = FALSE, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  cat("- name:", x$f$name, "\n")
  cat("- npar:", x$f$npar, "\n")
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
#'   \item output is a named list
#'   \item name of optimal parameter vector in the output list is z
#' }
#'
#' @param opt
#' An object of class \code{function}, a numerical optimizer, see the details.
#' @param f
#' The name of the function input of \code{opt}.
#' @param p
#' The name of the starting parameter values input of \code{opt}.
#' @param z
#' The name of the optimal parameter vector in the output list of \code{opt}.
#' @param ...
#' Additional arguments to be passed to the optimizer \code{opt}. Without
#' specifications, the default values of \code{opt} are used.
#' @param crit
#' The names of the list elements in the output of \code{opt} to be saved after
#' the optimization. Must contain \code{z}.
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
#'   z = "xmin",
#'   tol = 1e-6,
#'   crit = c("xmin", "fcount")
#' )
#'
#' @keywords
#' specification

set_optimizer <- function(opt, f, p, z, ..., crit = c(z)) {

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
  if (missing(z)) {
    stop("'z' must be specified.")
  }
  if (!is.character(z) || length(z) != 1) {
    stop("'z' must be a character.")
  }
  if (!is.character(crit)) {
    stop("'crit' must be a character (vector).")
  }
  if (!z %in% crit) {
    stop("'z' must be contained in 'crit'.")
  }

  ### build and return optimizer object
  optimizer <- list()
  optimizer$f <- opt
  optimizer$base_arg_names <- c(f,p,z)
  optimizer$args <- list(...)[[1]]
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
  set_optimizer(opt = stats::nlm, f = "f", p = "p", z = "estimate",
                list(...), crit = crit)
}

#' Set true parameter values
#'
#' @description
#' This function specifies true parameter values.
#'
#' @param
#' NA
#'
#' @return
#' NA
#'
#' @export
#'
#' @examples
#' NA
#'
#' @keywords
#' specification

set_true <- function() {

}

#' Clear optimization runs
#'
#' @description
#' This function clears optimization runs saved in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param which
#' Either \code{"all"} to clear all optimization runs, or alternatively a
#' numeric vector of row numbers in \code{x$runs$table}.
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @examples
#' NA
#'
#' @keywords
#' specification

clear_optimizations <- function(x, which = "all") {
  ino_check_inputs("x" = x, "which" = which)
  if(identical(which, "all")) {
    x$runs$table <- data.frame()
    x$runs$pars <- list()
  } else {
    x$runs$table <- x$runs$table[-which, , drop = FALSE]
    rownames(x$runs$table) <- NULL
    x$runs$pars <- x$runs$pars[-which, drop = FALSE]
  }
  return(x)
}
