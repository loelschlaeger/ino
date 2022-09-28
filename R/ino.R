#' Setup
#'
#' @description
#' Use this function to specify the numerical optimization problem. The function
#' returns an object of class \code{ino} that contains all specifications.
#'
#' @details
#' ### Specifying a function
#' One real-valued function \code{f} must be specified per \code{ino} object.
#' The function is optimized over its first argument, the target argument, which
#' must be a numeric vector of length \code{npar}, followed by any other
#' arguments specified via the \code{...} argument.
#'
#' ### Specifying multiple parameter values
#' You can specify multiple values for each \code{...} parameter for comparison.
#' Such arguments must be in \code{list} format, where each list element must be
#' a valid parameter value. The list elements can be named.
#' The names of the \code{...} parameters with multiple values must be added to
#' the \code{mpvs} input.
#'
#' ### Specifying an optimizer
#' The numerical optimizer must be specified via the \code{opt} argument as an
#' \code{optimizer} object.
#' Such \code{optimizer} objects can be created via the function
#' \code{\link[optimizeR]{set_optimizer}}.
#' You can specify multiple \code{optimizer} objects for comparison by passing
#' a (named) list of optimizers to \code{opt}.
#'
#' ### An example
#' Let \code{nll} be a negative log-likelihood function.
#' Its first argument is a numeric vector of length \code{2}.
#' The global optimum is obtained in the origin.
#' The function has the additional argument \code{data}.
#' Say that you want to conduct an experiment of the initialization effect for
#' \code{nll} for two different data sets.
#' And say that you want to compare the \code{\link[stats]{nlm}} and the
#' \code{\link[stats]{optim}} optimizer.
#' Then, specify
#' \preformatted{
#' setup_ino(
#'   f = nll,
#'   npar = 2,
#'   global = c(0,0),
#'   data = list("data1" = <data set 1>,
#'               "data2" = <data set 2>),
#'   mpvs = "data",
#'   opt = list("nlm"   = set_optimizer_nlm(),
#'              "optim" = set_optimizer_optim())
#' )
#' }
#'
#' @format
#' The format of an \code{ino} object is documented in \code{\link{new_ino}}.
#'
#' @param ...
#' Additional and named arguments to be passed to \code{f} (optional).
#' @inheritParams new_ino
#' @inheritParams validate_ino
#' @inheritParams random_initialization
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
#'   neg = TRUE
#' )
#'
#' @keywords
#' specification

setup_ino <- function(
    f, npar, global = NULL, ..., mpvs = character(), opt = set_optimizer_nlm(),
    test_par = list(
      validate = TRUE,
      init_rest = list("lower" = -1, "upper" = 1),
      init_digits = 2,
      f_checks = 10,
      f_checks_time = 1
    ), verbose = getOption("ino_progress")
) {
  if (missing(f)) {
    ino_stop(
      "Argument 'f' is not specified."
    )
  }
  if (missing(npar)) {
    ino_stop(
      "Argument 'npar' is not specified."
    )
  }
  x <- validate_ino(
    x = new_ino(
      f = f, npar = npar, global = global, add = list(...), mpvs = mpvs,
      f_name = deparse(substitute(f)), f_target = names(formals(f))[1],
      opt = opt
    ),
    test_par = test_par
  )
  ino_status("Setup ino", verbose = verbose)
  if(verbose) print(x)
  return(x)
}

#' Constructor
#'
#' @description
#' This function constructs an \code{ino} object.
#'
#' @format
#' An \code{ino} object is a list of three elements:
#' * The \code{prob} element is an object of class \code{prob}.
#'   It defines the optimization problem and is documented in
#'   \code{\link{new_prob}}.
#' * The \code{opts} element is an object of class \code{opts}.
#'   It defines the optimizer(s) and is documented in \code{\link{new_opts}}.
#' * The \code{runs} element is an object of class \code{runs}.
#'   It is the storage space for the optimization results and documented in
#'   \code{\link{new_runs}}.
#'
#' @param x
#' A list.
#' @inheritParams new_prob
#' @inheritParams new_opts
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

new_ino <- function(
    x = list(), f = function() {}, npar = integer(), global = NULL,
    add = list(), mpvs = character(), f_name = character(),
    f_target = character(), opt = list()
) {
  stopifnot(is.list(x))
  stopifnot(is.function(f))
  if(is.numeric(npar)) {
    npar <- as.integer(npar)
  }
  stopifnot(is.integer(npar))
  stopifnot(is.numeric(global) || is.null(global))
  stopifnot(is.list(add))
  stopifnot(is.character(mpvs))
  stopifnot(is.character(f_name))
  stopifnot(is.character(f_target))
  stopifnot(is.list(opt))
  x[["prob"]] <- new_prob(
    f = f, npar = npar, global = global, add = add, mpvs = mpvs,
    f_name = f_name, f_target = f_target)
  x[["opts"]] <- new_opts(opt = opt)
  x[["runs"]] <- new_runs()
  structure(x, class = "ino")
}

#' Validator
#'
#' @description
#' This function validates an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param test_par
#' A list of test parameters for an \code{ino} object:
#' * \code{validate}, a Boolean, set to \code{TRUE} (\code{FALSE}) to (not)
#'   validate the \code{ino} object. Per default, \code{validate = TRUE}.
#' * \code{init_rest}, a list of two elements, \code{lower} and \code{upper},
#'   with lower and upper limits, respectively, for test values for \code{f}.
#'   Can be single values (for joint limits) or numeric vectors of length
#'   \code{npar} (for individual limits).
#'   Per default, \code{lower = -1} and \code{upper = 1}.
#' * \code{init_digits}, the number of decimal places for the test initial
#'   values. Per default, \code{init_digits = 2}.
#' * \code{f_checks}, the number of checks for \code{f} with random input values
#'   (that fulfill the \code{init_rest} restrictions).
#'   Per default, \code{f_checks = 10}.
#' * \code{f_check_time}, the maximum number of seconds for a single check for
#'   \code{f}.
#'   A check is considered to be successful, if no error occurred
#'   within \code{f_check_time} seconds.
#'   Per default, \code{f_check_time = 1}.
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal

validate_ino <- function(x = new_ino(), test_par = list()) {
  stopifnot(inherits(x, "ino"))
  test_par <- as.list(test_par)
  if (!exists("validate", where = test_par)) {
    test_par[["validate"]] <- TRUE
  }
  if (test_par[["validate"]]) {
    x$prob <- validate_prob(x = x$prob, test_par = test_par)
    x$opts <- validate_opts(x = x$opts)
    x$runs <- validate_runs(x = x$runs)
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom crayon underline

print.ino <- function(x, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  print(x$prob, ...)
  cat("\n")
  cat(crayon::underline("Numerical optimizer\n"))
  print(x$opts, ...)
  cat("\n")
  cat(crayon::underline("Optimization runs\n"))
  print(x$runs, ...)
  cat("\n")
}

#' Number of parameters
#'
#' @description
#' This function extracts the \code{npar} element from an \code{ino} object,
#' which is the length of the parameter vector over which the target function is
#' optimized.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' An integer, the number of parameters.
#'
#' @export
#'
#' @keywords
#' specification

npar <- function(x) {
  check_inputs(x = x)
  as.integer(x$prob$npar)
}

#' Constructor
#'
#' @description
#' This function constructs a \code{prob} object.
#'
#' @details
#' The \code{prob} object specifies the optimization problem.
#'
#' @format
#' A \code{prob} object is a list of six elements:
#' * The \code{f} element is the function to be optimized.
#' * The \code{npar} element is the length of the first argument of \code{f},
#'   i.e. the argument over which \code{f} is optimized.
#' * The \code{global} element is the point where \code{f} obtains its global
#'   optimum. If it is not specified, it is \code{NULL}.
#' * The \code{add} element is a named list, where each element is a list of
#'   one (or more) additional function elements for \code{f}. The names of the
#'   arguments with multiple parameter values are saved as the character vector
#'   \code{mpvs} in the attributes of \code{add}.
#' * The \code{f_name} element is the name of \code{f}.
#' * The \code{f_target} element is the name of the first argument of \code{f}.
#'
#' @param x
#' A list.
#' @param f
#' An object of class \code{function}, the function to be optimized.
#' @param npar
#' The length of the first argument of \code{f}, i.e. the argument over which
#' \code{f} is optimized.
#' @param global
#' Either \code{NULL} (default) or the point where \code{f} obtains its global
#' optimum (i.e., a numeric vector of length \code{npar}).
#' @param add
#' A list of additional and named arguments to be passed to \code{f}.
#' @param mpvs
#' A character vector of the argument names with multiple parameter values.
#' None per default.
#' @param f_name
#' A character, the name of \code{f}.
#' @param f_target
#' A character, the name of the first argument of \code{f}.
#'
#' @return
#' An object of class \code{prob}.
#'
#' @keywords
#' internal

new_prob <- function(
    x = list(), f = function() {}, npar = integer(), global = NULL,
    add = list(), mpvs = character(), f_name = character(),
    f_target = character()
) {
  if (is.numeric(npar)) {
    npar <- as.integer(npar)
  }
  stopifnot(is.list(x))
  stopifnot(is.function(f))
  stopifnot(is.numeric(global) || is.null(global))
  stopifnot(is.list(add))
  stopifnot(is.character(mpvs))
  stopifnot(is.character(f_name))
  stopifnot(is.character(f_target))
  x[["f"]] <- f
  x[["npar"]] <- npar
  for(add_name in names(add)) {
    if(!add_name %in% mpvs) {
      add[[add_name]] <- list(add[[add_name]])
    }
  }
  x[["global"]] <- global
  x[["add"]] <- structure(add, mpvs = mpvs)
  x[["f_name"]] <- f_name
  x[["f_target"]] <- f_target
  structure(x, class = "prob")
}

#' Validator
#'
#' @description
#' This function validates a \code{prob} object.
#'
#' @param x
#' An object of class \code{prob}.
#' @inheritParams validate_ino
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal
#'
#' @importFrom stats runif

validate_prob <- function(x = new_prob(), test_par = list()) {
  stopifnot(inherits(x, "prob"))
  stopifnot(typeof(x) == "list")
  stopifnot(names(x) %in% c("f", "npar", "global", "add", "f_name", "f_target"))
  stopifnot(is.function(x$f))
  stopifnot(is_number(x$npar))
  stopifnot(length(x$npar) == 1)
  stopifnot(x$npar >= 1)
  stopifnot(is.numeric(x$global) || is.null(x$global))
  if (!is.null(x$global)) {
    stopifnot(length(x$global) == x$npar)
  }
  stopifnot(is.list(x$add))
  stopifnot("mpvs" %in% names(attributes(x$add)))
  if (length(x$add) > 0) {
    stopifnot(sapply(x$add, function(x) is.list(x)))
    for(mpv in attr(x$add, "mpvs")) {
      if (is.null(names(x$add[[mpv]]))) {
        names(x$add[[mpv]]) <- paste0(mpv, 1:length(x$add[[mpv]]))
      }
    }
  }
  if (!exists("init_rest", where = test_par)) {
    test_par[["init_rest"]] <- list("lower" = -1, "upper" = 1)
  }
  test_par[["init_rest"]] <- lapply(
    test_par[["init_rest"]], rep_len, length.out = x[["npar"]]
  )
  if (!exists("init_digits", where = test_par)) {
    test_par[["init_digits"]] <- 2
  }
  if (!exists("f_checks", where = test_par)) {
    test_par[["f_checks"]] <- 10
  }
  if (!exists("f_checks_time", where = test_par)) {
    test_par[["f_checks_time"]] <- 1
  }
  for (run in seq_len(test_par[["f_checks"]])) {
    init <- sapply(
      seq_len(x[["npar"]]),
      function(s) {
        round(
          x = stats::runif(
            n = 1, min = test_par[["init_rest"]][["lower"]],
            max = test_par[["init_rest"]][["upper"]]
          ),
          digits = test_par[["init_digits"]]
        )
      }
    )
    add_extract <- lapply(names(x$add), function(name) x$add[[name]][[1]])
    names(add_extract) <- names(x$add)
    f_out <- try_silent_timed(
      expr = do.call(
        what = x[["f"]],
        args = c(
          structure(list(init), names = x[["f_target"]]),
          add_extract
        )
      ),
      secs = test_par[["f_checks_time"]]
    )
    if (is.null(f_out)) {
      ino_warn(
        paste("Function test run", run, "cannot be validated."),
        paste("The test run returned 'NULL'. The evaluation most likely",
          "reached the time limit of", test_par$f_checks_time, "second(s)."),
        "Try to increase 'f_checks_time' in 'test_par' input.",
        paste("Initial values:", paste(init, collapse = " "))
      )
    } else if (inherits(f_out, "fail")) {
      ino_stop(
        paste("Function test run", run, "failed."),
        f_out,
        paste("Initial values:", paste(init, collapse = " "))
      )
    } else {
      if (!(is.numeric(f_out) && length(f_out) == 1)) {
        ino_stop(
          paste("Function output in test run", run, "is not a single numeric."),
          paste("Initial values:", paste(init, collapse = " "))
        )
      }
    }
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal

print.prob <- function(x, ...) {
  cat("f:", x$f_name, "\n")
  cat("npar:", x$npar, "\n")
  mpvs <- attr(x$add, "mpvs")
  if(length(mpvs) > 0) {
    cat(
      "mpvs:",
      paste0(mpvs, " (", sapply(mpvs, function(m) length(x$add[[m]])), ")",
             collapse = ", "),
      "\n"
    )
  }
}

#' Constructor
#'
#' @description
#' This function constructs an \code{opts} object.
#'
#' @details
#' The \code{opts} object specifies the numerical optimizer(s).
#'
#' @format
#' A \code{opts} object is a list, where each element is a \code{optimizer}
#' object. See \code{\link{new_optimizer}} for the documentation of an
#' \code{optimizer} object.
#'
#' @param x
#' A list.
#' @param opt
#' The output of \code{\link{set_optimizer}}, which is an object of class
#' \code{optimizer}.
#' Per default, \code{opt = set_optimizer_nlm()}, which specifies the
#' \code{\link[stats]{nlm}} optimizer.
#' Can also be a (named) list of multiple \code{optimizer} objects.
#'
#' @return
#' An object of class \code{opts}.
#'
#' @keywords
#' internal

new_opts <- function(x = list(), opt = set_optimizer_nlm()) {
  if (inherits(opt, "optimizer")) {
    opt <- list(opt)
  }
  stopifnot(is.list(opt))
  x <- opt
  if (is.null(names(x))) {
    names(x) <- sapply(x, `[[`, "opt_name") #paste0("opt", 1:length(x))
  }
  structure(x, class = "opts")
}

#' Validator
#'
#' @description
#' This function validates an \code{opts} object.
#'
#' @param x
#' An object of class \code{opts}.
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal

validate_opts <- function(x = new_opts()) {
  stopifnot(inherits(x, "opts"))
  stopifnot(typeof(x) == "list")
  stopifnot(sapply(x, function(x) inherits(x, "optimizer")))
  if(length(x) != length(unique(names(x)))){
    ino_stop(
      event = "Optimizer must have unique names.",
      debug = paste("When specifying multiple optimizer via the 'opt' argument",
                    "for 'setup_ino()', give each list element a unique name.")
    )
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal

print.opts <- function(x, ...) {
  for(i in 1:length(x)) {
    cat("'", names(x)[i], "': ", sep = "")
    print(x[[i]])
    cat("\n")
  }
}

#' Update optimizer
#'
#' @description
#' Use this function to update the optimizer functions for an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @inheritParams new_opts
#' @inheritParams setup_ino
#'
#' @return
#' An object of class \code{ino}.
#'
#' @seealso
#' [set_optimizer()] to specify an optimizer.
#'
#' @export
#'
#' @keywords
#' specification

update_opt <- function(x, opt, verbose = getOption("ino_progress")) {
  if (missing(x)) {
    ino_stop("Please specify the ino object.")
  }
  check_inputs(x = x, verbose = verbose)
  x$opts <- new_opts(opt = opt)
  ino_status("Updated optimizer", verbose = verbose)
  if(verbose) print(x$opt)
  return(x)
}

#' Constructor
#'
#' @description
#' This function constructs a \code{runs} object.
#'
#' @details
#' The \code{runs} object contains results of the optimization runs.
#'
#' @format
#' A \code{runs} object is a list. For each recorded optimization run, it has
#' a list that stores
#' * the name of the initialization strategy \code{.strategy},
#' * the optimization time \code{.time} (as a \code{difftime} object),
#' * the function value at the optimum \code{.optimum},
#' * the identifier for the optimizer \code{.optimizer},
#' * identifier for additional parameters for the target function (if any),
#' * the initial parameter vector \code{.init},
#' * the parameter estimate \code{.estimate},
#' * and additional outputs of the optimizer (if any).
#' @param x
#' A list.
#'
#' @return
#' An object of class \code{runs}.
#'
#' @keywords
#' internal

new_runs <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "runs")
}

#' Validator
#'
#' @description
#' This function validates a \code{runs} object.
#'
#' @param x
#' An object of class \code{runs}.
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal

validate_runs <- function(x = new_runs()) {
  stopifnot(inherits(x, "runs"))
  stopifnot(typeof(x) == "list")
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal

print.runs <- function(x, ...) {
  cat("Records:", length(x))
}

#' Number of optimization runs
#'
#' @description
#' This function returns the number of optimization runs saved in an \code{ino}
#' object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' An integer, the number of recorded optimization runs.
#'
#' @export
#'
#' @keywords
#' specification

nruns <- function(x) {
  check_inputs(x = x)
  length(x$runs)
}

#' Number of failed optimization runs
#'
#' @description
#' This function returns the number of failed optimization runs saved in an
#' \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' An integer, the number of failed recorded optimization runs.
#'
#' @export
#'
#' @keywords
#' specification

nfails <- function(x) {
  check_inputs(x = x)
  fails <- sapply(x$runs, function(x) inherits(x[[".fail"]], "fail"))
  structure(
    sum(fails),
    "fails" = which(fails)
  )
}

#' Create grid
#'
#' @description
#' This helper function creates a grid of all parameter combinations and
#' optimizers for an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param initial_values
#' A numeric vector of length \code{npar(x)} with initial values.
#'
#' @return
#' An object of class \code{grid}, which is a \code{list}, where each element
#' contains
#' * the \code{initial_values} vector,
#' * an \code{optimizer} object,
#' * and (optionally) additional parameters to the target function.
#' The \code{grid} object has an attribute \code{overview}, which is a data
#' frame where the identifiers for the optimizers and additional parameters are
#' saved.
#'
#' @keywords
#' internal

grid_ino <- function(x) {
  check_inputs(x = x)
  grid_par <- c(
    grid_par <- list(".optimizer" = names(x$opts)),
    structure(as.list(names(x$prob$add)), "names" = names(x$prob$add))
  )
  for (mpv in attr(x$prob$add, "mpvs")) {
    grid_par[[mpv]] <- names(x$prob$add[[mpv]])
  }
  grid_par <- expand.grid(grid_par, stringsAsFactors = FALSE)
  grid <- structure(list(), "overview" = grid_par)
  for (i in 1:nrow(grid_par)) {
    grid[[i]] <- list(".optimizer" = x$opts[[grid_par[i,".optimizer"]]])
    for (p in names(x$prob$add)) {
      grid[[i]][[p]] <- if (p %in% attr(x$prob$add, "mpvs")) {
        x$prob$add[[p]][[grid_par[i,p]]]
      } else {
        x$prob$add[[p]][[1]]
      }
    }
  }
  structure(grid, class = "grid")
}

#' Number of grid elements
#'
#' @description
#' This function returns the number of grid elements of an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' An integer, the number of grid elements.
#'
#' @export
#'
#' @keywords
#' specification

ngrid <- function(x) {
  check_inputs(x = x)
  length(grid_ino(x))
}

#' @exportS3Method
#' @noRd
#' @keywords internal

print.grid <- function(x, ...) {
  print(attr(x, "overview"))
}

#' Clear records
#'
#' @description
#' This function clears initialization records saved in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param which
#' Either
#' - \code{"all"} to clear all records,
#' - or \code{"fails"} to clear all failed records,
#' - or a numeric vector of row numbers from \code{summary(x)}.
#'
#' @return
#' The updated input \code{x}.
#'
#' @export
#'
#' @keywords
#' specification

clear_ino <- function(x, which) {
  if (missing(x)) {
    ino_stop(
      event = "Argument 'x' is not specified."
    )
  }
  if (missing(which)) {
    ino_stop(
      event = "Argument 'which' is not specified.",
      debug = "Either 'all' or a numeric vector of row indices of 'summary(x)'."
    )
  }
  if (identical(which, "all")) {
    x[["runs"]] <- new_runs()
  } else {
    if (identical(which, "fails")) {
      which <- attr(nfails(x), "fails")
    }
    if (!is.numeric(which) || any(which < 0)) {
      ino_stop(
        "Argument 'which' is misspecified.",
        "'all', 'fails', or a numeric vector of row indices of 'summary(x)'."
      )
    } else {
      x[["runs"]] <- new_runs(x[["runs"]][-which, drop = FALSE])
    }
  }
  return(x)
}

#' Check inputs
#'
#' @description
#' This helper function centralizes several input checks.
#'
#' @param ...
#' Named inputs to be checked.
#'
#' @return
#' No return value, called for side effects.
#'
#' @keywords
#' internal

check_inputs <- function(...) {
  x <- runs <- sampler <- ncores <- verbose <- at <- arg <- by_col <- NULL
  center <- ind_ign <- initialization <- how <- prop <- by_row <- NULL
  sampler <- function() {}

  inputs <- list(...)
  within(inputs, {
    n <- names(inputs)
    if ("x" %in% n) {
      if (!inherits(x, "ino")) {
        ino_stop(
          event = "'x' must be of class 'ino'."
        )
      }
    }
    if ("runs" %in% n) {
      if (length(runs) > 1 || !is_number(runs)) {
        ino_stop(
          event = "'runs' must be a positive number."
        )
      }
    }
    if ("sampler" %in% n) {
      if (!is.function(sampler)) {
        ino_stop(
          event = "'sampler' must be a function."
        )
      }
      sampler_try <- try_silent(sampler())
      if (!is.numeric(sampler_try) || length(sampler_try) != npar(x)) {
        ino_stop(
          event = "'sampler' must return a numeric vector of length 'npar(x)'."
        )
      }
    }
    if ("ncores" %in% n) {
      if(length(ncores) != 1 || !is_number(ncores) ) {
        ino_stop(
          event = "'ncores' must be a positive integer."
        )
      }
    }
    if ("verbose" %in% n) {
      if(length(verbose) != 1 || (!isTRUE(verbose) && !isFALSE(verbose))) {
        ino_stop(
          event = "'verbose' must be either TRUE or FALSE."
        )
      }
    }
    if ("at" %in% n) {
      if (!is.numeric(at) || length(at) != npar(x)) {
        ino_stop(
          event = "'at' must be a numeric vector of length 'npar(x)'."
        )
      }
    }
    if ("arg" %in% n) {
      if (!is.character(arg) || length(arg) != 1) {
        ino_stop(
          event = "'arg' must be a character."
        )
      }
      if (!arg %in% names(x$prob$add)) {
        ino_stop(
          event = paste0(
            "'", arg, "' is not an argument of '", x$prob$f_name, "'."
          )
        )
      }
      if (!all(
        sapply(x$prob$add[[arg]], inherits, c("matrix", "data.frame")))
      ) {
        ino_stop(
          event = paste0(
            "Argument '", arg, "' is not a matrix or data frame'."
          )
        )
      }
    }
    if ("by_col" %in% n) {
      if (length(by_col) != 1 || (!isFALSE(by_col) && !isTRUE(by_col))) {
        ino_stop(
          event = "'by_col' must be either TRUE or FALSE."
        )
      }
    }
    if ("center" %in% n) {
      if (length(center) != 1 || (!isFALSE(center) && !isTRUE(center))) {
        ino_stop(
          event = "'center' must be either TRUE or FALSE."
        )
      }
    }
    if ("scale" %in% n) {
      if (length(scale) != 1 || (!isFALSE(scale) && !isTRUE(scale))) {
        ino_stop(
          event = "'scale' must be either TRUE or FALSE."
        )
      }
    }
    if ("ind_ign" %in% n) {
      if (!is.numeric(ind_ign)) {
        ino_stop(
          event = "'ind_ign' must be a numeric vector."
        )
      }
    }
    if ("initialization" %in% n) {
      if (!inherits(initialization, "strategy_call")) {
        ino_stop(
          event = "'initialization' must be of class 'strategy_call'."
        )
      }
    }
    if ("how" %in% n) {
      if (!how %in% c("random", "first", "kmeans")) {
        ino_stop(
          event = "'how' must be one of 'random', 'first', or 'kmeans'."
        )
      }
    }
    if ("prop" %in% n) {
      if (!(is.numeric(prop) && all(prop <= 1) && all(prop >= 0))) {
        ino_stop(
          event = "'prop' must be between 0 and 1."
        )
      }
    }
    if ("by_row" %in% n) {
      if (!(is.logical(by_row) || length(by_row) == 1)) {
        ino_stop(
          event = "'by_row' must be either 'TRUE' or 'FALSE'."
        )
      }
    }
  })
  invisible()
}
