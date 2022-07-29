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
#' You can specify multiple values for each \code{...} parameter. Such arguments
#' must be in \code{list} format, where each list element must be a valid
#' parameter value. The names of the \code{...} parameters with multiple values
#' must be added to the \code{mpvs} input to make clear that you want to iterate
#' over them.
#'
#' ### Specifying an optimizer
#' The numerical optimizer must be specified via the \code{opt} argument as the
#' output of \code{\link{set_optimizer}}. You can specify multiple optimizer for
#' comparison by passing a list of optimizers to \code{opt}.
#'
#' ### An example
#' Let \code{nll} be a negative log-likelihood function.
#' Its first argument is a numeric vector of length \code{5}.
#' The function has the additional argument \code{data}.
#' Say that you want to conduct an experiment of the initialization effect for
#' \code{nll} for two different data sets.
#' And say that you want to compare the \code{\link[stats]{nlm}} and the
#' \code{\link[stats]{optim}} optimizer.
#' Then, specify
#' \preformatted{
#' setup_ino(
#'   f = nll,
#'   npar = 5,
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
    f, npar, ..., mpvs = character(), opt = set_optimizer_nlm(),
    test_par = list(
      validate = TRUE,
      init_rest = list("lower" = -1, "upper" = 1),
      init_digits = 2,
      f_checks = 10,
      f_checks_time = 1,
      opt_checks = 10,
      opt_checks_time = 1
    )
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
  validate_ino(
    x = new_ino(
      f = f, npar = par, add = list(...), mpvs = mpvs,
      f_name = deparse(substitute(f)), f_target = names(formals(f))[1],
      opt = opt
    ),
    test_par = test_par
  )
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
#'   It defines the optimizer and is documented in \code{\link{new_opti}}.
#' * The \code{runs} element is an object of class \code{runs}.
#'   It is the storage space for the optimization results and documented in
#'   \code{\link{new_runs}}.
#'
#' @param x
#' A list.
#' @inheritParams new_prob
#' @inheritParams new_opti
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

new_ino <- function(
    x = list(), f = function() {}, npar = integer(), add = list(),
    mpvs = character(), f_name = character(), f_target = character(),
    opt = list()
) {
  stopifnot(is.list(x))
  stopifnot(is.function(f))
  if(is.numeric(npar)) {
    npar <- as.integer(npar)
  }
  stopifnot(is_number(npar))
  stopifnot(is.list(add))
  stopifnot(is.character(mpvs))
  stopifnot(is.character(f_name))
  stopifnot(is.character(f_target))
  stopifnot(is.list(opt))
  x[["prob"]] <- new_prob(
    f = f, npar = npar, add = add, mpvs = mpvs, f_name = f_name,
    f_target = f_target)
  x[["opts"]] <- new_opti(opt = opt)
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
#' * \code{opt_checks}, the number of checks for \code{opt} with random initial
#'   values (that fulfill the \code{init_rest} restrictions).
#'   Per default, \code{opt_checks = 10}.
#' * \code{opt_check_time}, the maximum number of seconds for a single check for
#'   \code{opt}.
#'   A check is considered to be successful, if no error occurred
#'   within \code{opt_check_time} seconds.
#'
#' @return
#' The validated input \code{x}.
#'
#' @keywords
#' internal

validate_ino <- function(x = new_ino(), test_par = list()) {
  stopifnot(inherits(x, "ino"))
  stopifnot(is.list(test_par))
  if (!exists("validate", where = test_par)) {
    test_par[["validate"]] <- TRUE
  }
  if (test_par[["validate"]]) {
    if (!exists("init_rest", where = test_par)) {
      test_par[["init_rest"]] <- list("lower" = -1, "upper" = 1)
    }
    test_par[["init_rest"]] <- lapply(
      test_par[["init_rest"]], rep_len, length.out = test_par[["npar"]]
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
    if (!exists("opt_checks", where = test_par)) {
      test_par[["opt_checks"]] <- 10
    }
    if (!exists("opt_checks_time", where = test_par)) {
      test_par[["opt_checks_time"]] <- 1
    }
    x$prob <- validate_prob(x = x$prob, test_par = test_par)
    x$opts <- validate_opti(x = x$opts, test_par = test_par)
    x$runs <- validate_runs(x = x$runs)
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom crayon underline

print.ino <- function(x, show_args = FALSE, ...) {
  cat(crayon::underline("Function to be optimized\n"))
  print(x$prob, show_args = show_args, ...)
  cat("\n")
  cat(crayon::underline("Numerical optimizer\n"))
  print(x$opts, show_args = show_args, ...)
  cat("\n")
  cat(crayon::underline("Optimization runs\n"))
  print(x$runs, show_args = show_args, ...)
  cat("\n")
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
#' A \code{prob} object is a list of five elements:
#' * The \code{f} element is the function to be optimized.
#' * The \code{npar} element is the length of the first argument of \code{f},
#'   i.e. the argument over which \code{f} is optimized.
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
    x = list(), f = function() {}, npar = integer(), add = list(),
    mpvs = character(), f_name = character(), f_target = character()
) {
  if (is.numeric(npar)) {
    npar <- as.integer(npar)
  }
  stopifnot(is.list(x))
  stopifnot(is.function(f))
  stopifnot(is_number(npar))
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

validate_prob <- function(x = new_prob(), test_par = list()) {
  stopifnot(inherits(x, "prob"))
  stopifnot(typeof(x) == "list")
  ingr_x <- c("f", "npar", "add", "f_name", "f_target")
  stopifnot(ingr_x %in% names(x))
  stopifnot(names(x) %in% ingr_x)
  stopifnot(is.function(x$f))
  stopifnot(is_number(x$npar))
  stopifnot(length(x$npar) == 1)
  stopifnot(x$npar >= 1)
  stopifnot(is.list(x$add))
  stopifnot("mpvs" %in% names(attributes(x$add)))
  if (length(x$add) > 0) {
    stopifnot(sapply(x$add, function(x) is.list(x)))
  }
  for (run in seq_len(test_par[["f_checks"]])) {
    init <- sapply(
      X = seq_len(x[["npar"]]),
      FUN = function(s) {
        round(
          x = runif(
            n = 1, min = test_par[["init_rest"]][["lower"]],
            max = test_par[["init_rest"]][["upper"]]
          ),
          digits = test_par[["init_digits"]]
        )
      }
    )
    ### TODO: extract elements from add lists
    f_out <- try_silent_timed(
      expr = do.call(
        what = x[["f"]],
        args = c(
          structure(list(init), names = x[["f_target"]]),
          x[["add"]]
        )
      ),
      secs = test_par[["f_checks_time"]]
    )
    if (is.null(f_out)) {
      ino_warn(
        event = paste(
          "Function test run", run, "cannot be validated."
        ),
        debug = paste(
          "Initial values:", paste(init, collapse = " "), "\n",
          "The test run returned NULL. The evaluation most likely reached",
          "the time limit. Try to increase 'f_checks_time'."
        ),
        immediate. = TRUE
      )
    } else if (inherits(f_out, "fail")) {
      ino_stop(
        event = paste(
          "Function test run", run, "failed."
        ),
        debug = paste(
          opt_out, "\nInitial values:", paste(init, collapse = " ")
        )
      )
    } else {
      if (!(is.numeric(f_out) && length(f_out) == 1)) {
        ino_stop(
          event = "Function output is not a single numeric."
        )
      }
    }
  }
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom crayon underline

print.prob <- function(x, show_args = FALSE, ...) {
  cat("missing")
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
#' Can also be a list of multiple \code{optimizer} objects.
#'
#' @return
#' An object of class \code{prob}.
#'
#' @keywords
#' internal

new_opti <- function(x = list(), opt = set_optimizer_nlm()) {
  if (inherits(opt, "optimizer")) {
    opt <- list(opt)
  }
  stopifnot(is.list(opt))
  x <- opt
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

validate_opti <- function(x = new_opti()) {
  stopifnot(inherits(x, "opts"))
  stopifnot(typeof(x) == "list")
  stopifnot(sapply(x, function(x) inherits(x, "optimizer")))
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom crayon underline

print.opts <- function(x, show_args = FALSE, ...) {
  cat("missing")
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
#' A \code{runs} object is a list of two elements:
#' * The \code{table} element is a data frame. It has a row for each
#'   recorded optimization run, columns contain optimization results. It stores
#'   only single-value results, e.g. the optimal function value and the
#'   optimization time.
#' * The \code{pars} element is a list. It has an element for each recorded
#'   optimization run. It stores all multi-value optimization results, e.g.
#'   the initial parameter vector and the optimal parameter vector.
#'
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
  x[["table"]] <- data.frame()
  x[["pars"]] <- list()
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
  stopifnot(c("table", "pars") %in% names(x))
  stopifnot(names(x) %in% c("table", "pars"))
  stopifnot(is.data.frame(x$table))
  stopifnot(is.list(x$pars))
  stopifnot(nrow(x$table) == length(x$pars))
  return(x)
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom crayon underline

print.runs <- function(x, ...) {
  cat("Records:", nrow(x$table))
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
#' A \code{list}, where each element is a parameter set. Each parameter set
#' contains at least a placeholder for the target parameter, which is set to
#' \code{NA} and has to be filled by the initialization strategies.
#' Additionally, each parameter set contains further arguments for the target
#' function if available. In this case, the parameter names and identifier for
#' the parameter values are added as attributes \code{"par_name"} and
#' \code{"par_id"} to the parameter set.
#'
#' @keywords
#' internal

grid_ino <- function(x) {
  grid_par <- as.list(names(x$prob$add))
  names(grid_par) <- names(x$prob$add)
  for (mpv in attr(x$prob$add, "mpvs")) {
    grid_par[[mpv]] <- names(x$prob$add[[mpv]])
  }
  grid_par <- expand.grid(grid_par, stringsAsFactors = FALSE)
  grid <- list()
  for (i in 1:max(1, nrow(grid_par))) {
    par_set <- structure(list(NA), names = x$prob$f_target)
    par_set <- c(target, x$prob$add)
    for (p in colnames(grid_par)) {
      par_set[p] <- par_set[[p]][grid_par[i, p]]
    }
    attr(par_set, "par_id") <- as.character(grid_par[i, ])
    grid[[i]] <- par_set
  }
  return(grid)
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

#' Save results of optimization run
#'
#' @description
#' This helper function saves the results of an optimization run into the
#' submitted \code{ino} object.
#'
#' @details
#' The results are saved at \code{x$runs}, which is a list of two elements:
#' \itemize{
#'   \item \code{table} is a \code{data.frame} with the optimization runs as
#'         rows and optimization results as columns.
#'         Per default, the following columns are created:
#'         \itemize{
#'           \item \code{.strategy}, the name of the initialization strategy,
#'           \item \code{.optimizer}, the name of the optimizer,
#'           \item \code{.time}, the optimization time,
#'           \item \code{.optimum}, the function value at the optimum.
#'         }
#'   \item \code{pars} is a \code{list}, where the following values are saved
#'         for each optimization run:
#'         \itemize{
#'           \item \code{.init}, the initial value,
#'           \item \code{.estimate}, the optimal parameter values,
#'           \item and any non-single valued output of the optimizer that was
#'                 specified via \code{crit} in \code{\link{set_optimizer}}.
#'         }
#' }
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
#' The name of the optimizer.
#'
#' @return
#' The updated object \code{x} (invisibly).
#'
#' @keywords
#' internal

result_ino <- function(x, strategy, pars, result, opt_name) {

  ### determine number of new optimization result
  nopt <- nrow(x[["runs"]][["table"]]) + 1

  ### save optimization results
  x[["runs"]][["table"]][nopt, ".strategy"] <- strategy
  x[["runs"]][["table"]][nopt, ".time"] <- result$time
  v <- x$opt[[opt_name]]$base_arg_names[3]
  x[["runs"]][["table"]][nopt, ".optimum"] <- result$res[[v]]
  if (length(x$opt) > 1) {
    x[["runs"]][["table"]][nopt, ".optimizer"] <- opt_name
  }
  for (i in seq_along(attr(pars, "par_name"))) {
    if (attr(pars, "par_name")[i] %in% x$f$mpvs) {
      x[["runs"]][["table"]][nopt, attr(pars, "par_name")[i]] <-
        attr(pars, "par_id")[i]
    }
  }
  x[["runs"]][["pars"]][[nopt]] <- list()
  x[["runs"]][["pars"]][[nopt]][[".init"]] <- pars[[x$f$target_arg]]
  z <- x$opt[[opt_name]]$base_arg_names[4]
  x[["runs"]][["pars"]][[nopt]][[".estimate"]] <- result$res[[z]]
  opt_crit <- x$opt[[opt_name]]$crit
  crit_val <- result$res[opt_crit]
  for (i in seq_along(opt_crit)) {
    if (is.numeric(crit_val[[i]]) && length(crit_val[[i]]) == 1) {
      x[["runs"]][["table"]][nopt, opt_crit[i]] <- crit_val[[i]]
    } else {
      x[["runs"]][["pars"]][[nopt]][[opt_crit[i]]] <- crit_val[[i]]
    }
  }

  ### return (invisibly) updated ino object
  return(invisible(x))
}

