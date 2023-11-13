#' Nop Object
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.

### Arguments for filtering

#' @param which_run
#' Selects results of optimization runs. Either:
#' - \code{"all"} for all results,
#' - \code{"last"} for the results of the last optimizations,
#' - \code{"success"} for the results of successful optimizations,
#' - \code{"failed"} for the results from failed optimizations,
#' - \code{"comparable"} for the results of comparable optimizations (i.e.,
#'   results obtained for the original optimization problem without any
#'   transformations),
#' - \code{"incomparable"} for the results from incomparable optimizations,
#' - a \code{character} (vector) of optimization labels,
#' - an \code{integer} (vector) of run ids as provided, e.g., by \code{$best()}.
#' @param which_direction
#' Selects the type of optimization. Either:
#' - \code{"min"} for minimization,
#' - \code{"max"} for maximization.
#' @param which_optimizer
#' Selects numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels,
#' - an \code{integer} (vector) of optimizer ids as defined in the
#'   \code{$print()} output.
#' @param which_element
#' Selects elements of optimization results. Either:
#' - \code{"all"} for all available elements,
#' - a \code{character} (vector) with names of specific elements (see
#'   \code{$elements()} for the names of available elements per optimizer),
#'   where the following elements are always available:
#'   - \code{"value"}, the function value at the optimum,
#'   - \code{"parameter"}, the parameter at which the optimum value is obtained,
#'   - \code{"seconds"}, the optimization time in seconds,
#'   - \code{"initial"}, the initial parameters,
#'   - \code{"error"}, indicating whether an error occurred,
#'   - \code{"error_message"}, the error message (if any).
#' @param add_identifier
#' Selects identifiers to be added to the output. Can be one or more of:
#' - \code{".run_id"}, which yields an \code{integer} that identifies the
#'   optimization runs,
#' - \code{".optimization_label"}, which yields a \code{character} that
#'   identifies custom groups of optimization runs,
#' - \code{".optimizer_id"}, which yields an \code{integer} that identifies the
#'   optimizer,
#' - \code{".optimizer_label"}, which yields a \code{character} that identifies
#'   the optimizer,
#' - \code{".comparable"}, which yields a \code{logical} that identifies whether
#'   the result is comparable,
#' - \code{".direction"}, which yields \code{"min"} or \code{"max"} that
#'   identifies whether the result corresponds to minimization or maximization.
#' @param group_by
#' Selects how the output is grouped. Either:
#' - \code{NULL} to not group (default),
#' - \code{".optimization_label"} to group by optimization label,
#' - \code{".optimizer_label"} to group by optimizer label.

### Arguments for convenience

#' @param seed
#' Passed on to \code{\link{set.seed}} for reproducibility.
#' Can be \code{NULL} (default) for no seed or to reset the current seed.
#' @param hide_warnings
#' Either \code{TRUE} or \code{FALSE} to hide (show) warning messages
#' during the function evaluation or optimization.
#' @param seconds
#' A \code{numeric}, a time limit in seconds. Optimization is interrupted
#' prematurely if \code{seconds} is exceeded.
#' This currently only works reliably under Windows OS.
#' No time limit if \code{seconds = Inf} (the default).

#' @return
#' Either the \code{Nop} object to allow for method chaining, or a result.
#' Please refer to the respective documentation for the output of the different
#' methods.
#'
#' @details
#' # Getting started
#'
#' ## Step 1: Create a \code{Nop} object
#' Call \code{object <- Nop$new(f, npar, ...)} where
#' - \code{f} is a single-valued function to be optimized with respect to its
#'   first argument,
#' - \code{npar} is the length of the first argument of \code{f},
#' - and \code{...} are additional arguments for \code{f} (if any).
#'   The additional arguments can be managed via the \code{$argument()} method.
#'
#' ## Step 2: Specify numerical optimizers
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}, which can
#' be created with the \code{\link[optimizeR]{define_optimizer}} function.
#' Two \code{optimizer} objects are already available:
#' - \code{\link[optimizeR]{optimizer_nlm}}
#' - \code{\link[optimizeR]{optimizer_optim}}
#'
#' ## Step 3: Select initial values
#' Call one of the following methods to define starting values for the
#' optimization (the different initialization strategies are illustrated in the
#' package vignettes):
#' - \code{object$initialize_fixed()} for fixed initial values,
#' - \code{object$initialize_random()} for random initial values,
#' - \code{object$initialize_continue()} for initial values based on parameter
#'   estimates from previous optimization runs.
#'
#' ## Step 4: Optimization
#' Call \code{object$optimize()} for the optimization. See below for methods
#' to access and analyze the results.
#'
#' # Methods for accessing and analyzing the results
#' - \code{$results()} returns a \code{list} of the optimization results,
#' - \code{$summary()} summarizes the results in a \code{data.frame},
#' - \code{$optima()} returns a frequency table of the identified optima,
#' - \code{$plot()} visualizes the optimization times or values,
#' - \code{$best()} returns the best found parameter vector or function value.
#'
#' # Other methods and fields
#' - \code{object$validate()} checks the configurations of a \code{Nop} object,
#' - \code{object$evaluate()} evaluates the target function at some point,
#' - \code{$delete()} deletes optimization results,
#' - \code{$elements()} returns the names of the available elements in the
#'   optimizer outputs,
#' - \code{$runs()} returns the number of performed optimization runs,
#' - \code{$trace()} calculates the trace of an optimization path,
#' - \code{$initialize_reset()} resets the initial values,
#' - \code{$true()} stores the true best function value and parameter vector
#'   (if available),
#' - \code{$f_name} stores the function name,
#' - \code{$npar} stores the length of the target argument,
#' - \code{$fresh_label} generates a new label for the optimization,
#' - \code{$reserved_labels} returns the names of reserved labels.
#'
#' # Progress bar during optimization
#' Displaying progress during multiple optimization runs via the
#' \code{{progressr}} package is supported. To get started, run
#' \preformatted{
#' progressr::handlers(global = TRUE)
#' }
#' and see \code{\link[progressr]{handlers}} for details.
#'
#' # Parallel optimization
#' Parallel computation of multiple optimization runs via the \code{{future}}
#' package is supported. To get started, run one of
#' \preformatted{
#' future::plan(future::sequential)
#' future::plan(future::multisession)
#' future::plan(future::multicore)
#' }
#' and see \code{\link[future]{plan}} for details.
#'
#' @examples
#' # Minimization of the Ackley function
#' ackley <- TestFunctions::TF_ackley
#'
#' Nop_ackley <- Nop$new(f = ackley, npar = 2)$  # define the Nop object
#'   set_optimizer(
#'     optimizeR:Optimizer("stats::nlm")         # select the nlm optimizer
#'   )$
#'   initialize_random(
#'     sampler = function() rnorm(2, mean = 0, sd = 3),
#'     runs = 100, seed = 1
#'   )$                                          # initialize 100 times randomly
#'   optimize(which_direction = "min")           # minimize
#'
#' Nop_ackley$optima(print.rows = 5, digits = 1) # get optima overview
#'
#' Nop_ackley$best("parameter")                  # return best parameter vector
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Creates a new \code{Nop} object.
    #' @param objective
    #' The \code{function} to be optimized (the so-called objective function).
    #' It should return a single \code{numeric} value.
    #' @param target
    #' The names of the arguments over which \code{f} will be optimized (the
    #' so-called target arguments).
    #' Each of these arguments should expect a \code{numeric} \code{vector}.
    #' @param npar
    #' The lengths of the target arguments.
    #' @param ...
    #' Optionally additional, named arguments for \code{f} which are kept fixed
    #' during the optimization.
    #' @return
    #' A new \code{Nop} object.

    initialize = function(objective, target, npar, ...) {
      if (missing(objective)) {
        cli::cli_abort(
          c(
            "Please specify argument {.var objective}.",
            "i" = "It should be the {.cls function} to be optimized."
          ),
          call = NULL
        )
      }
      checkmate::assert_function(objective)
      if (is.null(formals(objective))) {
        cli::cli_abort(
          c(
            "Function {.var objective} should have at least one argument.",
            "i" = "The function needs an argument over which it is optimized."
          ),
          call = NULL
        )
      }
      if (missing(target)) {
        cli::cli_abort(
          c(
            "Please specify argument {.var target}.",
            "i" = "It should be the names of the target arguments."
          ),
          call = NULL
        )
      }
      checkmate::assert_character(target, any.missing = FALSE, min.len = 1)
      if (!checkmate::test_subset(target, names(formals(objective)))) {
        bad_args <- setdiff(target, formals(objective))
        cli::cli_abort(
          c(
            "The argument {.var target} is incorrectly stated.",
            "i" = "The objective does not have argument{?s} {.val {bad_args}}."
          ),
          call = NULL
        )
      }
      if (missing(npar)) {
        cli::cli_abort(
          c(
            "Please specify argument {.var npar}.",
            "i" = "It should be the lengths of the target arguments."
          ),
          call = NULL
        )
      }
      checkmate::assert_integerish(
        npar, lower = 1, any.missing = FALSE, len = length(target)
      )
      private$objective <- optimizeR::Objective$new(
        objective = objective,
        target = target,
        npar = npar,
        ...
      )
      ### TODO: do I need this?
      private$objective$objective_name <- deparse(substitute(objective))
    },

    #' @description
    #' Prints details of the numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.
    print = function(digits = getOption("digits", default = 7), ... ) {

      ### info on optimization problem
      cli::cli_h2("Optimization problem")
      cli::cli_bullets(c(
        "*" = "Function: {self$f_name}",
        "*" = "Optimize over: {private$.f_target} (length {self$npar})"
      ))
      tv_min <- private$.true_value_min
      if (!is.null(tv_min)) {
        tv_min <- toString(round(tv_min, digits = digits))
        cli::cli_bullets(c(
          "*" = "True minimum value: {tv_min}"
        ))
      }
      tp_min <- private$.true_parameter_min
      if (!is.null(tp_min)) {
        tp_min <- toString(round(tp_min, digits = digits))
        cli::cli_bullets(c(
          "*" = "True minimum at: {tp_min}"
        ))
      }
      tv_max <- private$.true_value_max
      if (!is.null(tv_max)) {
        tv_max <- toString(round(tv_max, digits = digits))
        cli::cli_bullets(c(
          "*" = "True maximum value: {tv_max}"
        ))
      }
      tp_max <- private$.true_parameter_max
      if (!is.null(tp_max)) {
        tp_max <- toString(round(tp_max, digits = digits))
        cli::cli_bullets(c(
          "*" = "True maximum at: {tp_max}"
        ))
      }

      ### info on additional arguments
      argument_names <- names(private$.arguments)
      if (length(argument_names) > 0) {
        modified <- argument_names %in% private$.original_arguments
        if (any(modified)) {
          names(argument_names) <- ifelse(modified, "!", " ")
        }
        cli::cli_h2("Additional function arguments")
        cli::cli_bullets(argument_names)
        if (any(modified)) {
          cat(cli::style_italic("Some arguments are currently modified.\n"))
        }
      }

      ### info on optimizer functions
      cli::cli_h2("Optimizer functions")
      optimizer <- private$.optimizer
      if (length(optimizer) == 0) {
        cat(cli::style_italic("No optimizer specified.\n\n"))
      } else {
        cli::cli_ol(names(optimizer))
      }

      ### info on initial values
      cli::cli_h2("Initial values")
      initial_values <- private$.initial_values
      if (length(initial_values) == 0) {
        cat(cli::style_italic("No initial values specified.\n\n"))
      } else {
        initial_types <- table(private$.initial_type)
        cli::cli_bullets(
          structure(
            paste0(initial_types, "x ", names(initial_types)),
            names = rep("*", length(initial_types))
          )
        )
      }

      ### info on optimization runs
      cli::cli_h2("Optimization runs")
      noptimizations <- suppressWarnings(self$runs(verbose = FALSE))
      if (noptimizations == 0) {
        cat(cli::style_italic("No results.\n\n"))
      } else {
        suppressWarnings({
          noptimizations <- self$runs(verbose = FALSE)
          ncomparable <- self$runs(which_run = "comparable", verbose = FALSE)
          nincomparable <- self$runs(which_run = "incomparable", verbose = FALSE)
          nfailed <- self$runs(which_run = "failed", verbose = FALSE)
          nsuccess <- self$runs(which_run = "success", verbose = FALSE)
        })
        cli::cli_bullets(c(
          "*" = "Total runs: {noptimizations}"
        ))
        if (nincomparable > 0) {
          cli::cli_bullets(c(
            "*" = "Incomparable runs: {nincomparable}"
          ))
        }
        if (nfailed > 0) {
          cli::cli_bullets(c(
            "*" = "Failed runs: {nfailed}"
          ))
        }
        if (nsuccess > 0) {

          ### info on optimization results
          cli::cli_h2("Optimization results")
          suppressWarnings({
            best_parameter_min <- self$best(
              which_element = "parameter", which_direction = "min",
              digits = digits, verbose = FALSE
            )
            best_value_min <- self$best(
              which_element = "value", which_direction = "min",
              digits = digits, verbose = FALSE
            )
            best_parameter_max <- self$best(
              which_element = "parameter", which_direction = "max",
              digits = digits, verbose = FALSE
            )
            best_value_max <- self$best(
              which_element = "value", which_direction = "max",
              digits = digits, verbose = FALSE
            )
          })
          if (!is.null(best_parameter_min) && !is.null(best_value_min)) {
            cli::cli_bullets(c(
              "*" = "Minimum parameter vector: {toString(best_parameter_min)}",
              "*" = "Minimum function value: {best_value_min}"
            ))
          }
          if (!is.null(best_parameter_max) && !is.null(best_value_max)) {
            cli::cli_bullets(c(
              "*" = "Maximum parameter vector: {toString(best_parameter_max)}",
              "*" = "Maximum function value: {best_value_max}"
            ))
          }
        }
      }
      invisible(self)
    },

    #' @description
    #' Manages additional (i.e., in addition to the target argument) arguments
    #' for \code{f}.
    #' @details
    #' Using \code{how = "similar"} or \code{how = "dissimilar"} for subsetting
    #' applies k-means clustering via \code{\link[stats]{kmeans}} and requires
    #' that the selected argument is \code{numeric}
    #' @param action
    #' One of:
    #' - \code{"set"} to set an argument,
    #' - \code{"get"} to extract an argument value,
    #' - \code{"remove"} to remove an argument,
    #' - \code{"reset"} to reset an argument to the original value,
    #' - \code{"modify"} to modify an argument to a new value (the original
    #'   value is saved and can be recovered via \code{"reset"}),
    #' - \code{"subset"} to subset an argument,
    #' - \code{"standardize"} to standardize a \code{numeric} argument.
    #' @param ...
    #' Additional parameters depending on the \code{action}:
    #' - if \code{action = "set"} or \code{"modify"}, one or more named
    #'   arguments,
    #' - if \code{action = "get"}, \code{"remove"}, or \code{"reset"},
    #'   the argument \code{name},
    #' - if \code{action = "subset"},
    #'   - the argument \code{name},
    #'   - \code{byrow}, either \code{TRUE} to subset
    #'     row-wise (default) or \code{FALSE} to subset column-wise,
    #'   - \code{how}, specifying how to subset, either \code{"random"}
    #'     (default), \code{"first"}, \code{"last"}, \code{"similar"}, or
    #'     \code{"dissimilar"}.
    #'   - \code{proportion}, a \code{numeric} between \code{0} and \code{1},
    #'     specifying the subset proportion (the default is \code{0.5}),
    #'   - \code{centers}, passed on to \code{\link[stats]{kmeans}}
    #'     if \code{how = "(dis)similar"} (by default, \code{centers = 2}),
    #'   - \code{ignore}, an \code{integer} vector of row indices (or column
    #'     indices if \code{byrow = FALSE}) to ignore for clustering if
    #'     \code{how = "(dis)similar"},
    #'   - \code{seed}, an \code{integer} for reproducibility,
    #' - if \code{action = "standardize"},
    #'   - the argument \code{name},
    #'   - \code{byrow}, either \code{TRUE} to standardize
    #'     row-wise or \code{FALSE} to standardize column-wise (default),
    #'   - \code{center}, set to \code{TRUE} (default) for centering, resulting
    #'     in zero mean,
    #'   - \code{scale}, set to \code{TRUE} (default) for scaling, resulting in
    #'     unit variance,
    #'   - \code{ignore}, an \code{integer} vector of column indices (or row
    #'     indices if \code{byrow = TRUE}) to not standardize,
    #'   - \code{jointly}, a \code{list} of \code{integer} vectors with column
    #'     indices (or row indices if \code{byrow = TRUE}) to standardize
    #'     jointly.
    #' @return
    #' The argument value if \code{action = "get"} and invisibly the \code{Nop}
    #' object, else. If \code{action = "standardize"}, the \code{numeric}
    #' centering and scaling used (if any) are added as
    #' attributes \code{"standardized:center"} and \code{"standardized:scale"}
    #' to the argument.
    argument = function(
      action, ..., verbose = getOption("ino_verbose", default = FALSE)
    ) {
      if (missing(action)) {
        cli::cli_abort(
          "Please specify argument {.var action}.",
          call = NULL
        )
      }
      checkmate::assert_string(action)
      action <- oeli::match_arg(
        action, c("set", "get", "remove", "reset", "modify")
      )
      checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
      args <- list(...)
      arg_names <- names(args)
      if (action == "set") {
        if (length(args) == 0) {
          cli::cli_warn("No argument to set.")
        } else if (length(args) > 1) {
          for (i in 1:length(args)) {
            arg <- list("set", args[[i]])
            names(arg) <- c("action", arg_names[i])
            do.call(self$argument, arg)
          }
        } else {
          name <- arg_names[1]
          if (!checkmate::test_string(name)) {
            cli::cli_abort(
              "All arguments to be set must be named.",
              call = NULL
            )
          }
          if (name %in% names(private$.arguments)) {
            private$.arguments[name] <- args
            if (verbose) {
              cli::cli_alert_info("Replaced argument {.var {name}}.")
            }
          } else {
            private$.arguments <- c(private$.arguments, args)
            if (verbose) {
              cli::cli_alert_info("Set argument {.var {name}}.")
            }
          }
        }
      }
      if (action == "get") {
        if (!"name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var name}.",
            call = NULL
          )
        }
        name <- args[["name"]]
        checkmate::assert_string(name)
        private$.check_additional_argument_exists(name)
        return(private$.arguments[[name]])
      }
      if (action == "remove") {
        if (!"name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var name}.",
            call = NULL
          )
        }
        name <- args[["name"]]
        checkmate::assert_string(name)
        private$.check_additional_argument_exists(name)
        arg_id <- which(names(private$.arguments) == name)
        private$.arguments[arg_id] <- NULL
        arg_id <- which(names(private$.original_arguments) == name)
        private$.original_arguments[arg_id] <- NULL
        if (verbose) cli::cli_alert_info("Removed argument {.var {name}}.")
      }
      if (action == "reset") {
        if (!"name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var name}.",
            call = NULL
          )
        }
        name <- args[["name"]]
        checkmate::assert_string(name)
        private$.check_additional_argument_exists(name)
        if (!is.null(private$.original_arguments[[name]])) {
          original_argument <- private$.original_arguments[[name]]
          private$.arguments[[name]] <- original_argument
          private$.original_arguments[[name]] <- NULL
          if (verbose) cli::cli_alert_info("Reset `{name}`.")
        } else {
          cli::cli_warn("Nothing to reset.")
        }
      }
      if (action == "modify") {
        if (length(args) == 0) {
          cli::cli_warn("No argument to modify.")
        } else if (length(args) > 1) {
          for (i in 1:length(args)) {
            arg <- list("modify", args[[i]])
            names(arg) <- c("action", arg_names[i])
            do.call(self$argument, arg)
          }
        } else {
          name <- arg_names[1]
          if (!checkmate::test_string(name)) {
            cli::cli_abort(
              "All arguments to be modified must be named.",
              call = NULL
            )
          }
          private$.check_additional_argument_exists(name)
          if (is.null(private$.original_arguments[[name]])) {
            private$.original_arguments[[name]] <- private$.arguments[[name]]
          }
          private$.arguments[[name]] <- args[[name]]
          if (verbose) cli::cli_alert_info("Modified argument {.var {name}}.")
        }
      }
      if (action == "subset") {
        if (!"name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var name}.",
            call = NULL
          )
        }
        name <- args[["name"]]
        checkmate::assert_string(name)
        original_argument <- self$argument("get", name = name)
        byrow <- if ("byrow" %in% arg_names) {
          args[["byrow"]]
        } else {
          TRUE
        }
        how <- if ("how" %in% arg_names) {
          args[["how"]]
        } else {
          "random"
        }
        proportion <- if ("proportion" %in% arg_names) {
          args[["proportion"]]
        } else {
          0.5
        }
        centers <- if ("centers" %in% arg_names) {
          args[["centers"]]
        } else {
          2
        }
        ignore <- if ("ignore" %in% arg_names) {
          args[["ignore"]]
        } else {
          integer()
        }
        if ("seed" %in% arg_names) {
          set.seed(seed = args[["seed"]])
        }
        subsetted_argument <- helper_subset(
          argument = original_argument, byrow = byrow, how = how,
          proportion = proportion, centers = centers, ignore = ignore
        )
        # TODO add message about subset action
        private$.arguments[[name]] <- subsetted_argument
        if (is.null(private$.original_arguments[[name]])) {
          private$.original_arguments[[name]] <- original_argument
        }
      }
      if (action == "standardize") {
        if (!"name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var name}.",
            call = NULL
          )
        }
        name <- args[["name"]]
        checkmate::assert_string(name)
        original_argument <- self$argument("get", name = name)
        byrow <- if ("byrow" %in% arg_names) {
          args[["byrow"]]
        } else {
          FALSE
        }
        center <- if ("center" %in% arg_names) {
          args[["center"]]
        } else {
          TRUE
        }
        scale <- if ("scale" %in% arg_names) {
          args[["scale"]]
        } else {
          TRUE
        }
        ignore <- if ("ignore" %in% arg_names) {
          args[["ignore"]]
        } else {
          integer()
        }
        jointly <- if ("jointly" %in% arg_names) {
          args[["jointly"]]
        } else {
          integer()
        }
        standardized_argument <- helper_standardize(
          argument = original_argument, byrow = byrow, center = center,
          scale = scale, ignore = ignore, jointly = jointly
        )
        private$.arguments[[name]] <- standardized_argument
        if (is.null(private$.original_arguments[[name]])) {
          private$.original_arguments[[name]] <- original_argument
        }
        if (verbose) cli::cli_alert_info("Standardized {.var {name}}.")
      }
      invisible(self)
    },

    #' @description
    #' Specifies a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer}, which can be created via
    #' \code{\link[optimizeR]{Optimizer}}.
    #' @param optimizer_label
    #' A \code{character}, a *unique* label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
    #' @return
    #' Invisibly the \code{Nop} object.
    set_optimizer = function(optimizer, optimizer_label = NULL) {
      if (missing(optimizer)) {
        cli::cli_abort(
          "Please specify argument {.var optimizer}.",
          call = NULL
        )
      }
      if (!inherits(optimizer, "Optimizer")) {
        cli::cli_abort(
          c(
            "Argument {.var optimizer} must be an {.cls Optimizer} object.",
            "i" = "See {.help optimizeR::Optimizer} to create such an object."
          ),
          call = NULL
        )
      }
      if (is.null(optimizer_label)) {
        optimizer_label <- optimizer$label
      }
      checkmate::assert_string(optimizer_label)
      if (optimizer_label %in% names(private$.optimizer)) {
        cli::cli_abort(
          "Label {.val {optimizer_label}} already exists, use another one.",
          call = NULL
        )
      }
      private$.optimizer[[optimizer_label]] <- optimizer
      invisible(self)
    },

    #' @description
    #' Evaluates the objective function.
    #' @param at
    #' A \code{numeric} vector of length \code{sum(npar)}, the point where the
    #' function is evaluated.
    #' By default, \code{at = rnorm(sum(self$npar))}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @return
    #' Either:
    #' - a \code{numeric} value, the function value at \code{at},
    #' - \code{"time limit reached"} if the time limit was reached,
    #' - the error message if the evaluation failed.
    evaluate = function(
      at = stats::rnorm(sum(self$npar)),
      seconds = Inf,
      hide_warnings = FALSE
    ) {
      private$objective$check_target(at, verbose = FALSE)
      private$objective$seconds <- seconds
      private$objective$hide_warnings <- hide_warnings
      private$objective$evaluate(at)
    },

    #' @description
    #' Defines fixed initial values for the optimization.
    #' @param at
    #' A \code{numeric} \code{vector} of length \code{self$sum(npar)}, the
    #' initial parameter vector. It can also be a \code{list} of such vectors.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_fixed = function(at) {
      self$initialize_custom(at, type = "fixed")
    },

    #' @description
    #' Defines random initial values for the optimization.
    #' @param sampler
    #' A \code{function} without any arguments that returns a \code{numeric}
    #' vector of length \code{sum(self$npar)}.
    #' By default, \code{sampler = rnorm(sum(self$npar))}, i.e., random values
    #' drawn from a standard normal distribution.
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' By default, \code{runs = 1}.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_random = function(
      sampler = function() stats::rnorm(sum(self$npar)), runs = 1, seed = NULL
    ) {
      checkmate::assert_function(sampler, nargs = 0)
      checkmate::assert_count(runs, positive = TRUE)
      set.seed(seed)
      at <- list()
      seconds <- numeric(runs)
      for (run in seq_len(runs)) {
        t_start <- Sys.time()
        value <- try(sampler(), silent = TRUE)
        t_end <- Sys.time()
        seconds[run] <- as.numeric(difftime(t_end, t_start, units = "secs"))
        at[[run]] <- value
      }
      self$initialize_custom(at, seconds = seconds, type = "random")
    },

    #' @description
    #' Defines grid initial values for the optimization.
    #' @param lower
    #' A \code{numeric} \code{vector} of length \code{self$sum(npar)}, the
    #' lower grid bounds for each parameter dimension.
    #' @param upper
    #' A \code{numeric} \code{vector} of length \code{self$sum(npar)}, the
    #' upper grid bounds for each parameter dimension.
    #' @param breaks
    #' A \code{numeric} \code{vector} of length \code{self$sum(npar)}, the
    #' number of breaks for each parameter dimension.
    #' @param jitter
    #' Either \code{TRUE} to add noise to the grid points for a random grid
    #' layout, or \code{FALSE} (default), else.
    #' @param ...
    #' Optional parameters passed to \code{\link[base]{jitter}}.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_grid = function(
      lower = rep(0, length(sum(self$npar))),
      upper = rep(1, length(sum(self$npar))),
      breaks = rep(3, length(sum(self$npar))),
      jitter = FALSE, ...
    ) {
      checkmate::assert_numeric(lower, any.missing = FALSE, len = sum(self$npar))
      checkmate::assert_numeric(upper, any.missing = FALSE, len = sum(self$npar))
      if (!all(lower <= upper)) {
        cli::cli_abort(
          "Lower bounds must be smaller than upper bounds.", call = NULL
        )
      }
      checkmate::assert_integerish(breaks, any.missing = FALSE, lower = 1, len = sum(self$npar))
      checkmate::assert_flag(jitter)
      grid_points <- mapply(seq, from = lower, to = upper, len = breaks, SIMPLIFY = FALSE)
      at <- asplit(expand.grid(grid_points), 1)
      if (jitter) {
        at <- lapply(at, jitter, ...)
      }
      self$initialize_custom(at, verbose = FALSE, type = "grid")
    },

    #' @description
    #' Defines initial values based on results from previous optimizations.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_transfer = function(
      which_run = "last", which_optimizer = "all", which_direction = c("min", "max")
    ) {
      out <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = c("parameter", "seconds")
      )
      at <- lapply(out, `[[`, "parameter")
      seconds <- sapply(out, `[[`, "seconds")
      runs <- length(at)
      self$initialize_custom(at = at, seconds = seconds, type = "transferred")
    },

    #' @description
    #' Defines custom initial values for the optimization.
    #' @param at
    #' A \code{list} of \code{numeric} \code{vector}s, each of length
    #' \code{self$sum(npar)}, the initial parameter vector.
    #' @param seconds
    #' A \code{numeric} \code{vector} of the same length as \code{at}. It
    #' contains the number of seconds it took to obtain these initial values,
    #' which is added to the overall optimization time.
    #' @param type
    #' A single \code{character}, the type of the initial values.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_custom = function(at, seconds = rep(0, length(at)), type = "custom") {
      checkmate::assert_list(at)
      runs <- length(at)
      lapply(at, private$objective$check_target, verbose = FALSE)
      checkmate::assert_numeric(seconds, lower = 0, any.missing = FALSE, len = runs)
      private$.initial_values <- c(private$.initial_values, at)
      private$.initial_type <- c(private$.initial_type, rep(type, runs))
      private$.initial_seconds <- c(private$.initial_seconds, seconds)
      if (self$verbose) {
        cli::cli_alert_info("Added {runs} {type} initial parameter value{?s}.")
      }
      invisible(self)
    },

    #' @description
    #' Defines a subset of promising initial values for the optimization.
    #' @param proportion
    #' A \code{numeric} between 0 and 1, the subset proportion.
    #' @param condition
    #' Defines the condition on which the initial values are selected, either
    #' - \code{"gradient_steep"} for the points where the numerical gradient is steepest,
    #' - \code{"value_low"} for the points where the function value is lowest,
    #' - \code{"value_high"} for the points where the function value is highest.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_promising = function(proportion, condition = "gradient_steep") {
      checkmate::assert_number(proportion, lower = 0, upper = 1)
      oeli::match_arg(condition, c("gradient_steep", "value_low", "value_high"))
      runs <- length(private$.initial_values)
      if (runs == 0) {
        if (self$verbose) {
          cli::cli_alert_info("No initial values defined yet.")
        }
      } else {
        runs <- ceiling(runs * proportion)
        if (condition == "gradient_steep") {
          # TODO
          # numDeriv::grad
        } else {
          values <- sapply(private$.initial_values, self$evaluate)
          if (condition == "value_low") {
            # TODO
          } else {
            # TODO
          }
        }
        ### remove seconds and types
        if (self$verbose) {
          cli::cli_alert_info("Selected {runs} initial parameter value{?s}.")
        }
      }
      invisible(self)
    },

    #' @description
    #' Transforms the currently defined initial values.
    #' @param transform
    #' A \code{function} for transforming the initial values. It must be able to
    #' receive and return a \code{numeric} \code{vector} of length
    #' \code{sum(self$npar)}.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_transform = function(transform = function(x) x) {
      runs <- length(private$.initial_values)
      if (runs == 0) {
        if (self$verbose) {
          cli::cli_alert_info("No initial values defined yet.")
        }
      } else {
        private$.initial_values <- lapply(private$.initial_values, transform)
        if (self$verbose) {
          cli::cli_alert_info("Transformed {runs} initial parameter value{?s}.")
        }
      }
      invisible(self)
    },

    #' @description
    #' Resets the currently defined initial values.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_reset = function() {
      runs <- length(private$.initial_values)
      if (runs == 0) {
        if (self$verbose) {
          cli::cli_alert_info("No initial values defined yet.")
        }
      } else {
        private$.initial_values <- list()
        private$.initial_type <- numeric()
        private$.initial_seconds <- numeric()
        if (self$verbose) {
          cli::cli_alert_info("Reset {runs} initial parameter value{?s}.")
        }
      }
      invisible(self)
    },

    #' @description
    #' Optimizes the function.
    #' @details
    #' Parallel computation of multiple optimization runs via the \code{{future}}
    #' package and displaying progress via the \code{{progressr}} package are supported.
    #' @param optimization_label
    #' A \code{character} to specify a label for the optimization.
    #' Labels are useful to distinguish optimization runs.
    #' By default, \code{self$fresh_label} creates a new label.
    #' @param reset_initial
    #' Either \code{TRUE} (default) to reset the initial values after the
    #' optimization, or \code{FALSE}, else.
    #' @return
    #' Invisibly the \code{Nop} object.
    optimize = function(
      optimization_label = self$fresh_label, which_optimizer = "all",
      which_direction = "min", seconds = Inf, hide_warnings = TRUE,
      reset_initial = TRUE
    ) {

      ### input checks
      private$objective$.__enclos_env__$private$.check_arguments_complete(
        verbose = FALSE
      )
      checkmate::assert_string(optimization_label)
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = FALSE
      )
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = TRUE, verbose = FALSE
      )
      checkmate::assert_number(seconds, lower = 0)
      checkmate::assert_flag(hide_warnings)
      checkmate::assert_flag(reset_initial)

      ### initial values available?
      if (length(private$.initial_values) == 0) {
        self$initialize_random(runs = 1)
        cli::cli_warn(
          "No initial values defined, so random initial values are used.",
          "Call {.fun $initialize_*} first to customize the initialization."
        )
      }

      ### build grid of optimization combinations
      combinations <- expand.grid(
        initial = private$.initial_values,
        optimizer_id = optimizer_ids,
        which_direction = which_direction
      )

      ### optimize
      if (self$verbose) {
        cli::cli_alert_info(
          "Optimization with label {.val {optimization_label}}."
        )
      }
      progress_step <- progressr::progressor(steps = nrow(combinations))
      results <- future.apply::future_apply(
        combinations, MARGIN = 1, function(x) {
        progress_step()
        structure(
          private$.optimize(
            initial = x$initial,
            optimizer = private$.optimizer[[x$optimizer_id]],
            which_direction = x$which_direction,
            seconds = seconds,
            hide_warnings = hide_warnings
          ),
          ".optimizer_id" = x$optimizer_id,
          ".optimizer_label" = names(private$.optimizer)[x$optimizer_id],
          ".direction" = x$which_direction
        )
      }, future.seed = TRUE)

      ### save results
      private$.save_results(
        results = results, optimization_label = optimization_label,
        verbose = verbose, comparable = private$.result_comparable()
      )
      if (reset_initial) {
        self$initialize_reset()
      }
      invisible(self)
    },

    #' @description
    #' Validates the configuration of a \code{Nop} object.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}, the point where the
    #' function and the optimizers are tested.
    #' By default, \code{at = rnorm(self$npar)}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @return
    #' Invisibly \code{TRUE} if the tests are successful.
    validate = function(
      at = stats::rnorm(self$npar), which_optimizer = "all",
      which_direction = "min", seconds = 10
    ) {

      ### TODO use cli::progress_step()

      ### input checks
      private$.check_target_argument(at)

      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = verbose
      )
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = FALSE,
        verbose = verbose
      )
      checkmate::assert_number(seconds, null.ok = TRUE, lower = 0)
      checkmate::assert_flag(verbose)
      checkmate::assert_count(digits)

      ###
      if (verbose) {
        cli::cli_alert_info("Check specifications:")
        cli::cli_alert_info("Function specified: {self$f_name}")
        cli::cli_alert_info(
          "Target argument specified: {private$.f_target} (length {self$npar})"
        )
        cli::cli_alert_info(
          "Test initial values specified: {round(at, digits = digits)}"
        )
        cli::cli_alert_info("Try to evaluate the function:")
      }
      out <- self$evaluate(
        at = at, seconds = seconds, hide_warnings = TRUE
      )
      if (is.character(out)) {
        if (identical(out, "time limit reached")) {
          cli::cli_warn(
            "Time limit of {seconds} second{?s} reached in the function call,
            consider increasing {.var seconds}."
          )
        } else {
          cli::cli_abort(
            "Function call threw an error: {out}",
            call = NULL
          )
        }
      } else {
        if (!is.numeric(out)) {
          cli::cli_abort(
            "Test function call did not return a {.cls numeric} value.",
            call = NULL
          )
        } else {
          cli::cli_alert_success(
            "Test function call returned a {.cls numeric}.",
          )
        }
        if (length(out) != 1) {
          cli::cli_abort(
            "Test function call is of length {length(out)}.
            It should be a single {.cls numeric} value.",
            call = NULL
          )
        } else {
          if (verbose) {
            cli::cli_alert_success("Value: {round(out, digits = digits)}")
          }
        }
      }
      if (length(optimizer_ids) == 0) {
        cli::cli_warn(
          "No optimizer specified, testing optimizers is skipped.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
        )
      } else {
        for (i in optimizer_ids) {
          cli::cli_alert_info(
            "Try to optimize with
            `{paste(names(private$.optimizer)[i], collapse = ', ')}`:"
          )
          out <- self$initialize_fixed(at, verbose = verbose)$
            optimize(
              which_optimizer = i, which_direction = which_direction,
              seed = NULL, return_results = TRUE,
              save_results = FALSE, ncores = 1, verbose = FALSE,
              seconds = seconds, hide_warnings = TRUE
            )
          if (!is.null(out$error)) {
            if (isTRUE(out$error)) {
              if (identical(out$error_message, "time limit reached")) {
                cli::cli_warn(
                  "Time limit of {seconds}s reached in the optimization.
                  Consider increasing {.var seconds}."
                )
              } else {
                cli::cli_abort("Optimization threw an error: {out$error_message}")
              }
            }
          } else {
            if (!is.list(out)) {
              cli::cli_abort(
                "Test optimization did not return a {.cls list}."
              )
            } else {
              cli::cli_alert_success(
                "Test optimization returned a {.cls list}."
              )
              for (value in c("value", "parameter", "seconds")) {
                if (!value %in% names(out)) {
                  cli::cli_abort(
                    "Output does not contain the element '{value}'."
                  )
                } else {
                  cli::cli_alert_success(
                    glue::glue(
                      "Optimization {value}: ",
                      "{paste(round(out[[value]], digits = digits), collapse = ' ')}"
                    )
                  )
                  out[[value]] <- NULL
                }
              }
              if (length(out) > 0) {
                cli::cli_alert_info(
                  glue::glue(
                    "Additional elements: ",
                    "{paste(names(out), collapse = ' ')}"
                  )
                )
              }
            }
          }
        }
      }
      invisible(TRUE)
    },

    #' @description
    #' Returns the names of available elements in the optimization results.
    #' @param group_by
    #' TODO
    #' @return
    #' Depending on group_by, ... A \code{list}.
    elements = function(
      which_optimizer = "all", group_by = NULL,
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### check if any results saved yet
      if (self$runs(verbose = FALSE) == 0) {
        return(list())
      }

      ### input checks
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = FALSE
      )
      group_by <- group_by # TODO

      ### get element names
      elements <- list()
      optimizer_labels <- names(private$.optimizer)
      for (optimizer_id in optimizer_ids) {
        optimizer_label <- optimizer_labels[optimizer_id]
        if (
          length(private$.optimizer_id_ids) < optimizer_id ||
          is.null(private$.optimizer_id_ids[[optimizer_id]])
        ) {
          cli::cli_warn(
            "No results for optimizer {.var {optimizer_label}}.",
          )
          elements[[optimizer_label]] <- character()
        } else {
          elements[[optimizer_label]] <- unique(names(unlist(
            private$.results[private$.optimizer_id_ids[[optimizer_id]]],
            recursive = FALSE
          )))
        }
      }

      ###
      if (identical(group_by, ".optimizer_label")) {
        return(elements)
      } else {
        return(unique(unlist(elements, use.names = FALSE)))
      }

    },

    #' @description
    #' Returns the number of optimization results.
    #' @return
    #' An \code{integer}.
    runs = function(
      which_run = "all", which_direction = c("min", "max"),
      which_optimizer = "all", which_element = "all",
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      length(
        private$.get_run_ids(
          which_run = which_run, which_direction = which_direction,
          which_optimizer = which_optimizer, which_element = which_element,
          verbose = verbose
        )
      )
    },

    #' @description
    #' Returns optimization results.
    #' @return
    #' A \code{list}.
    results = function(
      which_run = "all", which_direction = c("min", "max"),
      which_optimizer = "all", which_element = "all",
      add_identifier = character(), group_by = NULL,
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### check inputs
      add_identifier <- private$.check_add_identifier(
        add_identifier = add_identifier, several.ok = TRUE, none.ok = TRUE,
        verbose = verbose
      )
      elements <- self$elements(
        which_optimizer = which_optimizer, group_by = NULL, verbose = FALSE
      )
      which_element <- private$.check_which_element(
        which_element = which_element, choices = elements, several.ok = TRUE,
        expand_all = TRUE, verbose = FALSE
      )

      ### get ids of selected runs
      run_ids <- private$.get_run_ids(
        which_run = which_run, which_direction = which_direction,
        which_optimizer = which_optimizer, which_element = which_element,
        verbose = verbose
      )

      ### build list of results
      results <- private$.results[run_ids]
      lapply(results, function(result) {

        ### filter elements
        result[which(!names(result) %in% which_element)] <- NULL

        ### add identifier
        identifier <- attributes(result)[add_identifier]
        result <- append(result, identifier, after = 0)
      })

    },

    #' @description
    #' Provides an overview of the optimization runs.
    #' @return
    #' A \code{data.frame}.
    summary = function(
      which_element = c("value", "parameter"), which_run = "all",
      which_optimizer = "all", which_direction = "min",
      digits = getOption("digits", default = 7),
      add_identifier = character(),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### get results
      results <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = which_element, which_direction = which_direction,
        add_identifier = add_identifier, verbose = verbose
      )

      ### save results in data.frame
      out <- data.frame()
      for (i in seq_along(results)) {
        append <- results[[i]]

        ### add NAs for missing elements
        missing_results <- setdiff(
          unique(names(unlist(results, recursive = FALSE))), names(append)
        )
        append[missing_results] <- NA

        ### append
        out <- rbind(out, rbind(append), make.row.names = FALSE)
      }

      ### try to unlist
      for (i in seq_len(ncol(out))) {
        unlist_try <- unlist(out[, i], recursive = FALSE)
        if (length(unlist_try) == nrow(out)) {
          out[, i] <- unlist(out[, i], recursive = FALSE)
        }
      }

      ### round
      for (i in seq_len(ncol(out))) {
        if (is.vector(out[, i]) && is.numeric(out[, i])) {
          out[, i] <- round(out[, i], digits = digits)
        }
        if (is.list(out[, i]) && all(sapply(out[, i], is.numeric))) {
          out[[i]] <- lapply(out[, i], round, digits = digits)
        }
      }

      return(out)

    },

    #' @description
    #' Deletes optimization results.
    #' @param prompt
    #' Either \code{TRUE} to force confirmation, or \code{FALSE} else.
    #' @param replace
    #' Either \code{TRUE} (default) to replace the deleted identifiers, or
    #' \code{FALSE} else.
    #' @return
    #' Invisibly the \code{Nop} object.
    delete = function(
      which_run, which_optimizer = "all", which_direction = "min",
      prompt = interactive(), replace = TRUE
    ) {
      if (missing(which_run)) {
        cli::cli_abort("Please specify {.var which_run}.")
      }
      checkmate::assert_flag(prompt)
      checkmate::assert_flag(replace)
      run_ids <- private$.get_run_ids(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = "all", which_direction = which_direction,
        verbose = FALSE
      )
      if (length(run_ids) > 0) {
        if (prompt) {
          question <- glue::glue(
            "Do you really want to delete {length(run_ids)} results?"
          )
          if (!user_confirm(question, default = FALSE)) {
            return(invisible(self))
          }
        }
        private$.delete_results(run_ids = run_ids, replace = replace)
      }
      invisible(self)
    },

    #' @description
    #' Provides a frequency overview of the identified optimum values.
    #' @param sort_by
    #' Either:
    #' - \code{"frequency"} (default) to sort rows by frequency,
    #' - \code{"value"} to sort by function value.
    #' @param print.rows
    #' An \code{integer}, specifying the maximal number of rows to be
    #' printed. No printing if \code{print.rows = 0}, which is the default.
    #' @return
    #' A \code{data.frame}. If \code{group_by} is not \code{NULL}, a \code{list}
    #' of \code{data.frame}s.
    optima = function(
      which_run = "comparable", which_direction = c("min", "max"),
      which_optimizer = "all", group_by = NULL, sort_by = "frequency",
      digits = getOption("digits", default = 7), print.rows = 0,
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### input checks
      checkmate::assert_count(digits)
      checkmate::assert_count(print.rows)
      sort_by <- oeli::match_arg(sort_by, c("frequency", "value"))


      # TODO
      if (is.null(group_by)) {
        add_identifier <- character()
      } else {
        add_identifier <- group_by
      }

      ###
      data <- self$summary(
        which_element = "value", which_run = which_run,
        which_direction = which_direction, which_optimizer = which_optimizer,
        digits = digits, add_identifier = add_identifier, verbose = verbose
      )
      if (length(data) == 0) {
        return(data.frame())
      }

      ### split
      if (!is.null(group_by)) {
        data <- split(data, factor(data[[group_by]]))
      } else {
        data <- list(data)
      }

      ###
      optima <- list()
      for (i in seq_along(data)) {
        values <- data[[i]][["value"]]
        table <- as.data.frame(table(values, useNA = "ifany"))
        colnames(table) <- c("value", "frequency")
        decreasing <- identical(sort_by, "frequency") ||
          identical(which_direction, "max")
        table <- table[order(table[[sort_by]], decreasing = decreasing), ]
        rownames(table) <- NULL
        optima[[i]] <- table
      }
      names(optima) <- names(data)

      ###
      if (print.rows == 0) {
        if (is.null(group_by)) {
          return(optima[[1]])
        } else {
          return(optima)
        }
      } else {
        for (i in seq_along(optima)) {
          cli::cat_bullet(names(optima[i]), "\n")
          table <- optima[[i]]
          n <- min(nrow(table), print.rows)
          print(table[seq_len(n), ])
          if (n < nrow(table)) {
            cli::cli_alert_info(
              "Omitted {nrow(table) - n} row{?s}."
            )
          }
          cat("\n")
        }
        return(invisible(optima))
      }
    },

    #' @description
    #' Visualizes the optimization time or value.
    #' @param which_element
    #' Either:
    #' - \code{"seconds"} to plot the optimization times (default)
    #' - \code{"value"} to plot the optimization values
    #' @param relative
    #' Only if \code{which_element = "seconds"}.
    #' In this case, set to \code{TRUE} to plot relative time differences with
    #' respect to the overall median.
    #' @param ...
    #' Currently not used.
    #' @return
    #' A \code{\link[ggplot2]{ggplot}} object.
    #' @importFrom stats complete.cases median
    #' @importFrom dplyr summarize mutate
    #' @importFrom ggplot2 ggplot aes scale_x_continuous theme_minimal theme
    #' @importFrom ggplot2 geom_boxplot geom_vline annotate element_blank ggtitle
    #' @importFrom ggplot2 coord_cartesian
    #' @importFrom scales label_percent
    #' @importFrom forcats fct_reorder
    #' @importFrom rlang .data
    #' @importFrom scales percent
    plot = function(
      which_element = "seconds", group_by = NULL, relative = FALSE,
      which_run = "comparable", which_direction = c("min", "max"),
      which_optimizer = "all", ...
    ) {

      ### input checks
      which_element <- oeli::match_arg(which_element, c("seconds", "value"))
      if (!is.null(group_by)) {
        group_by <- heckmate::assert_choice(
          group_by, c(".optimization_label", ".optimizer_label")
        )
      }
      checkmate::assert_flag(relative)
      if (identical(which_element, "value") && relative) {
        cli::cli_warn(
          "Set {.var relative} to {.val FALSE}.",
          "Cannot be {.val TRUE} if {.var which_element} is {.val value}."
        )
        relative <- FALSE
      }

      ###
      data <- self$summary(
        which_element = which_element, which_run = which_run,
        which_optimizer = which_optimizer, which_direction = which_direction,
        digits = Inf,
        add_identifier = c(".optimization_label", ".optimizer_label")
      )
      incomplete_cases <- which(!stats::complete.cases(data))
      if (length(incomplete_cases) > 0) {
        cli::cli_alert_info(
          "Dropped {length(incomplete_cases)} row{?s} with missing data."
        )
        data <- data[-incomplete_cases, , drop = FALSE]
      }
      if (nrow(data) == 0) {
        cli::cli_alert_info("No data to plot.")
        return(invisible(NULL))
      }
      if (identical(which_element, "seconds") && relative) {
        med <- dplyr::summarize(
          data,
          "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
        ) |> as.numeric()
        data <- data |>
          dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med)
      }
      if (identical(which_element, "seconds") && !is.null(group_by)) {
        data <- data |>
          dplyr::mutate(
            label = forcats::fct_reorder(
              .f = .data[[group_by]], .x = .data[["seconds"]],
              .fun = stats::median, .desc = TRUE
            )
          )
      }
      if (is.null(group_by)) {
        base_plot <- ggplot2::ggplot(
          data, ggplot2::aes(x = .data[[which_element]], y = "")
        )
      } else {
        base_plot <- ggplot2::ggplot(
          data, ggplot2::aes(x = .data[[which_element]], y = .data[[group_by]])
        )
      }
      base_plot <- base_plot +
        ggplot2::theme_minimal()
      if (identical(which_element, "value")) {
        base_plot <- base_plot +
          ggplot2::geom_jitter(
            alpha = 0.5, width = 0
          ) +
          ggplot2::scale_x_continuous(
            name = "Function value at optimum"
          )
      }
      if (identical(which_element, "seconds")) {
        base_plot <- base_plot +
          ggplot2::geom_boxplot()
        if (relative) {
          base_plot <- base_plot +
            ggplot2::scale_x_continuous(
              labels = scales::label_percent(style_positive = c("plus")),
              name = "Relative deviation in optimization time"
            )
        } else {
          base_plot <- base_plot +
            ggplot2::scale_x_continuous(
              name = "Optimization time in seconds",
              limits = c(0, NA)
            )
        }
        if (!is.null(group_by)) {
          med <- dplyr::summarize(
            data,
            "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
          ) |> as.numeric()
          base_plot <- base_plot + ggplot2::geom_vline(
            xintercept = med
          ) +
            ggplot2::annotate(
              x = med, y = Inf, label = "Overall median",
              geom = "label", vjust = 1
            )
        }
      }
      if (is.null(group_by)) {
        base_plot <- base_plot +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
          )
      } else {
        base_plot <- base_plot +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank()
          )
      }

      ### add plot title
      base_plot + ggplot2::ggtitle(
        label = paste0("Optimization of ", self$f_name)
      )

      ### return plot
      return(base_plot)
    },

    #' @description
    #' Capture trace of optimization with \code{stats::nlm()}.
    #' @param initial
    #' A \code{numeric} vector of length \code{npar}, the starting point for
    #' the optimization.
    #' By default, \code{initial = stats::rnorm(self$npar)}, i.e., random
    #' initial values drawn from a standard normal distribution.
    #' @param iterations
    #' A positive \code{integer}, the maximum number of iterations before
    #' termination.
    #' By default, \code{interations = 100}.
    #' @param tolerance
    #' A non-negative \code{numeric}, the minimum allowable absolute change in
    #' the function value before termination.
    #' By default, \code{tolerance = 1e-6}.
    #' @param which_element
    #' A \code{character} (vector) of elements to provide in the output for
    #' each iteration. Can be one or more of:
    #' - \code{"value"}, the current function value,
    #' - \code{"parameter"}, the current value of each parameter,
    #' - \code{"gradient"}, the current gradient,
    #' - \code{"hessian"}, the current Hessian,
    #' - \code{"seconds"}, the computation time in seconds.
    #' @param ...
    #' Additional arguments passed on to \code{\link[stats]{nlm}}.
    #' The arguments \code{iterlim} and \code{hessian} cannot be specified.
    #' @return
    #' A \code{data.frame} with iterations in rows, the columns depend on the
    #' specification of \code{which_element}.
    #' @importFrom stats rnorm nlm
    trace = function(
      initial = stats::rnorm(self$npar), iterations = 100, tolerance = 1e-6,
      which_direction = "min",
      which_element = c("value", "parameter", "gradient", "hessian", "seconds"),
      ...
    ) {

      ### input checks
      checkmate::assert_count(iterations)
      checkmate::assert_number(tolerance, lower = 0)
      which_element <- oeli::match_arg(
        which_element,
        choices = c("value", "parameter", "gradient", "hessian", "seconds"),
        several.ok = TRUE
      )

      ### define nlm optimizer
      args <- list(...)
      args[["iterlim"]] <- 1
      args[["hessian"]] <- "hessian" %in% which_element
      nlm_opt <- do.call(optimizer_nlm, args)

      ### storage for trace
      out_colnames <- c(
        if ("value" %in% which_element) "v",
        if ("parameter" %in% which_element) paste0("p", 1:self$npar),
        if ("gradient" %in% which_element) paste0("g", 1:self$npar),
        if ("hessian" %in% which_element)
          paste0(
            "h", rep(1:self$npar, times = self$npar),
            rep(1:self$npar, each = self$npar)
          ),
        if ("seconds" %in% which_element) "s"
      )
      out <- matrix(NA_real_, nrow = 0, ncol = length(out_colnames))
      colnames(out) <- out_colnames

      ### track optimization path
      current_value <- self$evaluate(at = initial)
      current_initial <- initial
      for (i in seq_len(iterations)) {
        step <- self$.__enclos_env__$private$.optimize(
          initial = current_initial,
          optimizer = nlm_opt,
          which_direction = which_direction,
          seconds = NULL,
          hide_warnings = TRUE
        )
        step_pars <- c(
          if ("value" %in% which_element) step$value,
          if ("parameter" %in% which_element) step$parameter,
          if ("gradient" %in% which_element) step$gradient,
          if ("hessian" %in% which_element) as.numeric(step$hessian),
          if ("seconds" %in% which_element) step$seconds
        )
        out <- rbind(out, step_pars, deparse.level = 0)
        deviation <- abs(current_value - step$value)
        if (deviation < tolerance) break
        current_value <- step$value
        current_initial <- step$parameter
      }
      as.data.frame(out)

    },

    #' @description
    #' Stores the true best function value or parameter vector.
    #' @param input
    #' Either
    #' - \code{NULL} to return the stored true best value or parameter,
    #' - \code{NA} to remove it,
    #' - a \code{numeric} to define it.
    #' @return
    #' Either a \code{numeric} if \code{input} is \code{NULL} or invisibly the
    #' \code{Nop} object, else.
    true = function(
      input = NULL, which_element = "parameter", which_direction = "min",
      digits = getOption('digits', default = 7),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### input checks
      which_element <- private$.check_which_element(
        which_element = which_element, choices = c("value", "parameter"),
        several.ok = FALSE, verbose = verbose
      )
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = FALSE,
        verbose = verbose
      )
      checkmate::assert_count(digits)
      checkmate::assert_flag(verbose)

      ###
      if (is.null(input)) {
        if (identical(which_direction, "min")) {
          if (identical(which_element, "parameter")) {
            out <- private$.true_parameter_min
            if (is.null(out)) {
              cli::cli_warn(
                "The true minimum parameter vector is not specified."
              )
            }
          } else {
            out <- private$.true_value_min
            if (is.null(out)) {
              cli::cli_warn(
                "The true minimum function value is not specified."
              )
            }
          }
        } else {
          if (identical(which_element, "parameter")) {
            out <- private$.true_parameter_max
            if (is.null(out)) {
              cli::cli_warn(
                "The true maximum parameter vector is not specified."
              )
            }
          } else {
            out <- private$.true_value_max
            if (is.null(out)) {
              cli::cli_warn(
                "The true maximum function value is not specified."
              )
            }
          }
        }
        return(out)
      } else if (all(is.na(input))) {
        if (identical(which_direction, "min")) {
          if (identical(which_element, "parameter")) {
            private$.true_parameter_min <- NULL
            if (verbose) {
              cli::cli_alert_info(
                "Removed true minimum parameter vector."
              )
            }
          } else {
            private$.true_value_min <- NULL
            private$.true_parameter_min <- NULL
            if (verbose) {
              cli::cli_alert_info(
                "Removed true minimum function value and parameter vector."
              )
            }
          }
        } else {
          if (identical(which_element, "parameter")) {
            private$.true_parameter_max <- NULL
            if (verbose) {
              cli::cli_alert_info(
                "Removed true maximum parameter vector."
              )
            }
          } else {
            private$.true_value_max <- NULL
            private$.true_parameter_max <- NULL
            if (verbose) {
              cli::cli_alert_info(
                "Removed true maximum function value and parameter vector."
              )
            }
          }
        }
        return(invisible(self))
      } else {
        if (identical(which_direction, "min")) {
          if (identical(which_element, "parameter")) {
            private$.check_target_argument(input)
            self$true(
              input = self$evaluate(at = input),
              which_element = "value", which_direction = "min"
            )
            private$.true_parameter_min <- input
            cli::cli_alert_info(
              "Set true minimum parameter vector to,
              {round(private$.true_parameter_min, digits = digits)}."
            )
          } else {
            checkmate::assert_numeric(input)
            if (!is.null(private$.true_parameter_min)) {
              true_value_old <- self$evaluate(at = private$.true_parameter_min)
              if (input != true_value_old) {
                cli::cli_abort(
                  "Update or remove the true minimum parameter vector first."
                )
              }
            }
            private$.true_value_min <- input
            cli::cli_alert_info(
              "Set true minimum function value to,
              {round(private$.true_value_min, digits = digits)}."
            )
          }
        } else {
          if (identical(which_element, "parameter")) {
            private$.check_target_argument(input)
            self$true(
              input = self$evaluate(at = input),
              which_element = "value", which_direction = "max"
            )
            private$.true_parameter_max <- input
            cli::cli_alert_info(
              glue::glue(
                "Set true maximum parameter vector to",
                "{toString(round(private$.true_parameter_max, digits = digits))}.",
                .sep = " "
              )
            )
          } else {
            checkmate::assert_numeric(input)
            if (!is.null(private$.true_parameter_max)) {
              true_value_old <- self$evaluate(at = private$.true_parameter_max)
              if (input != true_value_old) {
                cli::cli_abort(
                  "Update or remove the true maximum parameter vector first."
                )
              }
            }
            private$.true_value_max <- input
            cli::cli_alert_info(
              glue::glue(
                "Set true maximum function value to (rounded)",
                "{round(private$.true_value_max, digits = digits)}.",
                .sep = " "
              )
            )
          }
        }
        return(invisible(self))
      }
    },

    #' @description
    #' Returns the best found function value or parameter vector.
    #' @param which_element
    #' Either \code{"value"} or \code{"parameter"}.
    #' @param which_direction
    #' Either \code{"min"} or \code{"max"}.
    #' @details
    #' In the case that multiple optimization runs led to the best value, only
    #' the first one of them is considered.
    #' @return
    #' A \code{numeric} (vector) with two attributes:
    #' - \code{.run_id}, the run id that led to the best value,
    #' - \code{.optimizer_label}, the optimizer that led to the best value.
    best = function(
      which_element = "value", which_run = c("success", "comparable"),
      which_direction = "min", which_optimizer = "all",
      digits = getOption("digits", default = 7),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {

      ### check inputs
      which_element <- private$.check_which_element(
        which_element = which_element, choices = c("value", "parameter"),
        several.ok = FALSE, verbose = verbose
      )
      which_run <- private$.check_which_run(
        which_run = which_run, verbose = verbose
      )
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = FALSE,
        verbose = verbose
      )
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = FALSE, verbose = verbose
      )

      ### get results
      data <- self$summary(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = c("value", "parameter"),
        which_direction = which_direction,
        digits = Inf,
        add_identifier = c(".run_id", ".optimizer_label"),
        verbose = verbose
      )
      if (nrow(data) == 0) {
        return(invisible(NULL))
      }

      ### extract best
      best <- do.call(
        what = switch (which_direction, "min" = which.min, "max" = which.max),
        args = list(data[["value"]])
      )
      structure(
        round(unlist(data[best, which_element]), digits = digits),
        ".run_id" = data[best, ".run_id"],
        ".optimizer_label" = data[best, ".optimizer_label"]
      )
    }

  ),

  active = list(

    #' @field npar The lengths of the target arguments.
    npar = function(value) {
      if (missing(value)) {
        private$objective$npar
      } else {
        cli::cli_abort(
          "The field {.var $npar} is read only.",
          call = NULL
        )
      }
    },

    #' @field fresh_label An optimization label that has not been used yet.
    fresh_label = function(value) {
      if (missing(value)) {
        default_label <- "unlabeled"
        n <- 1
        while (TRUE) {
          label <- glue::glue("{default_label}_{n}")
          if (!label %in% names(private$.optimization_label_ids)) {
            return(as.character(label))
          } else {
            n <- n + 1
          }
        }
      } else {
        cli::cli_abort(
          "The field {.var $fresh_label} is read only.",
          call = NULL
        )
      }
    },

    #' @field verbose
    #' Either \code{TRUE} to print progress and details, or \code{FALSE} to hide
    #' such messages.
    #' This can be defined globally via \code{options("ino_verbose" = TRUE)}.
    verbose = function(value) {
      if (missing(value)) {
        private$.verbose
      } else {
        checkmate::assert_flag(value)
        private$.verbose <- value
      }
    },

    #' @field digits
    #' An \code{integer}, the number of decimal places of interest.
    #' This can be defined globally via \code{options("digits" = 7)}.
    digits = function(value) {
      if (missing(value)) {
        private$.digits
      } else {
        checkmate::assert_int(value, lower = 0)
        private$.digits <- value
      }
    }

  ),

  private = list(

    ### storage of options
    .verbose = getOption("ino_verbose", default = FALSE),
    .digits = getOption("digits", default = 7),

    ### storage of the optimization problem details
    objective = NULL,
    .true_parameter_min = NULL,
    .true_parameter_max = NULL,
    .true_value_min = NULL,
    .true_value_max = NULL,
    .arguments = list(),
    .original_arguments = list(),
    .optimizer = list(),

    ### storage of the initial values
    .initial_values = list(),
    .initial_type = character(),
    .initial_seconds = numeric(),

    ### storage of the optimization results and ids
    .results = list(),
    .optimizer_id_ids = list(),
    .optimization_label_ids = list(),
    .comparable_ids = integer(),
    .direction_ids = list("min" = integer(), "max" = integer()),
    .failed_ids = integer(),
    .last_runs_ids = integer(),
    .element_ids = list(),

    ### determine the next run id
    .next_run_id = function() {
      length(private$.results) + 1
    },

    ### determine whether the result is comparable
    .result_comparable = function() {
      length(private$.original_arguments) == 0
    },

    ### save optimization results
    .save_results = function(results, optimization_label, verbose) {
      if (verbose) {
        cli::cli_alert_info(
          "Saving {length(results)} optimization result{?s}."
        )
      }
      for (i in seq_along(results)) {
        result <- results[[i]]
        run_id <- private$.next_run_id()
        parts <- names(result)
        if ("value" %in% parts) {
          checkmate::assert_number(result[["value"]], na.ok = TRUE)
        } else {
          result[["value"]] <- NA_real_
        }
        if ("parameter" %in% parts) {
          checkmate::assert_numeric(result[["parameter"]], all.missing = TRUE)
        } else {
          result[["parameter"]] <- NA_real_
        }
        if ("seconds" %in% parts) {
          checkmate::assert_number(result[["seconds"]], na.ok = TRUE, lower = 0)
          add_seconds <- private$.initial_seconds[i]
          if (checkmate::test_number(add_seconds)) {
            result[["seconds"]] <- result[["seconds"]] + add_seconds
          }
        } else {
          result[["seconds"]] <- NA_real_
        }
        if ("initial" %in% parts) {
          checkmate::assert_numeric(result[["initial"]])
        } else {
          result[["initial"]] <- NA_real_
        }
        if ("error" %in% parts) {
          checkmate::assert_flag(result[["error"]], na.ok = TRUE)
        } else {
          result[["error"]] <- FALSE
        }
        error <- result[["error"]]
        if ("error_message" %in% parts) {
          checkmate::assert_string(result[["error_message"]], na.ok = TRUE)
        } else {
          result[["error_message"]] <- NA_character_
        }
        attr(result, ".run_id") <- run_id
        attr(result, ".optimization_label") <- optimization_label
        optimizer_id <- attr(result, ".optimizer_id")
        optimizer_label <- attr(result, ".optimizer_label")
        comparable <- attr(result, ".comparable")
        direction <- attr(result, ".direction")
        private$.results[[run_id]] <- result
        private$.last_runs_ids <- c(private$.last_runs_ids, run_id)
        if (length(private$.optimizer_id_ids) < optimizer_id) {
          private$.optimizer_id_ids[[optimizer_id]] <- run_id
        } else {
          private$.optimizer_id_ids[[optimizer_id]] <- c(
            private$.optimizer_id_ids[[optimizer_id]], run_id
          )
        }
        if (is.null(private$.optimization_label_ids[[optimization_label]])) {
          private$.optimization_label_ids[[optimization_label]] <- run_id
        } else {
          private$.optimization_label_ids[[optimization_label]] <- c(
            private$.optimization_label_ids[[optimization_label]], run_id
          )
        }
        if (comparable) {
          private$.comparable_ids <- c(private$.comparable_ids, run_id)
        }
        private$.direction_ids[[direction]] <- c(
          private$.direction_ids[[direction]], run_id
        )
        if (error) {
          private$.failed_ids <- c(private$.failed_ids, run_id)
        }
        for (element in names(result)) {
          private$.element_ids[[element]] <- c(
            private$.element_ids[[element]], run_id
          )
        }
      }
    },

    ### delete optimization results
    .delete_results = function(run_ids, replace) {
      for (run_id in run_ids) {
        if (replace) {
          private$.results[run_id] <- NULL
        } else {
          private$.results[run_id] <- list(NULL)
        }
        private$.last_runs_ids <- remove_index(
          private$.last_runs_ids, index = run_id, replace = replace
        )
        private$.optimizer_id_ids <- lapply(
          private$.optimizer_id_ids, remove_index, index = run_id,
          replace = replace
        )
        private$.optimization_label_ids <- lapply(
          private$.optimization_label_ids, remove_index, index = run_id,
          replace = replace
        )
        private$.comparable_ids <- remove_index(
          private$.comparable_ids, index = run_id, replace = replace
        )
        private$.failed_ids <- remove_index(
          private$.failed_ids, index = run_id, replace = replace
        )
        private$.element_ids <- lapply(
          private$.element_ids, remove_index, index = run_id,
          replace = replace
        )
        cli::cli_alert_info(
          "Result with id {run_id} is removed."
        )
      }
    },

    ### check `which_run` filter
    .check_which_run = function(which_run, verbose = self$verbose) {

      ### input checks
      checkmate::assert_flag(verbose)

      ### check `which_run`
      if (checkmate::test_character(which_run)) {

        ### TODO check if all is alone, no contradictions
        return(which_run)

      } else if (checkmate::test_integerish(which_optimizer, lower = 1)) {

        ### TODO check if ids exist
        return(which_run)

      } else {

        cli::cli_abort(
          "Argument {.var which_run} is misspecified."
        )

      }

    },

    ### check `which_direction` filter
    .check_which_direction = function(
      which_direction, both_allowed = FALSE, verbose = self$verbose
    ) {

      ### input checks
      checkmate::assert_flag(both_allowed)
      checkmate::assert_flag(verbose)

      ### check `which_direction`
      which_direction <- oeli::match_arg(
        which_direction, choices = c("min", "max"), several.ok = both_allowed
      )


      if (identical(which_direction, "min")) {

        if (verbose) {
          cli::cli_alert_info("Selected minimization")
        }
        return("min")

      } else if (identical(which_direction, "max")) {

        if (verbose) {
          cli::cli_alert_info("Selected maximization")
        }
        return("max")

      } else {

        if (verbose) {
          cli::cli_alert_info("Selected minimization and maximization.")
        }
        return(c("min", "max"))

      }

    },

    ### check `which_optimizer` filter
    .check_which_optimizer = function(
      which_optimizer, to_id, verbose
    ) {

      ### input checks
      checkmate::assert_flag(to_id)
      checkmate::assert_flag(verbose)

      ### check whether any optimizer is specified
      optimizer_ids <- seq_along(private$.optimizer)
      if (length(optimizer_ids) == 0) {
        cli::cli_warn(
          "No optimizer specified.",
          "Use {.fun $set_optimizer} to specify an optimizer."
        )
        return(integer(0))
      }

      ### check `which_optimizer`
      if (checkmate::test_character(which_optimizer)) {

        if ("all" %in% which_optimizer) {

          if (length(unique(which_optimizer)) > 1) {
            cli::cli_abort(
              "Filter {.var which_optimizer = {.val all}} cannot be paired with
              other optimizer filters."
            )
          }
          if (verbose) {
            cli::cli_alert_info(
              "Selected all specified optimizers."
            )
          }
          if (to_id) {
            return(seq_along(private$.optimizer))
          } else {
            return("all")
          }

        } else {

          optimizer_ids <- which(names(private$.optimizer) %in% which_optimizer)
          optimizer_labels <- names(private$.optimizer)[optimizer_ids]

          if (length(optimizer_ids) > 0 && verbose) {
            cli::cli_alert_info("Selected optimizer {optimizer_labels}.")
          }

          if (to_id) {
            return(optimizer_ids)
          } else {
            return(optimizer_labels)
          }

        }

      } else if (checkmate::test_integerish(which_optimizer, lower = 1)) {

        optimizer_ids <- intersect(optimizer_ids, which_optimizer)
        optimizer_labels <- names(private$.optimizer)[optimizer_ids]

        if (length(optimizer_ids) > 0 && verbose) {
          cli::cli_alert_info(
            "Selected optimizer {optimizer_labels}."
          )
        }

        if (to_id) {
          return(optimizer_ids)
        } else {
          return(optimizer_labels)
        }

      } else {

        cli::cli_abort(
          "Argument {.var which_optimizer} is misspecified."
        )

      }

    },

    ### check `which_element` filter
    .check_which_element = function(
      which_element, choices = NULL, several.ok = TRUE, expand_all = FALSE,
      verbose = verbose
    ) {

      ### input checks
      checkmate::assert_flag(verbose)
      checkmate::assert_flag(several.ok)

      ### check `which_element`
      if (!is.null(choices)) {
        which_element <- oeli::match_arg(
          arg = which_element, choices = choices, several.ok = several.ok,
          none.ok = FALSE
        )
      }
      return(which_element)

    },

    ### check `add_identifier` filter
    .check_add_identifier = function(
      add_identifier, several.ok, none.ok, verbose
    ) {

      ### input checks
      checkmate::assert_flag(several.ok)
      checkmate::assert_flag(none.ok)
      checkmate::assert_flag(verbose)

      ### check `add_identifier` input
      oeli::match_arg(
        add_identifier, choices = c(
          ".run_id", ".optimization_label", ".optimizer_id",
          ".optimizer_label", ".comparable", ".direction"
        ),
        several.ok = several.ok, none.ok = none.ok
      )

      ### TODO add verbose output of selected identifiers
    },

    ### return run ids based on filters
    .get_run_ids = function(
      which_run, which_direction, which_optimizer, which_element, verbose
    ) {

      ### check inputs
      checkmate::assert_flag(verbose)
      which_run <- private$.check_which_run(
        which_run, verbose = FALSE
      )
      which_direction <- private$.check_which_direction(
        which_direction, both_allowed = TRUE, verbose = FALSE
      )
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer, to_id = FALSE, verbose = FALSE
      )
      which_element <- private$.check_which_element(
        which_element, verbose = FALSE
      )

      ### start with all ids
      run_ids <- seq_along(private$.results)
      if (length(run_ids) == 0) {
        cli::cli_warn("No optimization results available.")
        return(integer())
      }

      ### filter 'which_run'
      if (checkmate::test_character(which_run)) {

        for (filter in which_run) {

          if (filter %in% self$reserved_labels) {

            # if (identical(filter, "all")) {
            #   if (verbose) {
            #     cli::cli_alert_info("Filtered for all results.")
            #   }
            # }

            if (identical(filter, "last")) {
              if (verbose) {
                cli::cli_alert_info("Filtered for the last optimizations.")
              }
              run_ids <- intersect(run_ids, private$.last_runs_ids)
            }

            if (identical(filter, "success")) {
              if (verbose) {
                cli::cli_alert_info("Filtered for successful optimizations.")
              }
              run_ids <- setdiff(run_ids, private$.failed_ids)
            }

            if (identical(which_run, "failed")) {
              if (verbose) {
                cli::cli_alert_info("Filtered for failed optimizations.")
              }
              run_ids <- intersect(run_ids, private$.failed_ids)
            }

            if (identical(which_run, "comparable")) {
              if (verbose) {
                cli::cli_alert_info("Filtered for comparable optimizations.")
              }
              run_ids <- intersect(run_ids, private$.comparable_ids)
            }

            if (identical(which_run, "incomparable")) {
              if (verbose) {
                cli::cli_alert_info("Filtered for incomparable optimizations.")
              }
              run_ids <- setdiff(run_ids, private$.comparable_ids)
            }

          }

          if (filter %in% names(private$.optimization_label_ids)) {

            if (verbose) {
              cli::cli_alert_info("Filtered for optimization label {filter}.")
            }
            run_ids <- intersect(
              run_ids, private$.optimization_label_ids[[filter]]
            )

          }

        }

      } else if (checkmate::test_integerish(which_run)) {

        if (verbose) {
          cli::cli_alert_info("Filtered for run id {which_run}.")
        }
        run_ids <- which(seq_along(private$.results) %in% which_run)

      }

      ### filter `which_direction`
      if (checkmate::test_character(which_direction)) {

        if (identical(which_direction, "min")) {

          if (verbose) {
            cli::cli_alert_info("Filtered for minimizations.")
          }
          run_ids <- intersect(run_ids, private$.direction_ids[["min"]])

        }

        if (identical(which_direction, "max")) {

          if (verbose) {
            cli::cli_alert_info("Filtered for maximizations.")
          }
          run_ids <- intersect(run_ids, private$.direction_ids[["max"]])

        }

      }

      ### filter `which_optimizer`
      if (checkmate::test_character(which_optimizer)) {

        for (filter in which_optimizer) {

          if (filter %in% self$reserved_labels) {

            if (identical(filter, "all")) {
              # if (verbose) {
              #   cli::cli_alert_info("Filtered for all optimizers.")
              # }
            }

          }

          if (filter %in% names(private$.optimizer)) {

            if (verbose) {
              cli::cli_alert_info("Filtered for optimizer label {filter}.")
            }
            run_ids <- intersect(
              run_ids, private$.optimizer_label_ids[[filter]]
            )

          }

        }

      } else if (checkmate::test_integerish(which_optimizer)) {

        if (verbose) {
          optimizer_labels <- names(private$.optimizer)[which_optimizer]
          cli::cli_alert_info("Filtered for optimizer {optimizer_labels}.")
        }
        run_ids <- intersect(
          run_ids, unlist(private$.optimizer_id_ids[optimizer_ids])
        )

      }

      ### filter `which_element`
      if (identical(which_element, "all")) {

        # if (verbose) {
        #   cli::cli_alert_info("Filtered for all elements.")
        # }

      } else if (checkmate::test_character(which_element)) {

        run_ids <- intersect(
          run_ids,
          unique(unlist(private$.element_ids[unlist(which_element)]))
        )

      }

      ### return ids
      if (length(run_ids) == 0) {
        cli::cli_warn("No optimization results selected.")
      }
      return(run_ids)

    }
  )
)

