#' Nop Object
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.
#'
#' @param which_run
#' Selects results of optimization runs.
#'
#' By default, \code{which_run = "all"} selects all available results.
#'
#' It can also be
#' - \code{"success"} for the results of successful optimization runs,
#' - \code{"comparable"} for the results of comparable optimizations (i.e.,
#'   results obtained for the original optimization problem without any
#'   transformations),
#' - a \code{character} (vector) of optimization labels.
#'
#' Filters for optimization runs can be negated by putting a \code{"!"} in
#' front, for example \code{which_run = "!success"} filters for failed runs.
#'
#' Filters can be combined (by default via logical *and*), for example
#' \code{which_run = c("!success", "comparable")} filters for comparable
#' runs that failed.
#'
#' Filters can also be combined via logical *or* by adding
#' \code{"logical" = "or"}, for example
#' \code{which_run = c("comparable", "label", "logical" = "or")} filters for
#' runs that are either comparable or have the label \code{"label"}.
#'
#' Alternatively, \code{which_run} can be an \code{integer} (vector) of run ids
#' as provided for example by \code{$best()}.
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
#'   optimization run,
#' - \code{".optimization_label"}, which yields a \code{character} that
#'   identifies custom groups of optimization runs,
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
#' @param verbose
#' Either \code{TRUE} to print progress and details, or \code{FALSE} to hide
#' such messages.
#' This can be defined globally via \code{self$verbose}.
#' By default, \code{verbose = getOption("verbose", default = FALSE)}.
#' @param digits
#' An \code{integer}, the number of decimal places.
#' This can be defined globally via \code{self$digits}.
#' By default, \code{digits = getOption("digits", default = 7)}.
#'
#' @return
#' Either the \code{Nop} object to allow for method chaining, or a result.
#' Please refer to the respective documentation for the output of the different
#' methods.
#'
#' @details
#' # Getting started
#'
#' ## Step 1: Create a \code{Nop} object
#' Call \code{object <- Nop$new(objective, target, npar, ...)} where
#' - \code{objective} is a single-valued function to be optimized,
#' - \code{target} specifies the name(s) of the target argument(s) (if not
#'   supplied, the first argument of \code{objective} is selected),
#' - \code{npar} specifies the length(s) of the target argument(s),
#' - and \code{...} are additional arguments for \code{objective} that are kept
#'   fixed during the optimization (if any).
#'   These arguments can be accessed via the \code{$argument()} method.
#'
#' ## Step 2: Specify numerical optimizers
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}. Such
#' objects can be created via the \code{{optimizeR}} package, please refer to
#' [the package homepage](https://loelschlaeger.de/optimizeR/) for details.
#'
#' For example,
#' - \code{optimizeR::Optimizer$new(which = "stats::nlm")} defines the
#'   \code{\link[stats]{nlm}} optimizer,
#' - \code{optimizeR::Optimizer$new(which = "stats::optim")} defines the
#'   \code{\link[stats]{optim}} optimizer.
#'
#' ## Step 3: Select initial values
#' Call initialization methods to define starting values for the
#' optimization (the different initialization strategies are illustrated in the
#' package vignettes), for example:
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
#' - \code{$validate()} checks the configurations of a \code{Nop} object,
#' - \code{$evaluate()} evaluates the target function at some point,
#' - \code{$delete()} deletes optimization results,
#' - \code{$elements()} returns the names of the available elements in the
#'   optimizer outputs,
#' - \code{$runs()} returns the number of performed optimization runs,
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
#' Nop_ackley <- Nop$new(objective = ackley, npar = 2)$  # define the Nop object
#'   set_optimizer(
#'     optimizeR::Optimizer$new(which = "stats::nlm") # select the nlm optimizer
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

Nop_old <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Creates a new \code{Nop} object.
    #' @param objective
    #' The \code{function} to be optimized (the so-called objective function).
    #' It should return a single \code{numeric} value.
    #' @param target
    #' The names of the arguments over which \code{objective} will be optimized
    #' (the so-called target arguments).
    #' If not specified, the first argument of \code{objective} is selected.
    #' Each of these arguments should expect a \code{numeric} \code{vector}.
    #' @param npar
    #' The length(s) of the target argument(s).
    #' @param ...
    #' Optionally additional, named arguments for \code{objective} which are
    #' kept fixed during the optimization.
    #' @return
    #' A new \code{Nop} object.

    initialize = function(objective, target, npar, ...) {

      ### input checks
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
      all_arguments <- oeli::function_arguments(
        objective, with_ellipsis = FALSE
      )
      if (length(all_arguments) == 0) {
        cli::cli_abort(
          c(
            "Function {.var objective} should have at least one argument.",
            "i" = "The function needs an argument over which it is optimized."
          ),
          call = NULL
        )
      }
      if (missing(target)) {
        target <- all_arguments[1]
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

      ### build objective
      private$.objective <- optimizeR::Objective$new(
        objective = objective,
        target = target,
        npar = npar,
        ...
      )
      private$.objective$objective_name <- oeli::variable_name(objective)

      ### build result index
      private$.results <- oeli::Storage$new()
      private$.results$missing_identifier <- FALSE
      private$.results$hide_warnings <- TRUE

    },

    #' @description
    #' Prints details of the numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.

    print = function(digits = self$digits, ... ) {

      ### info on optimization problem
      cli::cli_h2("Optimization problem")
      objective <- private$.objective$objective_name
      target <- private$.objective$.__enclos_env__$private$.target
      cli::cli_bullets(c(
        "*" = "Objective: {objective}",
        "*" = "Target: {target} (length {self$npar})"
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
      argument_names <- private$.objective$fixed_arguments
      if (length(argument_names) > 0) {
        modified <- argument_names %in% names(private$.original_arguments)
        if (any(modified)) {
          names(argument_names) <- ifelse(modified, "!", " ")
        }
        cli::cli_h2("Fixed arguments")
        cli::cli_bullets(argument_names)
        if (any(modified)) {
          cat(cli::style_italic("\nSome arguments are currently modified.\n"))
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
        cat(cli::style_italic("No results available.\n\n"))
      } else {
        suppressWarnings({
          ncomparable <- self$runs(which_run = "comparable", verbose = FALSE)
          nincomparable <- self$runs(which_run = "!comparable", verbose = FALSE)
          nfailed <- self$runs(which_run = "!success", verbose = FALSE)
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
        cat("\n")
      }
      invisible(self)

    },

    #' @description
    #' Manages fixed arguments for \code{objective}.
    #' @param action
    #' One of:
    #' - \code{"set"} to set an argument,
    #' - \code{"get"} to extract an argument value,
    #' - \code{"remove"} to remove an argument,
    #' - \code{"reset"} to reset an argument to the original value (if any),
    #' - \code{"modify"} to modify an argument to a new value, but the original
    #'   value is saved and can be recovered via \code{"reset"}.
    #' @param ...
    #' Additional parameters depending on \code{action}:
    #' - if \code{action = "set"} or \code{"modify"}, one or more named
    #'   arguments,
    #' - if \code{action = "get"}, \code{"remove"}, or \code{"reset"},
    #'   the \code{argument_name}, a \code{character} that selects an argument
    #' @return
    #' The argument value if \code{action = "get"} and invisibly the \code{Nop}
    #' object, else.

    fixed_argument = function(action, ...) {
      if (missing(action)) {
        cli::cli_abort(
          "Please specify the argument {.var action}.",
          call = NULL
        )
      }
      checkmate::assert_string(action)
      action <- oeli::match_arg(
        action, c("set", "get", "remove", "reset", "modify")
      )
      args <- list(...)
      arg_names <- names(args)
      if (action == "set") {
        self$set_argument(...)
      }
      if (action == "get") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        return(self$get_argument(argument_name = args[["argument_name"]]))
      }
      if (action == "remove") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        argument_name <- args[["argument_name"]]
        private$.objective$remove_argument(
          argument_name, verbose = self$verbose
        )
        if (argument_name %in% names(private$.original_arguments)) {
          arg_id <- which(names(private$.original_arguments) == argument_name)
          private$.original_arguments[arg_id] <- NULL
        }
        if (self$verbose) {
          cli::cli_alert_info("Removed argument {.var {argument_name}}.")
        }
      }
      if (action == "reset") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        argument_name <- args[["argument_name"]]
        checkmate::assert_string(argument_name)
        if (!is.null(private$.original_arguments[[argument_name]])) {
          original_argument <- private$.original_arguments[[argument_name]]
          arg <- list("set", original_argument)
          names(arg) <- c("action", argument_name)
          do.call(self$fixed_argument, arg)
          private$.original_arguments[[argument_name]] <- NULL
          if (self$verbose) {
            cli::cli_alert_info("Reset argument {.var {argument_name}}.")
          }
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
            do.call(self$fixed_argument, arg)
          }
        } else {
          argument_name <- names(args)[1]
          if (is.null(private$.original_arguments[[argument_name]])) {
            original_argument <- private$.objective$get_argument(
              argument_name, verbose = FALSE
            )
            private$.original_arguments[[argument_name]] <- original_argument
          }
          do.call(
            private$.objective$set_argument,
            c(args, list("overwrite" = TRUE, "verbose" = FALSE))
          )
          if (self$verbose) {
            cli::cli_alert_info("Modified argument {.var {argument_name}}.")
          }
        }
      }
      invisible(self)
    },

    #' @description
    #' Set a fixed argument for \code{objective}.
    #' @param ...
    #' One or more named arguments.
    #' @return
    #' Invisibly the \code{Nop} object.

    set_argument = function(...) {
      args <- list(...)
      if (length(args) == 0) {
        cli::cli_warn("No argument to set.")
      } else if (length(args) > 1) {
        for (i in 1:length(args)) {
          do.call(self$set_argument, args[i])
        }
      } else {
        do.call(
          private$.objective$set_argument,
          c(args, list("overwrite" = TRUE, "verbose" = self$verbose))
        )
      }
    },

    #' @description
    #' Get a fixed argument for \code{objective}.
    #' @param argument_name
    #' A \code{character} that selects an argument.
    #' @return
    #' Invisibly the \code{Nop} object.

    get_argument = function(argument_name) {
      private$.objective$get_argument(argument_name, verbose = self$verbose)
    },

    #' @description
    #' Standardizes the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the fixed argument of \code{object} to be
    #' standardized. The argument must a \code{numeric} \code{vector},
    #' \code{matrix}, or \code{data.frame}.
    #' @param byrow
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to standardize row-wise or
    #' \code{FALSE} (default) to standardize row-wise.
    #' @param center
    #' Either \code{TRUE} (default) for centering or \code{FALSE}, else.
    #' @param scale
    #' Either \code{TRUE} (default) for scaling or \code{FALSE}, else.
    #' @param ignore
    #' A \code{integer} (vector) of column indices (or row indices if
    #' \code{byrow = TRUE}) to not standardize.
    #' @return
    #' Invisibly the \code{Nop} object.

    standardize = function(
      argument_name, byrow = FALSE, center = TRUE, scale = TRUE,
      ignore = integer()
    ) {
      original_argument <- self$get_argument(argument_name)
      standardized_argument <- normalize::normalize(
        x = original_argument, byrow = byrow,
        center = center, scale = scale, ignore = ignore
      )
      if (self$verbose) {
        cli::cli_alert_info(
          "Standardized argument '{argument_name}'."
        )
      }
      arg <- list("modify", standardized_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
    },

    #' @description
    #' Reduces the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be reduced.
    #' @param byrow
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to reduce row-wise (default) or
    #' \code{FALSE} to reduce column-wise.
    #' @param how
    #' A \code{character}, specifying how to reduce. Can be one of:
    #' - \code{"random"} (default), reduce at random
    #' - \code{"first"}, reduce to the first elements
    #' - \code{"last"}, reduce to the last elements
    #' - \code{"similar"}, reduce to similar elements
    #' - \code{"dissimilar"}, reduce to dissimilar elements
    #' Note that \code{"similar"} and \code{"dissimilar"} are based on k-means
    #' clustering via \code{\link[stats]{kmeans}}. To apply these options,
    #' the argument \code{argument_name} must be \code{numeric}.
    #' @param proportion
    #' A \code{numeric} between \code{0} and \code{1}, specifying the
    #' reduction proportion.
    #' By default, \code{proportion = 0.5}.
    #' @param centers
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case, passed to \code{\link[stats]{kmeans}}.
    #' By default, \code{centers = 2}.
    #' @param ignore
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case a \code{integer} (vector) of row indices (or column indices
    #' if \code{byrow = FALSE}) to ignore for clustering.
    #' @return
    #' Invisibly the \code{Nop} object.

    reduce = function(
      argument_name, byrow = TRUE, how = "random", proportion = 0.5,
      centers = 2, ignore = integer()
    ) {
      original_argument <- self$get_argument(argument_name)
      reduced_argument <- portion::portion(
        x = original_argument, byrow = byrow, how = how,
        proportion = proportion, centers = centers, ignore = ignore
      )
      if (self$verbose) {
        cli::cli_alert_info(
          glue::glue(
            "Reduced argument '{argument_name}' from ",
            if (checkmate::test_atomic_vector(original_argument)) {
              length_old <- length(original_argument)
              length_new <- length(reduced_argument)
              "{length_old} to {length_new} {how} element(s)."
            } else {
              if (byrow) {
                nrow_old <- nrow(original_argument)
                nrow_new <- nrow(reduced_argument)
                glue::glue(
                  "{nrow_old} to {nrow_new} {how} row(s).",
                )
              } else {
                ncol_old <- ncol(original_argument)
                ncol_new <- ncol(reduced_argument)
                glue::glue(
                  "{ncol_old} to {ncol_new} {how} column(s).",
                )
              }
            }
          )
        )
      }
      arg <- list("modify", reduced_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
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
      if (self$verbose) {
        cli::cli_alert_info("Set optimizer {.val {optimizer_label}}.")
      }
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
      private$.check_target(at, verbose = FALSE)
      private$.objective$seconds <- seconds
      private$.objective$hide_warnings <- hide_warnings
      private$.objective$evaluate(at)
    },

    #' @description
    #' Defines fixed initial values for the optimization.
    #' @param at
    #' A \code{numeric} \code{vector} of length \code{self$sum(npar)}, the
    #' initial parameter vector.
    #' It can also be a \code{list} of such vectors.
    #' @return
    #' Invisibly the \code{Nop} object.

    initialize_fixed = function(at) {
      if (!is.list(at)) {
        at <- list(at)
      }
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
      lower = rep(0, sum(self$npar)),
      upper = rep(1, sum(self$npar)),
      breaks = rep(3, sum(self$npar)),
      jitter = FALSE, ...
    ) {
      if (checkmate::test_number(lower)) {
        lower <- rep(lower, sum(self$npar))
      }
      if (checkmate::test_number(upper)) {
        upper <- rep(upper, sum(self$npar))
      }
      if (checkmate::test_number(breaks)) {
        breaks <- rep(breaks, sum(self$npar))
      }
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
      at <- lapply(as.list(asplit(expand.grid(grid_points), 1)), as.numeric)
      if (jitter) {
        at <- lapply(at, jitter, ...)
      }
      at <- as.list(at)
      self$initialize_custom(at, type = "grid")
    },

    #' @description
    #' Defines initial values based on results from previous optimizations.
    #' @return
    #' Invisibly the \code{Nop} object.

    initialize_continue = function(
      which_run, which_optimizer = "all", which_direction = c("min", "max")
    ) {
      out <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_direction = which_direction,
        which_element = c("parameter", "seconds")
      )
      at <- lapply(out, `[[`, "parameter")
      seconds <- sapply(out, `[[`, "seconds")
      runs <- length(at)
      drop <- integer()
      for (i in seq_len(runs)) {
        check <- try(
          private$.check_target(at[[i]], verbose = FALSE),
          silent = TRUE
        )
        if (inherits(check, "try-error")) {
          drop <- c(drop, i)
        }
      }
      if (length(drop) > 0) {
        at <- at[-drop]
        seconds <- seconds[-drop]
        if (self$verbose) {
          cli::cli_alert_info(
            "{length(drop)} set{?s} of results cannot be continued."
          )
        }
      }
      self$initialize_custom(
        at = at, seconds = seconds, type = "continued"
      )
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

    initialize_custom = function(
      at, seconds = rep(0, length(at)), type = "custom"
    ) {
      checkmate::assert_list(at)
      checkmate::assert_string(type)
      runs <- length(at)
      lapply(at, private$.check_target, verbose = FALSE)
      checkmate::assert_numeric(seconds, lower = 0, len = runs)
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
          gradient_norms <- sapply(
            private$.initial_values,
            function(x) {
              grad <- numDeriv::grad(
                func = private$.objective$evaluate,
                x = x
              )
              sqrt(sum(grad^2))
            }
          )
          indices <- order(gradient_norms, decreasing = TRUE)[seq_len(runs)]
        } else {
          values <- sapply(private$.initial_values, self$evaluate)
          indices <- order(values, decreasing = (condition == "value_high"))[seq_len(runs)]
        }
        private$.initial_values <- private$.initial_values[indices]
        private$.initial_seconds <- private$.initial_seconds[indices]
        private$.initial_type <- private$.initial_type[indices]
        if (self$verbose) {
          cli::cli_alert_info(
            "Reduced to a subset of {runs} initial parameter value{?s} based
            on the condition {.val {condition}}."
          )
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
      private$.check_arguments_complete(verbose = self$verbose)
      checkmate::assert_string(optimization_label)
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = self$verbose
      )
      if (length(optimizer_ids) == 0) {
        return(invisible(self))
      }
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = TRUE,
        verbose = self$verbose
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
        cli::cli_alert_info("Optimization with label {.val {optimization_label}}.")
      }
      progress_step <- progressr::progressor(steps = nrow(combinations))
      results <- future.apply::future_apply(
        combinations, MARGIN = 1, function(x) {
          out <- private$.optimize(
            initial = x$initial,
            optimizer_id = x$optimizer_id,
            direction = x$which_direction,
            seconds = seconds,
            hide_warnings = hide_warnings
          )
          progress_step()
          return(out)
        }, future.seed = TRUE
      )

      ### save results
      private$.save_results(
        results = results, optimization_label = optimization_label
      )
      if (reset_initial) self$initialize_reset()
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

      ### input checks
      private$.check_target(at)
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
    #' @return
    #' A \code{character} if \code{group_by = NULL}, otherwise a
    #' \code{list} of \code{character}s.

    elements = function(
      which_optimizer = "all", group_by = NULL, verbose = self$verbose
    ) {

      ### input checks
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = verbose
      )
      group_by <- private$.check_group_by(
        group_by = group_by, verbose = verbose
      )

      ### get element names
      if (is.null(group_by)) {
        out <- private$.results$identifier
        out <- out[startsWith(out, "element:")]
        out <- sub(".*:", "", out)
      } else {
        if (group_by == ".optimization_label") {
          out <- rep(list(character()), length(private$.optimization_labels))
          names(out) <- private$.optimization_labels
          for (optimization_label in names(out)) {
            out[[optimization_label]] <- unique(
              names(
                unlist(
                  private$.results$get(
                    identifier = c(
                      paste0("optimization_label:", optimization_label),
                      paste0("optimizer_id:", optimizer_ids)
                    ),
                    locical = "and"
                  ),
                  recursive = FALSE
                )
              )
            )
          }
        } else if (group_by == ".optimizer_label") {
          out <- rep(list(character()), length(private$.optimizer[optimizer_ids]))
          names(out) <- names(private$.optimizer[optimizer_ids])
          for (optimizer_label in names(out)) {
            out[[optimizer_label]] <- unique(
              names(
                unlist(
                  private$.results$get(
                    identifier = paste0("optimizer_label:", optimizer_label),
                    logical = "and"
                  ),
                  recursive = FALSE
                )
              )
            )
          }
        } else {
          oeli::unexpected_error(
            issue_link = "https://github.com/loelschlaeger/ino/issues"
          )
        }
      }
      return(out)
    },

    #' @description
    #' Returns the number of optimization results.
    #' @return
    #' An \code{integer}.

    runs = function(
      which_run = "all", which_direction = c("min", "max"),
      which_optimizer = "all", which_element = "all", verbose = self$verbose
    ) {
      if (private$.results$number() == 0) {
        return(0)
      }
      length(
        private$.get_run_ids(
          which_run = which_run,
          which_direction = which_direction,
          which_optimizer = which_optimizer,
          which_element = which_element,
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
      add_identifier = character(), verbose = self$verbose
    ) {

      ### check inputs
      which_run <- private$.check_which_run(
        which_run = which_run, verbose = verbose
      )
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = FALSE, verbose = FALSE
      )
      elements <- self$elements(
        which_optimizer = which_optimizer, group_by = NULL, verbose = verbose
      )
      which_element <- private$.check_which_element(
        which_element = which_element, choices = elements, several.ok = TRUE,
        verbose = verbose
      )
      add_identifier <- private$.check_add_identifier(
        add_identifier = add_identifier, several.ok = TRUE, none.ok = TRUE,
        verbose = verbose
      )

      ### get list of results and add `run_ids` identifier
      run_ids <- private$.get_run_ids(
        which_run = which_run, which_direction = which_direction,
        which_optimizer = which_optimizer, which_element = which_element,
        verbose = FALSE
      )
      results <- private$.results$get(ids = run_ids, id_names = TRUE)
      run_ids <- as.numeric(names(results))
      names(results) <- NULL
      for (i in seq_along(results)) {
        attr(results[[i]], ".run_id") <- run_ids[i]
      }

      ### filter not required elements out of results and add identifiers
      results <- lapply(results, function(result) {
        result[which(!names(result) %in% which_element)] <- NULL
        identifier <- attributes(result)[add_identifier]
        result <- append(result, identifier, after = 0)
      })

      ### return results
      return(results)

    },

    #' @description
    #' Provides an overview of the optimization runs.
    #' @return
    #' A \code{data.frame}.

    summary = function(
      which_element = c("value", "parameter"), which_run = "all",
      which_optimizer = "all", which_direction = c("min", "max"),
      add_identifier = character(), digits = self$digits, verbose = self$verbose
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

      ### try to unlist results in list form
      for (i in seq_len(ncol(out))) {
        unlist_try <- unlist(out[, i], recursive = FALSE)
        if (length(unlist_try) == nrow(out)) {
          out[, i] <- unlist(out[, i], recursive = FALSE)
        }
      }

      ### round numeric results
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
      which_run = "comparable", which_direction = "min",
      which_optimizer = "all", group_by = NULL, sort_by = "frequency",
      digits = self$digits, print.rows = 0, verbose = self$verbose
    ) {

      ### input checks
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = FALSE, verbose = FALSE
      )
      group_by <- private$.check_group_by(group_by = group_by, verbose = FALSE)
      sort_by <- oeli::match_arg(sort_by, c("frequency", "value"))
      checkmate::assert_count(digits)
      checkmate::assert_count(print.rows)

      ### access the values of the optimizations
      data <- self$summary(
        which_element = "value", which_run = which_run,
        which_direction = which_direction, which_optimizer = which_optimizer,
        digits = digits,
        add_identifier = if (is.null(group_by)) character() else group_by,
        verbose = verbose
      )

      ### group data
      if (is.null(group_by)) {
        data <- list(data)
      } else {
        data <- oeli::group_data_frame(
          df = data, by = group_by, keep_by = FALSE
        )
      }

      ### create optima tables
      optima <- list()
      for (i in seq_along(data)) {
        if (nrow(data[[i]]) == 0) {
          table <- data.frame()
        } else {
          values <- data[[i]][["value"]]
          table <- as.data.frame(table(values, useNA = "ifany"))
          colnames(table) <- c("value", "frequency")
          decreasing <- identical(sort_by, "frequency") ||
            identical(which_direction, "max")
          table <- table[order(table[[sort_by]], decreasing = decreasing), ]
          rownames(table) <- NULL
        }
        optima[[i]] <- table
      }
      names(optima) <- names(data)

      ### print and return tables
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

    plot = function(
      which_element = "seconds", group_by = NULL, relative = FALSE,
      which_run = "comparable", which_direction = c("min", "max"),
      which_optimizer = "all", ...
    ) {

      ### input checks
      which_element <- private$.check_which_element(
        which_element = which_element, choices = c("seconds", "value")
      )
      group_by <- private$.check_group_by(group_by = group_by)
      checkmate::assert_flag(relative)
      if (identical(which_element, "value") && relative) {
        cli::cli_warn(
          "Set {.var relative} to {.val FALSE}.",
          "Cannot be {.val TRUE} if {.var which_element} is {.val value}."
        )
        relative <- FALSE
      }

      ### prepare plot
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
    #' Visualizes deviation of parameters.
    #' @param reference
    #' A \code{numeric} of length \code{self$npar}, the reference parameters.
    #' @param which_element
    #' Either:
    #' - \code{"initial"} to compute deviations to the initial values (default)
    #' - \code{"parameter"} to compute deviations to the estimated parameters
    #' @param parameter_labels
    #' A \code{character} of length \code{length(reference)} with labels for the
    #' parameters.
    #' @return
    #' A \code{\link[ggplot2]{ggplot}} object.
    #' @importFrom ggplot2 ggplot aes geom_point scale_x_discrete
    #' @importFrom ggplot2 scale_y_continuous geom_hline coord_cartesian
    #' @importFrom reshape2 melt
    deviation = function(
      reference = self$true_parameter, which_element = "initial",
      which_run = "all", which_optimizer = "all", only_comparable = FALSE,
      title = "Parameter deviation", ylim = c(NA, NA),
      parameter_labels = paste0("theta", 1:self$npar)
    ) {
      data <- self$summary(
        which_run = which_run, which_element = c(which_element, "label")
      )
      data <- data.frame(
        "label" = data[["label"]],
        t(sapply(data[[which_element]], function(x) x - reference))
      ) |>
        reshape2::melt(id.vars = "label")
      data |> ggplot2::ggplot(ggplot2::aes(x = variable, y = value)) +
        ggplot2::geom_point(ggplot2::aes(color = label), position = "jitter") +
        ggplot2::scale_x_discrete(
          labels = parameter_labels,
          name = which_element
        ) +
        ggplot2::scale_y_continuous(
          name = "deviation"
        ) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::coord_cartesian(ylim = ylim)
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
      digits = self$digits, verbose = self$verbose
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
            private$.check_target(input)
            self$true(
              input = self$evaluate(at = input),
              which_element = "value", which_direction = "min"
            )
            private$.true_parameter_min <- input
            cli::cli_alert_info(
              "Set true minimum parameter vector to {round(private$.true_parameter_min, digits = digits)}."
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
              "Set true minimum function value to {round(private$.true_value_min, digits = digits)}."
            )
          }
        } else {
          if (identical(which_element, "parameter")) {
            private$.check_target(input)
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
    #' the first one of them is returned.
    #' @return
    #' A \code{numeric} (vector) with two attributes:
    #' - \code{.run_id}, the run id that led to the best value,
    #' - \code{.optimizer_label}, the optimizer that led to the best value.

    best = function(
      which_element = "value", which_run = c("success", "comparable"),
      which_direction = "min", which_optimizer = "all", digits = self$digits,
      verbose = self$verbose
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
    },

    #' @description
    #' Returns the parameter vector corresponding to an optimum closest
    #' (in absolute distance) to a reference \code{value}.
    #' @param value
    #' A single \code{numeric}, a reference for the value.
    #' @details
    #' In the case of ties, only one of the closest parameter vectors is
    #' returned.
    #' @return
    #' A \code{numeric} parameter \code{vector} with attributes defined by
    #' \code{add_identifier}.

    closest = function(
      value, which_run = "all", which_direction = c("min", "max"),
      which_optimizer = "all", add_identifier = c(".run_id", ".optimizer_label"),
      digits = self$digits
    ) {

      ### check inputs
      checkmate::assert_number(value, finite = TRUE)
      which_run <- private$.check_which_run(which_run = which_run)
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = TRUE
      )
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = FALSE
      )
      add_identifier <- private$.check_add_identifier(
        add_identifier = add_identifier, several.ok = TRUE, none.ok = TRUE
      )

      ### get values
      values <- self$summary(
        which_element = c("value", "parameter"), which_run = which_run,
        which_optimizer = which_optimizer, which_direction = which_direction,
        add_identifier = add_identifier, digits = Inf
      )

      ### find closest value
      row <- which.min(abs(values$value - value))
      out <- round(values[row, "parameter"][[1]], digits = digits)
      for (identifier in add_identifier) {
        attr(out, identifier) <- values[row, identifier]
      }
      return(out)

    }

  ),

  active = list(

    #' @field npar The length(s) of the target argument(s).
    npar = function(value) {
      if (missing(value)) {
        private$.objective$npar
      } else {
        cli::cli_abort(
          "The field {.var $npar} is read only.",
          call = NULL
        )
      }
    },

    #' @field fresh_label A new optimization label that has not been used yet.
    fresh_label = function(value) {
      if (missing(value)) {
        default_label <- "unlabeled"
        n <- 1
        while (TRUE) {
          label <- glue::glue("{default_label}_{n}")
          if (!label %in% private$.optimization_labels) {
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
    #' By default, \code{verbose = getOption("verbose", default = FALSE)}.
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
    #' By default, \code{digits = getOption("digits", default = 7)}.
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
    .verbose = getOption("verbose", default = FALSE),
    .digits = getOption("digits", default = 7),

    ### storage of the optimization problem details
    .objective = NULL,
    .true_parameter_min = NULL,
    .true_parameter_max = NULL,
    .true_value_min = NULL,
    .true_value_max = NULL,
    .original_arguments = list(),
    .optimizer = list(),
    .results = NULL,
    .optimization_labels = character(),

    ### storage of the initial values
    .initial_values = list(),
    .initial_type = character(),
    .initial_seconds = numeric(),

    ### check methods
    .check_target = function(at, verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_target(
        .at = at, verbose = verbose
      )
    },

    .check_arguments_complete = function(verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_arguments_complete(
        verbose = verbose
      )
    },

    ### optimization method
    .optimize = function(
      initial, optimizer_id, direction, seconds, hide_warnings
    ) {
      optimizer <- private$.optimizer[[optimizer_id]]
      optimizer$seconds <- seconds
      optimizer$hide_warnings <- hide_warnings
      result <- optimizer$.__enclos_env__$private$.optimize(
        objective = private$.objective, initial = initial,
        additional_arguments = list(), direction = direction
      )
      structure(
        result,
        ".optimizer_id" = optimizer_id,
        ".optimizer_label" = names(private$.optimizer)[optimizer_id],
        ".direction" = direction
      )
    },

    ### save optimization results
    .save_results = function(
      results, optimization_label, verbose = self$verbose
    ) {
      if (verbose) {
        cli::cli_alert_info(
          "Saving {length(results)} optimization result{?s}."
        )
      }
      comparable <- length(private$.original_arguments) == 0
      for (i in seq_along(results)) {
        result <- results[[i]]
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
          checkmate::assert_flag(result[["error"]])
        } else {
          result[["error"]] <- FALSE
        }
        if ("error_message" %in% parts) {
          checkmate::assert_string(result[["error_message"]], na.ok = TRUE)
        } else {
          result[["error_message"]] <- NA_character_
        }
        attr(result, ".optimization_label") <- optimization_label
        optimizer_id <- attr(result, ".optimizer_id")
        optimizer_label <- attr(result, ".optimizer_label")
        direction <- attr(result, ".direction")
        direction_identifier <- if (direction == "max") {
          c("direction:max", "!direction:min")
        } else {
          c("direction:min", "!direction:max")
        }
        identifier <- c(
          paste0("optimization_label:", optimization_label),
          paste0("optimizer_id:", optimizer_id),
          paste0("optimizer_label:", optimizer_label),
          paste0(ifelse(comparable, "", "!"), "comparable"),
          direction_identifier,
          paste0(ifelse(result[["error"]], "", "!"), "fail"),
          paste0("element:", names(result))
        )
        private$.results$add(
          x = result, identifier = identifier, confirm = FALSE
        )
        private$.optimization_labels <- unique(
          c(private$.optimization_labels, optimization_label)
        )
      }
    },

    ### check `which_run` filter
    .check_which_run = function(which_run, verbose = self$verbose) {

      ### input checks
      checkmate::assert_flag(verbose)

      ### check `which_run`
      if (length(which_run) == 0) {
        cli::cli_abort(
          "Argument {.var which_run} is misspecified.",
          call = NULL
        )
      }
      if (checkmate::test_character(which_run)) {

        if ("logical" %in% names(which_run)) {
          logical <- which_run[["logical"]]
          checkmate::assert_choice(logical, c("and", "or"))
          which_run <- which_run[-which(names(which_run) == "logical")]
        } else {
          logical <- "and"
        }

        if (length(which_run) == 0) {
          cli::cli_abort(
            "Argument {.var which_run} is misspecified.",
            call = NULL
          )
        }

        if ("all" %in% which_run) {

          if (length(which_run) == 1) {
            return("all")
          }
          if (logical == "and") {
            which_run <- which_run[-which(which_run == "all")]
          } else {
            which_run <- "all"
            if (verbose) {
              cli::cli_alert_info(
                "Selected all optimization runs."
              )
            }
            return(which_run)
          }

        }

        which_run <- unique(which_run)
        if (length(which_run) == 1) {

          if (verbose) {
            cli::cli_alert_info(
              "Selected optimization runs that fulfill the filter {.val {which_run}}."
            )
          }

        } else {

          which_run <- c(which_run, "logical" = logical)

          if (verbose) {
            if (logical == "and") {

              cli::cli_alert_info(
                "Selected optimization runs that fulfill all the filters {.val {which_run}}."
              )

            } else {

              cli::cli_alert_info(
                "Selected optimization runs that fulfill at least one of the filters {.val {which_run}}."
              )

            }
          }

        }

      } else if (checkmate::test_integerish(which_run, lower = 1)) {

        which_run <- unique(which_run)
        largest_id <- private$.results$number()

        if (any(which_run > largest_id)) {
          cli::cli_warn(
            "The largest available run id is {largest_id}.",
          )
          which_run <- which_run[-which(which_run > largest_id)]
        }

        if (verbose) {
          if (length(which_run) == 0) {
            cli::cli_abort("Argument {.var which_run} is misspecified.")
          } else {
            cli::cli_alert_info(
              "Selected optimization runs with id {which_run}."
            )
          }
        }

      } else {

        cli::cli_abort(
          "Argument {.var which_run} is misspecified.",
          call = NULL
        )

      }

      return(which_run)

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
          cli::cli_alert_info("Selected minimization.")
        }
        return("min")

      } else if (identical(which_direction, "max")) {

        if (verbose) {
          cli::cli_alert_info("Selected maximization.")
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
      which_optimizer, to_id, verbose = self$verbose
    ) {

      ### input checks
      checkmate::assert_flag(to_id)
      checkmate::assert_flag(verbose)

      ### check whether any optimizer is specified yet
      optimizer_ids <- seq_along(private$.optimizer)
      if (length(optimizer_ids) == 0) {
        cli::cli_warn(
          "No optimizer specified yet.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
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
          cli::cli_alert_info("Selected optimizer {optimizer_labels}.")
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
      which_element, choices, several.ok = TRUE, verbose = self$verbose,
      expand_all = TRUE
    ) {

      ### input checks
      checkmate::assert_names(choices)
      checkmate::assert_flag(several.ok)
      checkmate::assert_flag(verbose)
      checkmate::assert_flag(expand_all)

      ### check `choices`
      if (length(choices) == 0) {
        cli::cli_abort(
          "No elements available to select from. Did you already run optimizations?",
          call = NULL
        )
      }

      ### check `which_element`
      if ("all" %in% which_element) {

        if (expand_all) {
          which_element <- choices
        }
        if (verbose) {
          cli::cli_alert_info("Selected all available elements.")
        }

      } else {

        which_element <- oeli::match_arg(
          arg = which_element, choices = choices, several.ok = several.ok,
          none.ok = FALSE
        )
        if (verbose) {
          cli::cli_alert_info(
            "Selected element{?s} {.val {which_element}}."
          )
        }

      }
      return(which_element)

    },

    ### check `add_identifier` filter
    .check_add_identifier = function(
      add_identifier, several.ok, none.ok, verbose = self$verbose
    ) {

      ### input checks
      checkmate::assert_flag(several.ok)
      checkmate::assert_flag(none.ok)
      checkmate::assert_flag(verbose)

      ### check `add_identifier` input
      add_identifier <- oeli::match_arg(
        add_identifier, choices = c(
          ".run_id", ".optimization_label", ".optimizer_label", ".comparable",
          ".direction"
        ),
        several.ok = several.ok, none.ok = none.ok
      )
      if (verbose && length(add_identifier) > 0) {
        cli::cli_alert_info(
          "Selected identifier{?s} {.val {add_identifier}}."
        )
      }
      return(add_identifier)
    },

    .check_group_by = function(group_by, verbose = self$verbose) {
      checkmate::assert_choice(
        group_by, choices = c(".optimization_label", ".optimizer_label"),
        null.ok = TRUE
      )
      if (verbose && !is.null(group_by)) {
        cli::cli_alert_info("Selected grouping by {.val {group_by}}.")
      }
      return(group_by)
    },

    ### return run ids based on filters
    .get_run_ids = function(
      which_run, which_direction, which_optimizer, which_element,
      verbose = self$verbose
    ) {

      ### check inputs
      checkmate::assert_flag(verbose)
      which_run <- private$.check_which_run(which_run, verbose = FALSE)
      which_direction <- private$.check_which_direction(
        which_direction, both_allowed = TRUE, verbose = FALSE
      )
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer, to_id = FALSE, verbose = FALSE
      )
      which_element <- private$.check_which_element(
        which_element, choices = self$elements(
          which_optimizer = which_optimizer, group_by = NULL, verbose = FALSE
        ),
        verbose = FALSE, expand_all = FALSE
      )

      ### build identifier
      identifier <- character()
      ids <- integer()

      ### filter 'which_run'
      if (checkmate::test_character(which_run)) {

        if ("logical" %in% names(which_run)) {
          logical <- which_run[["logical"]]
          which_run <- which_run[-which(names(which_run) == "logical")]
        } else {
          logical <- "and"
        }

        for (filter in which_run) {

          if (identical(filter, "all")) {
            identifier <- c(identifier, "all")
          } else if (identical(filter, "success")) {
            identifier <- c(identifier, "!fail")
            if (verbose) {
              cli::cli_alert_info("Filtered for successful optimizations.")
            }
          } else if (identical(filter, "!success")) {
            identifier <- c(identifier, "fail")
            if (verbose) {
              cli::cli_alert_info("Filtered for failed optimizations.")
            }
          } else if (identical(filter, "comparable")) {
            identifier <- c(identifier, "comparable")
            if (verbose) {
              cli::cli_alert_info("Filtered for comparable optimizations.")
            }
          } else if (identical(filter, "!comparable")) {
            identifier <- c(identifier, "!comparable")
            if (verbose) {
              cli::cli_alert_info("Filtered for incomparable optimizations.")
            }
          } else {
            if (startsWith(filter, "!")) {
              filter <- sub("!*", "", filter)
              identifier <- c(identifier, paste0("!optimization_label:", filter))
              if (verbose) {
                cli::cli_alert_info(
                  "Filtered for optimizations that do not have the label {.val {filter}}."
                )
              }
            } else {
              identifier <- c(identifier, paste0("optimization_label:", filter))
              if (verbose) {
                cli::cli_alert_info(
                  "Filtered for optimizations that do have the label {.val {filter}}."
                )
              }
            }
          }

        }

        ids <- private$.results$indices(identifier = identifier, logical = logical)

      } else if (checkmate::test_integerish(which_run)) {

        ids <- c(ids, which_run)
        if (verbose) {
          cli::cli_alert_info("Filtered for run id {which_run}.")
        }

      }

      ### filter `which_direction`
      identifier <- character()
      if (checkmate::test_character(which_direction)) {

        if (all(c("min", "max") %in% which_direction)) {

          identifier <- c(identifier, "all")

        } else if (identical(which_direction, "min")) {

          identifier <- c(identifier, "direction:min")
          if (verbose) {
            cli::cli_alert_info("Filtered for minimization.")
          }

        } else if (identical(which_direction, "max")) {

          identifier <- c(identifier, "direction:max")
          if (verbose) {
            cli::cli_alert_info("Filtered for maximization.")
          }

        }

      }

      ### filter `which_optimizer`
      if (checkmate::test_character(which_optimizer)) {

        for (filter in which_optimizer) {

          if (identical(filter, "all")) {

            identifier <- c(identifier, "all")

          } else {

            if (filter %in% names(private$.optimizer)) {

              identifier <- c(identifier, paste0("optimizer_label:", filter))
              if (verbose) {
                cli::cli_alert_info(
                  "Filtered for optimizer label {.val {filter}}."
                )
              }

            } else {

              cli::cli_warn(
                "Optimizer label {.val {filter}} is unkown."
              )

            }

          }
        }

      } else if (checkmate::test_integerish(which_optimizer)) {

        identifier <- c(identifier, paste0("optimizer_id:", which_optimizer))
        if (verbose) {
          optimizer_labels <- names(private$.optimizer)[which_optimizer]
          cli::cli_alert_info("Filtered for optimizer {.val {optimizer_labels}}.")
        }

      }

      ### filter `which_element`
      if (checkmate::test_character(which_element)) {

        if ("all" %in% which_element) {

          identifier <- c(identifier, "all")

        } else {

          identifier <- c(identifier, paste0("element:", which_element))
          if (verbose) {
            cli::cli_alert_info(
              "Filtered for element{?s} {.val {which_element}}."
            )
          }
        }

      }

      ### return ids
      ids <- unique(
        intersect(
          private$.results$indices(identifier = identifier, logical = "and"),
          ids
        )
      )
      if (length(ids) == 0) {
        cli::cli_warn("No optimization results selected.")
        ids <- integer()
      }
      return(ids)
    }

  )
)

