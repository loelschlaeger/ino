#' Nop Object
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.
#'
#' @param which_optimizer
#' Selects numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels,
#' - a \code{numeric} (vector) of optimizer ids, see the \code{$print()} output.
#' @param which_run
#' Selects results of saved optimization runs. Either:
#' - \code{"all"} for all results,
#' - \code{"last"} for the results of the last \code{$optimize()} call,
#' - \code{"failed"}, the results from failed optimization runs,
#' - a \code{character} (vector) of optimization labels,
#' - an \code{integer} (vector) of run ids (as provided e.g. by \code{$best()}).
#' @param which_element
#' Selects elements of optimization results. Either:
#' - \code{"all"} for all available elements,
#' - \code{"default"}, the elements that are saved for all optimization runs by
#'   default, i.e.
#'   - \code{"value"}, the function value at the found optimum,
#'   - \code{"parameter"}, the parameter at which the optimum value is obtained,
#'   - \code{"seconds"}, the optimization time in seconds,
#'   - \code{"initial"}, the initial parameter,
#'   - \code{"error"}, indicating whether an error occurred,
#'   - \code{"error_message"}, the error message (if any).
#' - a \code{character} (vector) with names of specific elements (see
#'   \code{$elements()} for the names of available elements per optimizer).
#' @param which_direction
#' Selects the type of optimization. Either:
#' - \code{"min"} for minimization,
#' - \code{"max"} for maximization.
#' @param digits
#' An \code{integer}, the number of decimal places.
#' @param only_comparable
#' Either \code{TRUE} for comparable results only (i.e., results obtained
#' for the original optimization problem without any transformations),
#' or \code{FALSE} to include also non-comparable results.
#' @param verbose
#' Either \code{TRUE} to print progress and details, or \code{FALSE} to hide
#' such messages.
#' This can be defined globally via \code{options("ino_verbose" = TRUE)}.
#' @param seed
#' An \code{integer}, passed on to \code{\link{set.seed}} for reproducibility.
#' Can be \code{NULL} for no seed, which is the default.
#' @param return_results
#' Set to \code{TRUE} to return the optimization results as a \code{list}.
#' By default \code{return_results = FALSE}.
#' @param simplify
#' If \code{simplify = TRUE}, the nested list output of optimization results is
#' flattened if possible.
#' @param save_results
#' Set to \code{TRUE} to save the optimization results inside the \code{Nop}
#' object. These results can be analyzed via different methods, see the details.
#' By default, \code{save_results = TRUE}.
#' @param hide_warnings
#' Either \code{TRUE} or \code{FALSE} to hide (show) warning messages
#' during the function evaluation or optimization.
#' If \code{hide_warnings = FALSE}, warning messages are printed even if
#' \code{verbose = FALSE}.
#' @param time_limit
#' An \code{integer}, a time limit in seconds. Computations are interrupted
#' prematurely if \code{time_limit} is exceeded.
#' This currently only works reliably under Windows OS.
#' No time limit if \code{time_limit = NULL} (the default).
#' @param title
#' A \code{character}, the plot title.
#' @param xlim
#' Passed on to \code{\link[ggplot2]{coord_cartesian}}.
#' @param ylim
#' Passed on to \code{\link[ggplot2]{coord_cartesian}}.
#' @param add_identifier
#' A \code{character} (vector) of identifiers to be added to the output.
#' Can be one or more of
#' - \code{".run_id"}, an \code{integer} that identifies the optimization runs,
#' - \code{".optimization_label"}, a \code{character} that identifies custom
#'   groups of optimization runs,
#' - \code{".optimizer_id"}, an \code{integer} that identifies the optimizer,
#' - \code{".optimizer_label"}, a \code{character} that identifies the
#'   optimizer,
#' - \code{".comparable"}, \code{TRUE} or \code{FALSE}, identifies whether
#'   the result is comparable,
#' - \code{".direction"}, \code{"min"} or \code{"max"}, identifies whether
#'   the result corresponds to minimization or maximization.
#' @param group_by
#' Either:
#' - \code{".optimization_label"} to group by optimization label
#' - \code{".optimizer_label"} to group by optimizer label
#' - \code{NULL} to not group (default)
#'
#' @return
#' For the output of the different methods, please refer to their respective
#' documentation.
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
#' ## Step 2: Specify one or more numerical optimizers
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}, which can
#' be created with the \code{\link[optimizeR]{define_optimizer}} function from
#' the \{optimizeR\} package.
#' Two \code{optimizer} objects are already available:
#' - \code{\link[optimizeR]{optimizer_nlm}}
#' - \code{\link[optimizeR]{optimizer_optim}}
#'
#' ## Step 3: Initialize and optimize
#' Call one of the following methods to define starting values for the
#' optimization:
#' - \code{object$initialize_fixed()} for fixed initial values,
#' - \code{object$initialize_random()} for random initial values,
#' - \code{object$initialize_continue()} for initial values based on parameter
#'   estimates from previous optimization runs.
#'
#' The different initialization strategies are illustrated in the package
#' vignettes. Next, call \code{object$optimize()} for the optimization.
#'
#' # Methods for analyzing the results
#' The optimization results can be analyzed via the following methods:
#' - \code{$results()} returns a \code{list} of the saved optimization results,
#' - \code{$summary()} summarizes the results in a \code{data.frame},
#' - \code{$optima()} returns a frequency table of the identified optima,
#' - \code{$plot()} visualizes the optimization times or values,
#' - \code{$best()} returns the best found parameter vector or function value.
#'
#' # Other methods and fields
#' A \code{Nop} object also provides the following methods and fields for
#' convenience:
#' - \code{object$validate()} checks the configurations of a \code{Nop} object,
#' - \code{object$evaluate()} evaluates the target function at some point,
#' - \code{$delete()} deletes optimization results,
#' - \code{$elements()} returns the names of the available elements in the
#'   optimizer outputs,
#' - \code{$number()} returns the number of saved optimization results,
#' - \code{$deviation()} calculates and plots deviations of parameters to a
#'   reference parameter (e.g., to the true parameter vector
#'   \code{self$true_parameter}),
#' - \code{$trace()} calculates the trace of an optimization,
#' - \code{$initialize_reset()} resets the initial values,
#' - \code{$true()} stores the true best function value and parameter vector
#'   (if available).
#' - \code{$f_name} stores the function name,
#' - \code{$npar} stores the length of the target argument,
#' - \code{$fresh_label} generates a new label for the optimization.
#'
#' @examples
#' # Optimization of the Ackley function
#'
#' ackley <- Nop$new(f = f_ackley, npar = 2)$  # define the Nop object
#'   set_optimizer(optimizer_nlm())$           # set the nlm optimizer
#'   set_optimizer(optimizer_optim())$         # also set the optim optimizer
#'   initialize_random(
#'     sampler = function() rnorm(2, mean = 0, sd = 3),
#'     runs = 100, seed = 1
#'   )$                                        # initialize 100 times randomly
#'   optimize()                                # optimize
#'
#' ackley$optima(print.rows = 5)               # get overview of optima
#'
#' ackley$best("parameter")                    # get the best parameter vector
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Creates a new \code{Nop} object.
    #' @param f
    #' The \code{function} to be optimized.
    #' It should return a single \code{numeric} value and
    #' is optimized over its first argument (the target argument), which should
    #' be a \code{numeric} vector of length \code{npar}.
    #' @param npar
    #' An \code{integer}, the length of the first argument of \code{f} (i.e.,
    #' the first argument over which \code{f} is optimized).
    #' @param ...
    #' Optionally additional (i.e., in addition to the target argument)
    #' arguments for \code{f}. They must be named. They can be transformed via
    #' the \code{$argument()} method.
    #' @return
    #' A new \code{Nop} object.
    initialize = function(f, npar, ...) {
      if (missing(f)) {
        ino_stop("Please specify argument {.var f}.")
      }
      if (!is.function(f)) {
        ino_stop("Argument {.var f} must be a {.cls function}.")
      }
      if (is.null(formals(f))) {
        ino_stop("{.var f} must have at least one argument.")
      }
      if (missing(npar)) {
        ino_stop("Please specify argument {.var npar}.")
      }
      is_count(npar, error = TRUE, allow_zero = FALSE)
      private$.f <- f
      private$.f_inverse <- function(...) -f(...)
      f_name <- deparse(substitute(f))
      if (!is_name(f_name, error = FALSE)) {
        f_name <- "unnamed_function"
        ino_warn(
          "Function {.var f} is unnamed.",
          "You can set a name via {.var $f_name <- {.val name}}."
        )
      }
      private$.f_name <- f_name
      private$.f_target <- names(formals(f))[1]
      private$.npar <- as.integer(npar)
      if (length(list(...)) > 0) {
        self$argument("set", ...)
      }
    },

    #' @description
    #' Prints details of the numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.
    #' @importFrom crayon underline
    #' @importFrom glue glue
    #' @importFrom cli style_italic
    print = function(digits = getOption("digits", default = 7), ... ) {
      cat(
        glue::glue(
          crayon::underline("Optimization problem:"),
          "- Function: {self$f_name}",
          "- Optimize over: {private$.f_target} (length {self$npar})",
          .sep = "\n"
        ),
        "\n"
      )
      arguments <- private$.arguments
      if (length(arguments) > 0) {
        cat(
          glue::glue(
            "- Additional arguments: ",
            "{paste(names(arguments), collapse = ', ')}",
          ),
          "\n"
        )
      }
      true_parameter_min <- private$.true_parameter_min
      if (!is.null(true_parameter_min)) {
        cat(
          glue::glue(
            "- True minimum at: ",
            "{toString(round(true_parameter_min, digits = digits))}"
          ),
          "\n"
        )
      }
      true_value_min <- private$.true_value_min
      if (!is.null(true_value_min)) {
        cat(
          glue::glue(
            "- True minimum value: ",
            "{round(true_value_min, digits = digits)}"
          ),
          "\n"
        )
      }
      true_parameter_max <- private$.true_parameter_max
      if (!is.null(true_parameter_max)) {
        cat(
          glue::glue(
            "- True maximum at: ",
            "{toString(round(true_parameter_max, digits = digits))}"
          ),
          "\n"
        )
      }
      true_value_max <- private$.true_value_max
      if (!is.null(true_value_max)) {
        cat(
          glue::glue(
            "- True maximum value: ",
            "{round(true_value_max, digits = digits)}"
          ),
          "\n"
        )
      }
      cat(crayon::underline("Optimizer functions:\n"))
      optimizer <- private$.optimizer
      if (length(optimizer) == 0) {
        cat(
          cli::style_italic(
            "No optimizer specified yet.\n"
          )
        )
      } else {
        for (optimizer_id in seq_along(optimizer)) {
          optimizer_label <- names(optimizer)[optimizer_id]
          cat(glue::glue("- {optimizer_id}: {optimizer_label}"), "\n")
        }
      }
      cat(crayon::underline("Optimization runs:\n"))
      if (suppressWarnings(self$number(verbose = FALSE)) == 0) {
        cat(cli::style_italic("No results saved yet.\n"))
      } else {
        suppressWarnings({
          noptimizations <- self$number(verbose = FALSE)
          ncomparable <- self$number(only_comparable = TRUE, verbose = FALSE)
          nfailed <- self$number(which_run = "failed", verbose = FALSE)
        })
        cat(glue::glue(
          "- Total runs: {noptimizations}",
          "- Not comparable runs: {noptimizations - ncomparable}",
          "- Failed runs: {nfailed}",
          .sep = "\n"
        ), "\n")
        cat(crayon::underline("Optimization results:\n"))
        suppressWarnings({
          best_parameter_min <- self$best(
            which_element = "parameter", which_direction = "min",
            only_comparable = TRUE, digits = digits, verbose = FALSE
          )
          best_value_min <- self$best(
            which_element = "value", which_direction = "min",
            only_comparable = TRUE, digits = digits, verbose = FALSE
          )
          best_parameter_max <- self$best(
            which_element = "parameter", which_direction = "max",
            only_comparable = TRUE, digits = digits, verbose = FALSE
          )
          best_value_max <- self$best(
            which_element = "value", which_direction = "max",
            only_comparable = TRUE, digits = digits, verbose = FALSE
          )
        })
        if (!is.null(best_parameter_min) && !is.null(best_value_min)) {
          cat(glue::glue(
            "- Minimum parameter vector: {toString(best_parameter_min)}",
            "- Minimum function value: {best_value_min}",
            .sep = "\n"
          ), "\n")
        }
        if (!is.null(best_parameter_max) && !is.null(best_value_max)) {
          cat(glue::glue(
            "- Maximum parameter vector: {toString(best_parameter_max)}",
            "- Maximum function value: {best_value_max}",
            .sep = "\n"
          ), "\n")
        }
      }
      invisible(self)
    },

    #' @description
    #' Manages additional (i.e., in addition to the target argument) arguments
    #' for \code{f}.
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
    #'   - \code{how}, a \code{character}, specifying how to subset, either
    #'     - \code{"random"} (default), subset at random,
    #'     - \code{"first"}, subset to the first elements,
    #'     - \code{"last"}, subset to the last elements,
    #'     - \code{"similar"}, subset to similar elements,
    #'     - \code{"dissimilar"}, subset to dissimilar elements,
    #'     (the options \code{"similar"} and \code{"dissimilar"} apply k-means
    #'     clustering via \code{\link[stats]{kmeans}} and require that
    #'     the argument is \code{numeric}),
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
    #' @importFrom ellipsis check_dots_used
    argument = function(
      action, ..., verbose = getOption("ino_verbose", default = FALSE)
    ) {
      if (missing(action)) {
        ino_stop("Please specify argument {.var action}.")
      }
      is_name(action, allow_na = FALSE)
      if (!action %in% c(
        "set", "get", "remove", "reset", "modify", "subset", "standardize"
      )) {
        ino_stop(
          "Argument {.var action} should be one of {.val set}, {.val get},
          {.val remove}, {.val reset}, {.val modify}, {.val subset}, or
          {.val standardize}."
        )
      }
      is_TRUE_FALSE(verbose, allow_na = FALSE)
      args <- list(...)
      arg_names <- names(args)
      if (action == "set") {
        if (length(args) == 0) {
          ino_warn("No argument to set.")
        } else if (length(args) > 1) {
          for (i in 1:length(args)) {
            arg <- list("set", args[[i]])
            names(arg) <- c("action", arg_names[i])
            do.call(self$argument, arg)
          }
        } else {
          name <- arg_names[1]
          if (!is_name(name, error = FALSE)) {
            ino_stop(
              glue::glue(
                "All arguments to be set must be named."
              )
            )
          }
          if (name %in% names(private$.arguments)) {
            ino_stop(
              glue::glue(
                "Argument {.var <name>} already exists, ",
                "call {.var $argument(\"remove\", {.val <name>})} first.",
                .open = "<", .close = ">"
              )
            )
          }
          private$.arguments <- c(private$.arguments, args)
          ino_status(
            glue::glue(
              "Set argument {.var <name>}.",
              .open = "<", .close = ">"
            ),
            verbose = verbose
          )
        }
      }
      if (action == "get") {
        if (!"name" %in% arg_names) {
          ino_stop("Please specify {.var name}.")
        }
        name <- args[["name"]]
        is_name(name)
        private$.check_additional_argument_exists(name)
        return(private$.arguments[[name]])
      }
      if (action == "remove") {
        if (!"name" %in% arg_names) {
          ino_stop("Please specify {.var name}.")
        }
        name <- args[["name"]]
        is_name(name)
        private$.check_additional_argument_exists(name)
        arg_id <- which(names(private$.arguments) == name)
        private$.arguments[arg_id] <- NULL
        arg_id <- which(names(private$.original_arguments) == name)
        private$.original_arguments[arg_id] <- NULL
        ino_status(
          glue::glue(
            "Removed argument {.var <name>}.",
            .open = "<", .close = ">"
          ),
          verbose = verbose
        )
      }
      if (action == "reset") {
        if (!"name" %in% arg_names) {
          ino_stop("Please specify {.var name}.")
        }
        name <- args[["name"]]
        is_name(name)
        private$.check_additional_argument_exists(name)
        if (!is.null(private$.original_arguments[[name]])) {
          original_argument <- private$.original_arguments[[name]]
          private$.arguments[[name]] <- original_argument
          private$.original_arguments[[name]] <- NULL
          ino_status(
            glue::glue("Reset `{name}`."),
            verbose = verbose
          )
        } else {
          ino_warn("Nothing to reset.")
        }
      }
      if (action == "modify") {
        if (length(args) == 0) {
          ino_warn("No argument to modify.")
        } else if (length(args) > 1) {
          for (i in 1:length(args)) {
            arg <- list("modify", args[[i]])
            names(arg) <- c("action", arg_names[i])
            do.call(self$argument, arg)
          }
        } else {
          name <- arg_names[1]
          if (!is_name(name, error = FALSE)) {
            ino_stop(
              glue::glue(
                "All arguments to be modified must be named."
              )
            )
          }
          private$.check_additional_argument_exists(name)
          if (is.null(private$.original_arguments[[name]])) {
            private$.original_arguments[[name]] <- private$.arguments[[name]]
          }
          private$.arguments[[name]] <- args[[name]]
          ino_status(
            glue::glue(
              "Modified argument {.var <name>}.",
              .open = "<", .close = ">"
            ),
            verbose = verbose
          )
        }
      }
      if (action == "subset") {
        if (!"name" %in% arg_names) {
          ino_stop("Please specify {.var name}.")
        }
        name <- args[["name"]]
        is_name(name)
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
          ino_seed(seed = args[["seed"]], verbose = verbose)
        }
        subsetted_argument <- helper_subset(
          argument = original_argument, byrow = byrow, how = how,
          proportion = proportion, centers = centers, ignore = ignore
        )
        ino_status(
          glue::glue(
            "Reduced `{name}` from ",
            if (is.atomic(original_argument) &&
                is.null(dim(original_argument))) {
              length_old <- length(original_argument)
              length_new <- length(subsetted_argument)
              "{length_old} to {length_new} {how} element(s)."
            } else {
              if (byrow) {
                nrow_old <- nrow(original_argument)
                nrow_new <- nrow(subsetted_argument)
                glue::glue(
                  "{nrow_old} to {nrow_new} {how} row(s).",
                )
              } else {
                ncol_old <- ncol(original_argument)
                ncol_new <- ncol(subsetted_argument)
                glue::glue(
                  "{ncol_old} to {ncol_new} {how} column(s).",
                )
              }
            }
          ),
          verbose = verbose
        )
        private$.arguments[[name]] <- subsetted_argument
        if (is.null(private$.original_arguments[[name]])) {
          private$.original_arguments[[name]] <- original_argument
        }
      }
      if (action == "standardize") {
        if (!"name" %in% arg_names) {
          ino_stop("Please specify {.var name}.")
        }
        name <- args[["name"]]
        is_name(name)
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
        ino_status(
          glue::glue("Standardized `{name}`."),
          verbose = verbose
        )
      }
      invisible(self)
    },

    #' @description
    #' Specifies a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer}, which can be created via
    #' \code{\link[optimizeR]{define_optimizer}}.
    #' @param optimizer_label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
    #' @return
    #' Invisibly the \code{Nop} object.
    set_optimizer = function(optimizer, optimizer_label = NULL) {
      if (missing(optimizer)) {
        ino_stop("Please specify argument {.var optimizer}.")
      }
      if (!inherits(optimizer, "optimizer")) {
        ino_stop(
          "Argument {.var optimizer} must be an {.cls optimizer} object.",
          "See {.help optimizeR::define_optimizer} to create such an object."
        )
      }
      if (is.null(optimizer_label)) {
        optimizer_label <- optimizer$optimizer_name
      }
      is_name(optimizer_label)
      if (optimizer_label %in% names(private$.optimizer)) {
        ino_stop(
          glue::glue(
            "Label {.val <optimizer_label>} already exists, use another one.",
            .open = "<", .close = ">"
          )
        )
      }
      private$.optimizer[[optimizer_label]] <- optimizer
      invisible(self)
    },

    #' @description
    #' Evaluates the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}, the point where the
    #' function is evaluated.
    #' By default, \code{at = rnorm(self$npar)}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @return
    #' Either:
    #' - a \code{numeric} value, the function value at \code{at},
    #' - \code{"time limit reached"} if the time limit was reached,
    #' - the error message if the evaluation failed.
    evaluate = function(
      at = stats::rnorm(self$npar), time_limit = NULL, hide_warnings = FALSE
    ) {
      private$.check_additional_arguments_complete()
      private$.check_target_argument(at)
      is_time_limit(time_limit)
      is_TRUE_FALSE(hide_warnings)
      at <- list(at)
      names(at) <- private$.f_target
      if (!is.null(time_limit)) {
        setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
        on.exit({
          setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        })
      }
      tryCatch(
        {
          suppressWarnings(
            do.call(
              what = private$.f,
              args = c(at, private$.arguments)
            ),
            classes = if (hide_warnings) "warning" else ""
          )
        },
        error = function(e) {
          msg <- e$message
          tl <- grepl("reached elapsed time limit|reached CPU time limit", msg)
          if (tl) return("time limit reached") else return(msg)
        }
      )
    },

    #' @description
    #' Defines fixed initial values for the optimization.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}, the initial parameter
    #' vector. It can also be a \code{list} of such vectors.
    #' @param seconds
    #' Optionally a \code{numeric} (vector), TODO
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_fixed = function(
      at, verbose = getOption("ino_verbose", default = FALSE), seconds = NULL
    ) {
      is_TRUE_FALSE(verbose)
      if (is.list(at)) {
        for (i in seq_along(at)) {
          private$.check_target_argument(at[[i]], paste0("at[[", i, "]]"))
        }
      } else {
        private$.check_target_argument(at)
        at <- list(at)
      }
      private$.initial_values <- c(private$.initial_values, at)
      add_s <- ifelse(length(at) > 1, 's', '')
      ino_status(
        glue::glue("Added {length(at)} set{add_s} of fixed initial values."),
        verbose = verbose
      )
      invisible(self)
    },

    #' @description
    #' Defines random initial values for the optimization.
    #' @param sampler
    #' A \code{function} without any arguments that returns a \code{numeric}
    #' vector of length \code{npar}.
    #' By default, \code{sampler = rnorm(self$npar)}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' By default, \code{runs = 1}.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_random = function(
      sampler = function() stats::rnorm(self$npar), runs = 1, seed = NULL,
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      if (!is.function(sampler)) {
        ino_stop("Argument {.var sampler} must be a {.cls function}.")
      }
      if (!is.null(formals(sampler))) {
        ino_stop("{.var sampler} must not have any arguments.")
      }
      is_count(runs, allow_zero = FALSE)
      is_TRUE_FALSE(verbose, allow_na = FALSE)
      ino_seed(seed = seed, verbose = verbose)
      at <- list()
      for (run in seq_len(runs)) {
        value <- try(sampler(), silent = TRUE)
        private$.check_target_argument(value, "sampler()")
        at[[run]] <- value
      }
      private$.initial_values <- c(private$.initial_values, at)
      add_s <- ifelse(length(at) > 1, 's', '')
      ino_status(
        glue::glue("Added {length(at)} set{add_s} of random initial values."),
        verbose = verbose
      )
      invisible(self)
    },

    #' @description
    #' Defines random initial values based on the optimal parameters from
    #' previous optimization runs.
    #' @param transform
    #' A \code{function} for transforming the initial values.
    #' @param save_as
    #' A \code{character} that defines how to save TODO
    #' - \code{"new"}
    #' - \code{"replace"}
    #' - \code{"extend"}
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_continue = function(
      which_run = "last", which_optimizer = "all", which_direction = "min",
      transform = function(x) x, save_as = "extend",
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      is_TRUE_FALSE(verbose, allow_na = FALSE)
    },

    #' @description
    #' Resets the initialization.
    #' @return
    #' Invisibly the \code{Nop} object.
    initialize_reset = function(
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      private$.initial_values <- list()
      ino_status(
        "Reset the initial values.",
        verbose = verbose
      )
      invisible(self)
    },

    #' @description
    #' Optimizes the function.
    #' @param optimization_label
    #' Only relevant if \code{save_results = TRUE}.
    #' In this case, a \code{character} to specify a label for the optimization.
    #' Labels are useful to distinguish initialization strategies.
    #' By default, \code{optimization_label = self$fresh_label} creates a new
    #' label.
    #' @param ncores
    #' An \code{integer}, setting the number of CPU cores for parallel
    #' optimization runs.
    #' The default is \code{1}.
    #' You can use \code{parallel::detectCores()} to detect the number of
    #' available CPU cores.
    #' @param reset_initial
    #' Either \code{TRUE} (default) to reset the initial values after the
    #' optimization, or \code{FALSE}, else.
    #' @return
    #' The return value depends on the value of \code{return_results}:
    #' - if \code{return_results = FALSE}, invisibly the \code{Nop} object,
    #' - if \code{return_results = TRUE}, a nested \code{list} of
    #'   optimization results. Each element corresponds to one optimization run
    #'   and is a \code{list} of results for each optimizer. The results for
    #'   each optimizer is a \code{list}, the output of
    #'   \code{\link[optimizeR]{apply_optimizer}}. If \code{simplify = TRUE},
    #'   the output is flattened if possible.
    #' @importFrom parallel makeCluster stopCluster
    #' @importFrom doSNOW registerDoSNOW
    #' @importFrom foreach foreach %dopar% %do%
    optimize = function(
      which_optimizer = "all", which_direction = "min", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      optimization_label = self$fresh_label,
      ncores = 1, verbose = getOption("ino_verbose", default = FALSE),
      simplify = FALSE, time_limit = NULL, hide_warnings = TRUE,
      reset_initial = TRUE
    ) {
      private$.check_additional_arguments_complete()
      which_direction <- match_arg(which_direction, c("min", "max"))
      is_TRUE_FALSE(return_results, allow_na = FALSE)
      is_TRUE_FALSE(save_results, allow_na = FALSE)
      is_name(optimization_label, allow_na = FALSE)
      is_count(ncores, allow_zero = FALSE)
      is_TRUE_FALSE(verbose, allow_na = FALSE)
      is_TRUE_FALSE(simplify, allow_na = FALSE)
      is_time_limit(time_limit)
      is_TRUE_FALSE(hide_warnings, allow_na = FALSE)
      is_TRUE_FALSE(reset_initial, allow_na = FALSE)
      ino_seed(seed, verbose = verbose)
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (length(optimizer_ids) == 0) {
        return(invisible(self))
      }
      if (length(private$.initial_values) == 0) {
        self$initialize_random(seed = seed, verbose = verbose)
        ino_warn(
          "No initial values defined, so random initial values are used.",
          "Call {.fun $initialize_*} first to customize the initialization."
        )
      }
      runs <- length(private$.initial_values)
      format <- "Finished run :current of :total [elapsed :elapsed, to go :eta]"
      pb <- progress::progress_bar$new(
        format = format, total = runs, clear = FALSE, show_after = 0
      )
      opts <- structure(
        list(function(n) {
          if (verbose) if (pb$.__enclos_env__$private$total > 1) pb$tick()
        }),
        names = "progress"
      )
      if (ncores > 1 && runs < 2 * ncores) {
        if (interactive()) {
          parallel <- ino_ask(
            paste(
              "Few runs for", ncores, "cores, optimize in parallel anyways?"
            ),
            default = FALSE
          )
        } else {
          parallel <- FALSE
        }
      } else {
        parallel <- ncores > 1
      }
      if (parallel) {
        ino_status(
          glue::glue(
            "Optimization in parallel on {<ncores>} cores.",
            .open = "<", .close = ">"
          ),
          verbose = verbose
        )
        cluster <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cluster))
        doSNOW::registerDoSNOW(cluster)
      }
      `%par_seq%` <- ifelse(parallel, `%dopar%`, `%do%`)
      ino_status(
        glue::glue(
          "Optimization with label {.val <optimization_label>}.",
          .open = "<", .close = ">"
        ),
        verbose = verbose && save_results
      )
      if (verbose) pb$tick(0)
      results <- foreach::foreach(
        run = 1:runs, .packages = "ino",
        .export = "private", .inorder = TRUE, .options.snow = opts
      ) %par_seq% {
        if (verbose) pb$tick()
        lapply(optimizer_ids, function(optimizer_id) {
          private$.optimize(
            initial = private$.initial_values[[run]],
            optimizer_id = optimizer_id,
            which_direction = which_direction,
            time_limit = time_limit,
            hide_warnings = hide_warnings
          )
        })
      }
      if (save_results) {
        private$.last_runs_ids <- integer()
        ino_status("Reset the last run ids.", verbose = verbose)
        for (r in seq_len(runs)) {
          for (o in seq_along(optimizer_ids)) {
            private$.save_result(
              result = results[[r]][[o]],
              optimizer_id = o,
              optimization_label = optimization_label
            )
          }
        }
      }
      if (reset_initial) {
        self$initialize_reset(verbose = verbose)
      }
      if (return_results) {
        names(results) <- paste0("Run", seq_len(runs))
        for (i in seq_along(results)) {
          names(results[[i]]) <- paste0("Optimizer", seq_along(optimizer_ids))
        }
        if (simplify) {
          helper_flatten(results)
        } else {
          results
        }
      } else {
        invisible(self)
      }
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
      at = stats::rnorm(self$npar), which_optimizer = "all", time_limit = 10,
      verbose = getOption("ino_verbose", default = FALSE),
      digits = getOption("digits", default = 7)
    ) {
      private$.check_target_argument(at)
      optimizer_ids <- suppressWarnings(
        private$.get_optimizer_ids(which_optimizer = which_optimizer)
      )
      is_time_limit(time_limit)
      is_TRUE_FALSE(verbose)
      is_number(digits, allow_na = FALSE)
      ino_status("Check specifications:", verbose = verbose)
      ino_success(
        glue::glue("Function specified: {self$f_name}"),
        verbose = verbose
      )
      ino_success(
        glue::glue(
          "Target argument specified: {private$.f_target} (length {self$npar})"
        ),
        verbose = verbose
      )
      ino_success(
        glue::glue(
          "Test initial values specified: ",
          paste(round(at, digits = digits), collapse = " ")
        ),
        verbose = verbose
      )
      ino_status("Try to evaluate the function:", verbose = verbose)
      out <- self$evaluate(
        at = at, time_limit = time_limit, hide_warnings = TRUE
      )
      if (is.character(out)) {
        if (identical(out, "time limit reached")) {
          ino_warn(
            glue::glue(
              "Time limit of {time_limit}s reached in the function call."
            ),
            "Consider increasing {.var time_limit}."
          )
        } else {
          ino_stop(
            "Function call threw an error.",
            glue::glue("Message: {out}")
          )
        }
      } else {
        if (!is.numeric(out)) {
          ino_stop(
            "Test function call did not return a {.cls numeric} value."
          )
        } else {
          ino_success(
            "Test function call returned a {.cls numeric}.",
            verbose = verbose
          )
        }
        if (length(out) != 1) {
          ino_stop(
            glue::glue("Test function call is of length {length(out)}."),
            "It should be a single {.cls numeric} value."
          )
        } else {
          ino_success(
            glue::glue("Value: {round(out, digits = digits)}"),
            verbose = verbose
          )
        }
      }
      if (length(optimizer_ids) == 0) {
        ino_warn(
          "No optimizer specified, testing optimizers is skipped.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
        )
      } else {
        for (i in optimizer_ids) {
          ino_status(
            glue::glue(
              "Try to optimize with ",
              "`{paste(names(private$.optimizer)[i], collapse = ', ')}`:"
            ),
            verbose = verbose
          )
          out <- self$initialize_fixed(at, verbose = verbose)$
            optimize(
              which_optimizer = i, seed = NULL, return_results = TRUE,
              save_results = FALSE, ncores = 1, verbose = FALSE,
              simplify = TRUE, time_limit = time_limit, hide_warnings = TRUE
            )
          if (!is.null(out$error)) {
            if (isTRUE(out$error)) {
              if (identical(out$error_message, "time limit reached")) {
                ino_warn(
                  glue::glue(
                    "Time limit of {time_limit}s reached in the optimization."
                  ),
                  "Consider increasing {.var time_limit}."
                )
              } else {
                ino_stop(
                  "Optimization threw an error.",
                  glue::glue("Message: {out$error_message}")
                )
              }
            }
          } else {
            if (!is.list(out)) {
              ino_stop(
                "Test optimization did not return a {.cls list}."
              )
            } else {
              ino_success(
                "Test optimization returned a {.cls list}.",
                verbose = verbose
              )
              for (value in c("value", "parameter", "seconds")) {
                if (!value %in% names(out)) {
                  ino_stop(
                    glue::glue("Output does not contain the element '{value}'.")
                  )
                } else {
                  ino_success(
                    glue::glue(
                      "Optimization {value}: ",
                      "{paste(round(out[[value]], digits = digits), collapse = ' ')}"
                    ),
                    verbose = verbose
                  )
                  out[[value]] <- NULL
                }
              }
              if (length(out) > 0) {
                ino_status(
                  glue::glue(
                    "Additional elements: ",
                    "{paste(names(out), collapse = ' ')}"
                  ),
                  verbose = verbose
                )
              }
            }
          }
        }
      }
      invisible(TRUE)
    },

    #' @description
    #' Returns the names of all available elements by optimizers.
    #' @return
    #' A \code{list}.
    elements = function(which_optimizer = "all") {
      if (self$number(verbose = FALSE) == 0) {
        return(list())
      }
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      private$.get_element_names(
        which_element = "all", which_optimizer = which_optimizer,
        verbose = FALSE
      )
    },

    #' @description
    #' Returns the number of saved optimization results.
    #' @return
    #' An \code{integer}.
    number = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      which_direction = "min", only_comparable = FALSE,
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      length(
        private$.get_run_ids(
          which_run = which_run, which_optimizer = which_optimizer,
          which_element = which_element, which_direction = which_direction,
          only_comparable = only_comparable, verbose = verbose
        )
      )
    },

    #' @description
    #' Returns saved optimization results.
    #' @return
    #' A nested \code{list} of optimization results.
    #' Each element corresponds to one optimization run and is a \code{list}
    #' of results for each optimizer.
    #' If \code{simplify = TRUE}, the output is flattened if possible.
    results = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      which_direction = "min",
      only_comparable = FALSE, simplify = TRUE, add_identifier = character(),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      add_identifier <- match_arg(
        add_identifier, choices = c(
          ".run_id", ".optimization_label", ".optimizer_id", ".optimizer_label",
          ".comparable", ".direction"
        ),
        several.ok = TRUE, none.ok = TRUE
      )
      run_ids <- private$.get_run_ids(
        which_run = which_run, which_optimizer = which_optimizer,
        which_direction = which_direction,
        which_element = which_element, only_comparable = only_comparable,
        verbose = verbose
      )
      results <- private$.results[run_ids]
      elements <- suppressWarnings(unique(unlist(
        private$.get_element_names(
          which_element = which_element, which_optimizer = which_optimizer,
          verbose = FALSE
        )
      )))
      results <- lapply(results, function(result) {
        identifier <- attributes(result)[add_identifier]
        result[which(!names(result) %in% elements)] <- NULL
        result <- append(result, identifier, after = 0)
      })
      if (simplify) {
        helper_flatten(results)
      } else {
        results
      }
    },

    #' @description
    #' Provides an overview of the optimization runs.
    #' @return
    #' A \code{data.frame}.
    #' @importFrom dplyr bind_rows
    summary = function(
      which_element = c("value", "parameter"), which_run = "all",
      which_optimizer = "all", which_direction = "min",
      digits = getOption("digits", default = 7),
      only_comparable = FALSE, add_identifier = character(),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      out <- data.frame()
      results <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = which_element, which_direction = which_direction,
        only_comparable = only_comparable,
        simplify = FALSE, add_identifier = add_identifier, verbose = verbose
      )
      for (i in seq_along(results)) {
        append <- results[[i]]
        missing_results <- setdiff(
          unique(names(unlist(results, recursive = FALSE))),
          names(append)
        )
        append[missing_results] <- NA
        out <- rbind(out, rbind(append), make.row.names = FALSE)
      }
      for (i in seq_len(ncol(out))) {
        unlist_try <- unlist(out[, i], recursive = FALSE)
        if (length(unlist_try) == nrow(out)) {
          out[, i] <- unlist(out[, i], recursive = FALSE)
        }
      }
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
        ino_stop("Please specify {.var which_run}.")
      }
      is_TRUE_FALSE(prompt, allow_na = FALSE)
      is_TRUE_FALSE(replace, allow_na = FALSE)
      run_ids <- private$.get_run_ids(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = "all", which_direction = which_direction,
        only_comparable = FALSE, verbose = FALSE
      )
      if (length(run_ids) > 0) {
        if (prompt) {
          question <- glue::glue(
            "Do you really want to delete {length(run_ids)} results?"
          )
          if (!ino_ask(question, default = FALSE)) {
            return(invisible(self))
          }
        }
        private$.delete_results(run_ids = run_ids, replace = replace)
      }
      invisible(self)
    },

    #' @description
    #' Provides an overview of the identified optimum values.
    #' @param sort_by
    #' Either:
    #' - \code{"frequency"} (default) to sort rows by frequency,
    #' - \code{"value"} to sort by function value.
    #' @param print.rows
    #' An \code{integer}, specifying the maximal number of rows to be
    #' printed. No printing if \code{print.rows = 0}, which is the default.
    #' @return
    #' A \code{data.frame}.
    optima = function(
      digits = getOption("digits", default = 7), sort_by = "frequency",
      which_run = "all", which_optimizer = "all", which_direction = "min",
      only_comparable = TRUE, print.rows = 0
    ) {
      is_count(digits, allow_zero = TRUE)
      is_count(print.rows, allow_zero = TRUE)
      sort_by <- match_arg(sort_by, choices = c("frequency", "value"))
      values <- self$summary(
        which_element = "value", which_run = which_run,
        which_optimizer = which_optimizer, which_direction = which_direction,
        digits = digits, only_comparable = only_comparable
      )
      if (length(values) == 0) {
        return(data.frame())
      }
      optima <- as.data.frame(table(values, useNA = "ifany"))
      colnames(optima) <- c("value", "frequency")
      decreasing <- identical(sort_by, "frequency") || identical(which_direction, "max")
      optima <- optima[order(optima[[sort_by]], decreasing = decreasing), ]
      rownames(optima) <- NULL
      if (print.rows == 0) {
        return(optima)
      } else {
        print.rows <- min(nrow(optima), print.rows)
        print(optima[seq_len(print.rows), ])
        if (print.rows < nrow(optima)) {
          cli::cli_alert_info(
            "Omitted {nrow(optima) - print.rows} row{?s}."
          )
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
      which_run = "all", which_optimizer = "all", which_direction = "min",
      only_comparable = FALSE,
      title = paste0(which_direction, "imization of ", self$f_name),
      xlim = c(NA, NA), ...
    ) {
      which_element <- match_arg(which_element, c("seconds", "value"))
      if (!is.null(group_by)) {
        group_by <- match_arg(
          group_by, c(".optimization_label", ".optimizer_label")
        )
      }
      is_TRUE_FALSE(relative)
      if (identical(which_element, "value") && relative) {
        ino_warn(
          "Set {.var relative} to {.val FALSE}.",
          "Cannot be {.val TRUE} if {.var which_element} is {.val value}."
        )
        relative <- FALSE
      }
      is_name(title)
      if (!(is.vector(xlim) && length(xlim) == 2)) {
        ino_stop(
          "Argument {.var xlim} must be {.cls numeric} vector of length 2.",
          "Entries can also be {.val NA}."
        )
      }
      data <- self$summary(
        which_element = which_element, which_run = which_run,
        which_optimizer = which_optimizer, which_direction = which_direction,
        only_comparable = only_comparable, digits = Inf,
        add_identifier = c(".optimization_label", ".optimizer_label")
      )
      incomplete_cases <- which(!stats::complete.cases(data))
      if (length(incomplete_cases) > 0) {
        ino_status(
          paste(
            "Dropped", length(incomplete_cases), "rows with missing data."
          )
        )
        data <- data[-incomplete_cases, , drop = FALSE]
      }
      if (nrow(data) == 0) {
        ino_status(
          "No data to plot."
        )
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
      if (!is.null(title)) {
        base_plot <- base_plot + ggplot2::ggtitle(label = title)
      }
      base_plot + ggplot2::coord_cartesian(xlim = xlim)
    },

    #' @description
    #' Calculates and visualizes deviations of parameters.
    #' @param reference
    #' A \code{numeric} of length \code{self$npar}, the reference parameters.
    #' By default, the true parameter vector is used (if available).
    #' @param which_element
    #' Either:
    #' - \code{"initial"} to compute deviations to the initial values (default)
    #' - \code{"parameter"} to compute deviations to the estimated parameters
    #' @param plot
    #' Either \code{TRUE} (default) to produce a plot of the deviations, or
    #' \code{FALSE} to return the values.
    #' @return
    #' Either a \code{data.frame} if \code{plot = FALSE}, otherwise a
    #' \code{\link[ggplot2]{ggplot}} object.
    #' @importFrom ggplot2 ggplot aes geom_point scale_x_discrete
    #' @importFrom ggplot2 scale_y_continuous geom_hline coord_cartesian
    #' @importFrom reshape2 melt
    deviation = function(
      reference = self$true(
        which_element = "parameter", which_direction = "min"
      ),
      which_element = "initial", which_run = "all", which_optimizer = "all",
      which_direction = "min", only_comparable = FALSE, group_by = NULL,
      plot = TRUE, title = "Parameter deviation", ylim = c(NA, NA)
    ) {
      private$.check_target_argument(reference)
      which_element <- match_arg(which_element, c("initial", "parameter"))
      if (!is.null(group_by)) {
        group_by <- match_arg(
          group_by, c(".optimization_label", ".optimizer_label")
        )
      }
      is_TRUE_FALSE(plot)
      is_name(title)
      data <- self$summary(
        which_element = which_element, which_run = which_run,
        which_optimizer = which_optimizer, digits = Inf,
        only_comparable = only_comparable,
        add_identifier = c(".run_id", group_by)
      )
      deviations <- t(sapply(data[[which_element]], function(x) x - reference))
      parameter_labels <- paste0("p", 1:self$npar)
      colnames(deviations) <- parameter_labels
      if (!is.null(group_by)) {
        deviations <- cbind(data[group_by], deviations)
      }
      if (!plot) {
        return(as.data.frame(deviations))
      }
      deviations <- cbind(data[".run_id"], deviations)
      deviations <- reshape2::melt(deviations, id.vars = c(".run_id", group_by))
      if (!is.null(group_by)) {
        colnames(deviations)[2] <- "label"
      }
      ggplot2::ggplot(deviations, ggplot2::aes(x = variable, y = value)) +
        ggplot2::geom_jitter(
          mapping = if (!is.null(group_by)) {
            ggplot2::aes(color = label)
          } else {
            NULL
          },
          height = 0
        ) +
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
    #' A \code{numeric}, the minimum allowable absolute change in the
    #' function value before termination.
    #' By default, \code{tolerance = 1e-6}.
    #' @param which_element
    #' A \code{character} (vector) of elements to provide in the output, can be
    #' one or more of:
    #' - \code{value} (the current function value)
    #' - \code{parameter} (the current value of each parameter)
    #' - \code{gradient} (the current gradient value)
    #' - \code{hessian} (the current Hessian value)
    #' - \code{seconds} (the number of seconds for the current iteration)
    #' @param ...
    #' Additional arguments passed on to \code{\link[stats]{nlm}}.
    #' The arguments \code{iterlim} and \code{hessian} cannot be specified.
    #' @return
    #' A \code{data.frame} with iterations in rows, the columns depend on the
    #' specification of \code{which_element}.
    #' @importFrom stats rnorm nlm
    #' @importFrom optimizeR optimizer_nlm apply_optimizer
    trace = function(
      initial = stats::rnorm(self$npar), iterations = 100, tolerance = 1e-6,
      which_element = c("value", "parameter", "gradient", "hessian", "seconds"),
      seed = NULL, ...
    ) {
      is_count(iterations)
      is_number(tolerance)
      which_element <- match_arg(
        arg = which_element,
        choices = c("value", "parameter", "gradient", "hessian", "seconds"),
        several.ok = TRUE, none.ok = FALSE
      )
      args <- list(...)
      args[["iterlim"]] <- 1
      args[["hessian"]] <- "hessian" %in% which_element
      nlm_opt <- do.call(optimizer_nlm, args)
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
      current_value <- self$evaluate(at = initial)
      current_initial <- initial
      for (i in 1:iterations) {
        step <- do.call(
          what = optimizeR::apply_optimizer,
          args = c(
            list(
              "optimizer" = nlm_opt,
              "objective" = private$.f,
              "initial" = current_initial
            ),
            private$.arguments
          )
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
      which_element <- match_arg(which_element, c("value", "parameter"))
      which_direction <- match_arg(which_direction, c("min", "max"))
      is_number(digits, allow_na = FALSE)
      is_TRUE_FALSE(verbose, allow_na = FALSE)
      if (is.null(input)) {
        if (identical(which_direction, "min")) {
          if (identical(which_element, "parameter")) {
            out <- private$.true_parameter_min
            if (is.null(out)) {
              ino_warn(
                "The true minimum parameter vector has not been specified yet."
              )
            }
          } else {
            out <- private$.true_value_min
            if (is.null(out)) {
              ino_warn(
                "The true minimum function value has not been specified yet."
              )
            }
          }
        } else {
          if (identical(which_element, "parameter")) {
            out <- private$.true_parameter_max
            if (is.null(out)) {
              ino_warn(
                "The true maximum parameter vector has not been specified yet."
              )
            }
          } else {
            out <- private$.true_value_max
            if (is.null(out)) {
              ino_warn(
                "The true maximum function value has not been specified yet."
              )
            }
          }
        }
        return(out)
      } else if (all(is.na(input))) {
        if (identical(which_direction, "min")) {
          if (identical(which_element, "parameter")) {
            private$.true_parameter_min <- NULL
            ino_status(
              "Removed true minimum parameter vector.",
              verbose = verbose
            )
          } else {
            private$.true_value_min <- NULL
            private$.true_parameter_min <- NULL
            ino_status(
              "Removed true minimum function value and parameter vector.",
              verbose = verbose
            )
          }
        } else {
          if (identical(which_element, "parameter")) {
            private$.true_parameter_max <- NULL
            ino_status(
              "Removed true maximum parameter vector.",
              verbose = verbose
            )
          } else {
            private$.true_value_max <- NULL
            private$.true_parameter_max <- NULL
            ino_status(
              "Removed true maximum function value and parameter vector.",
              verbose = verbose
            )
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
            ino_status(
              glue::glue(
                "Set true minimum parameter vector to",
                "{toString(round(private$.true_parameter_min, digits = digits))}.",
                .sep = " "
              ),
              verbose = verbose
            )
          } else {
            is_number(input, allow_na = FALSE)
            if (!is.null(private$.true_parameter_min)) {
              true_value_old <- self$evaluate(at = private$.true_parameter_min)
              if (input != true_value_old) {
                ino_stop(
                  "Update or remove the true minimum parameter vector first."
                )
              }
            }
            private$.true_value_min <- input
            ino_status(
              glue::glue(
                "Set true minimum function value to",
                "{round(private$.true_value_min, digits = digits)}.",
                .sep = " "
              ),
              verbose = verbose
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
            ino_status(
              glue::glue(
                "Set true maximum parameter vector to",
                "{toString(round(private$.true_parameter_max, digits = digits))}.",
                .sep = " "
              ),
              verbose = verbose
            )
          } else {
            is_number(input, allow_na = FALSE)
            if (!is.null(private$.true_parameter_max)) {
              true_value_old <- self$evaluate(at = private$.true_parameter_max)
              if (input != true_value_old) {
                ino_stop(
                  "Update or remove the true maximum parameter vector first."
                )
              }
            }
            private$.true_value_max <- input
            ino_status(
              glue::glue(
                "Set true maximum function value to (rounded)",
                "{round(private$.true_value_max, digits = digits)}.",
                .sep = " "
              ),
              verbose = verbose
            )
          }
        }
        return(invisible(self))
      }
    },

    #' @description
    #' Returns the best found function value or parameter vector.
    #' @details
    #' In the case that multiple optimization runs led to the best value, only
    #' the first one of them is considered.
    #' A \code{numeric} (vector) with two attributes:
    #' - \code{.run_id}, the run id that led to the best value,
    #' - \code{.optimizer_label}, the optimizer that led to the best value.
    best = function(
      which_element = "value", which_run = "all", which_optimizer = "all",
      which_direction = "min", only_comparable = TRUE,
      digits = getOption("digits", default = 7),
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      which_element <- match_arg(which_element, c("value", "parameter"))
      data <- self$summary(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = c("value", "parameter"),
        which_direction = which_direction,
        only_comparable = only_comparable, digits = Inf,
        add_identifier = c(".run_id", ".optimizer_label"),
        verbose = verbose
      )
      if (nrow(data) == 0) {
        return(invisible(NULL))
      }
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

    #' @field f_name A \code{character}, the name of the function \code{f}.
    f_name = function(value) {
      if (missing(value)) {
        private$.f_name
      } else {
        if (!is_name(value, error = FALSE)) {
          ino_stop("{.var $f_name} must be a single {.cls character}.")
        } else {
          private$.f_name <- value
        }
      }
    },

    #' @field npar An \code{integer}, the length of the target argument (i.e.,
    #' the argument over which \code{f} is optimized).
    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop("{.var $npar} is read only.")
      }
    },

    #' @field fresh_label A \code{character}, an optimization label that has
    #' not been used yet.
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
        ino_stop("{.var $fresh_label} is read only.")
      }
    }

  ),

  private = list(

    ### definition of the optimization problem
    .f = NULL,
    .f_inverse = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .true_parameter_min = NULL,
    .true_parameter_max = NULL,
    .true_value_min = NULL,
    .true_value_max = NULL,
    .arguments = list(),
    .original_arguments = list(),
    .optimizer = list(),
    .initial_values = list(),

    ### checks for the optimization problem
    .check_additional_argument_exists = function(name) {
      stopifnot(is_name(name))
      if (!name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue(
            "Function argument {.var <name>} is not yet specified.",
            .open = "<", .close = ">"
          ),
          glue::glue(
            "Call {.var $argument(\"set\", {.val <name>} = ...)} first.",
            .open = "<", .close = ">"
          )
        )
      }
      TRUE
    },
    .check_additional_arguments_complete = function() {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        if (!all(nzchar(args_all[[arg]])) && is.name(args_all[[arg]])) {
          private$.check_additional_argument_exists(arg)
        }
      }
      TRUE
    },
    .check_target_argument = function(target_arg, arg_name = NULL) {
      if (is.null(arg_name)) {
        arg_name <- deparse(substitute(target_arg))
      } else {
        is_name(arg_name, allow_na = FALSE)
      }
      if (!is.vector(target_arg) || !is.numeric(target_arg)) {
        ino_stop(
          glue::glue(
            "Input {.var <arg_name>} must be a {.cls numeric}.",
            .open = "<", .close = ">"
          )
        )
      }
      if (length(target_arg) != self$npar) {
        ino_stop(
          glue::glue(
            "Input {.var <arg_name>} must be of length {<self$npar>}.",
            .open = "<", .close = ">"
          )
        )
      }
      TRUE
    },

    ### storage of optimization results and ids
    .results = list(),
    .next_run_id = function() {
      length(private$.results) + 1
    },
    .result_comparable = function() {
      length(private$.original_arguments) == 0
    },
    .optimizer_id_ids = list(),
    .optimization_label_ids = list(),
    .comparable_ids = integer(),
    .direction_ids = list("min" = integer(), "max" = integer()),
    .failed_ids = integer(),
    .last_runs_ids = integer(),
    .element_ids = list(),
    .save_result = function(result, optimizer_id, optimization_label) {
      run_id <- private$.next_run_id()
      parts <- names(result)
      if ("value" %in% parts) {
        is_number(result[["value"]], allow_na = TRUE)
      } else {
        result[["value"]] <- NA_real_
      }
      if ("parameter" %in% parts) {
        is_number_vector(result[["parameter"]], allow_na = TRUE)
      } else {
        result[["parameter"]] <- NA_real_
      }
      if ("seconds" %in% parts) {
        is_time(result[["seconds"]], allow_na = TRUE)
      } else {
        result[["seconds"]] <- NA_real_
      }
      if ("initial" %in% parts) {
        is_number_vector(result[["initial"]], allow_na = TRUE)
      } else {
        result[["initial"]] <- NA_real_
      }
      if ("error" %in% parts) {
        is_TRUE_FALSE(result[["error"]], allow_na = TRUE)
      } else {
        result[["error"]] <- FALSE
      }
      error <- result[["error"]]
      if ("error_message" %in% parts) {
        is_name(result[["error_message"]], allow_na = TRUE)
      } else {
        result[["error_message"]] <- NA_character_
      }
      attr(result, ".run_id") <- run_id
      attr(result, ".optimization_label") <- optimization_label
      attr(result, ".optimizer_id") <- optimizer_id
      optimizer_label <- names(private$.optimizer)[optimizer_id]
      attr(result, ".optimizer_label") <- optimizer_label
      comparable <- private$.result_comparable()
      attr(result, ".comparable") <- comparable
      direction <- private$.optimizer[[optimizer_id]][["optimizer_direction"]]
      attr(result, ".direction") <- direction
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
    },
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
        ino_status(
          "Result with id {run_id} is removed.",
          verbose = getOption("ino_verbose", default = FALSE)
        )
      }
    },

    ### filter for results, optimizers, and elements
    .get_run_ids = function(
      which_run, which_optimizer, which_element, which_direction,
      only_comparable, verbose = getOption("ino_verbose", default = FALSE)
    ) {
      if (length(private$.results) == 0) {
        ino_warn(
          "No optimization results saved yet.",
          "Please call {.var $optimize(save_results = TRUE)} first."
        )
        return(integer())
      }
      direction_ids <- private$.get_direction_ids(
        which_direction = which_direction, verbose = verbose
      )
      run_ids <- if (is_name(which_run, allow_na = FALSE, error = FALSE)) {
        if (identical(which_run, "all")) {
          ino_status(
            "Selected all saved results.",
            verbose = verbose
          )
          seq_along(private$.results)
        } else if (identical(which_run, "last")) {
          ino_status(
            "Selected results of the last {.fun $optimize} call.",
            verbose = verbose
          )
          private$.last_runs_ids
        } else if (identical(which_run, "failed")) {
          ino_status(
            "Selected results from failed optimization runs.",
            verbose = verbose
          )
          private$.failed_runs_ids
        } else if (which_run %in% self$optimization_labels) {
          ino_status(
            "Selected results by label.",
            verbose = verbose
          )
          private$.optimization_label_ids[[which_run]]
        } else {
          integer()
        }
      } else if (is_index_vector(which_run, error = FALSE)) {
        ino_status(
          "Selected results by id.",
          verbose = verbose
        )
        which(seq_along(private$.results) %in% which_run)
      } else {
        ino_stop(
          "Argument {.var which_run} is misspecified."
        )
      }
      run_ids <- intersect(
        run_ids, direction_ids
      )
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer, verbose = verbose
      )
      run_ids <- intersect(
        run_ids,
        unlist(private$.optimizer_id_ids[optimizer_ids])
      )
      which_element <- private$.get_element_names(
        which_element = which_element, which_optimizer = which_optimizer,
        verbose = verbose
      )
      run_ids <- intersect(
        run_ids,
        unique(unlist(private$.element_ids[unlist(which_element)]))
      )
      is_TRUE_FALSE(only_comparable, allow_na = FALSE)
      if (only_comparable) {
        ino_status(
          "Selected only comparable results.",
          verbose = verbose
        )
        run_ids <- intersect(
          run_ids,
          private$.comparable_ids
        )
      }
      if (length(run_ids) == 0) {
        ino_warn("Your filter selects no saved result.")
        return(integer())
      }
      return(run_ids)
    },
    .get_optimizer_ids = function(
      which_optimizer, verbose = getOption("ino_verbose", default = FALSE)
    ) {
      ids <- seq_along(private$.optimizer)
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer specified yet.",
          "Use {.fun $set_optimizer} to specify an optimizer."
        )
        return(integer(0))
      }
      if (identical(which_optimizer, "all")) {
        ino_status(
          "Selected all specified optimizers.",
          verbose = verbose
        )
        return(ids)
      } else if (is_name_vector(which_optimizer, error = FALSE)) {
        ino_status(
          "Selected optimizers by label.",
          verbose = verbose
        )
        ids <- which(names(private$.optimizer) %in% which_optimizer)
      } else if (is_number_vector(which_optimizer, error = FALSE)) {
        ino_status(
          "Selected optimizers by id.",
          verbose = verbose
        )
        ids <- which(seq_along(private$.optimizer) %in% which_optimizer)
      } else {
        ino_stop(
          "Argument {.var which_optimizer} is misspecified."
        )
      }
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer selected."
        )
        return(integer(0))
      }
      return(ids)
    },
    .get_direction_ids = function(
      which_direction, verbose = getOption("ino_verbose", default = FALSE)
    ) {
      which_direction <- match_arg(which_direction, c("min", "max"))
      ino_status(
        "Selected {which_direction}imizations.",
        verbose = verbose
      )
      private$.direction_ids[[which_direction]]
    },
    .get_element_names = function(
      which_element = "all", which_optimizer = "all",
      verbose = getOption("ino_verbose", default = FALSE)
    ) {
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer, verbose = FALSE
      )
      all_elements <- list()
      for (optimizer_id in optimizer_ids) {
        optimizer_label <- names(private$.optimizer)[optimizer_id]
        if (
          length(private$.optimizer_id_ids) < optimizer_id ||
          is.null(private$.optimizer_id_ids[[optimizer_id]])
        ) {
          ino_warn(
            glue::glue(
              "No results for optimizer {.var <optimizer_label>} saved yet.",
              .open = "<", .close = ">"
            )
          )
          all_elements[[optimizer_label]] <- character()
        } else {
          all_elements[[optimizer_label]] <- unique(names(unlist(
            private$.results[private$.optimizer_id_ids[[optimizer_id]]],
            recursive = FALSE
          )))
        }
      }
      if (identical(which_element, "all")) {
        ino_status(
          "Selected all available elements.",
          verbose = verbose
        )
        elements <- all_elements
      } else if (identical(which_element, "default")) {
        ino_status(
          "Selected default elements.",
          verbose = verbose
        )
        elements <- lapply(elements, intersect, c(
          "value", "parameter", "seconds", "initial", "error", "error_message"
        ))
      } else if (is_name_vector(which_element, allow_na = FALSE, error = FALSE)) {
        ino_status(
          "Selected specific elements.",
          verbose = verbose
        )
        elements <- list()
        for (optimizer_id in optimizer_ids) {
          optimizer_label <- names(private$.optimizer)[optimizer_id]
          unavailable <- setdiff(which_element, all_elements[[optimizer_label]])
          if (length(unavailable) > 0) {
            ino_warn(
              glue::glue(
                "Elements not available for {.var <optimizer_label>}:",
                .open = "<", .close = ">"
              ),
              glue::glue(
                "{unavailable}"
              )
            )
          }
          elements[[optimizer_label]] <- setdiff(which_element, unavailable)
        }
      } else {
        ino_stop(
          "Input {.var which_element} is misspecified.",
          "It should be {.val all} or {.val default}.",
          "It can also be a {.cls character} vector of specific element names."
        )
      }
      return(elements)
    },

    ### cheap function optimization
    .optimize = function(
      initial, optimizer_id, which_direction, time_limit, hide_warnings
    ) {
      if (!is.null(time_limit)) {
        setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
        on.exit({
          setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        })
      }
      optimizer <- private$.optimizer[[optimizer_id]]
      invert_objective <- !identical(
        optimizer[["optimizer_direction"]], which_direction
      )
      objective <- if (invert_objective) private$.f_inverse else private$.f
      tryCatch(
        {
          result <- suppressWarnings(
            do.call(
              what = optimizeR::apply_optimizer,
              args = c(
                list(
                  "optimizer" = optimizer,
                  "objective" = objective,
                  "initial" = initial
                ),
                private$.arguments
              )
            ),
            classes = if (hide_warnings) "warning" else ""
          )
          if (invert_objective) {
            result[["value"]] <- -result[["value"]]
          }
          result
        },
        error = function(e) {
          error_message <- e$message
          time_limit_reached <- grepl(
            "reached elapsed time limit|reached CPU time limit", error_message
          )
          if (time_limit_reached) {
            error_message <- "time limit reached"
          }
          if (!hide_warnings) {
            ino_warn(
              "Optimization failed with message: {error_message}"
            )
          }
          return(
            list(
              "initial" = initial,
              "error" = TRUE,
              "error_message" = error_message
            )
          )
        }
      )
    }

  )
)

#' @noRd
#' @exportS3Method

print.Nop <- function(x, digits = getOption("digits", default = 7), ...) {
  x$print(digits = digits, ...)
}

#' @noRd
#' @exportS3Method

summary.Nop <- function(
    object, which_element = c("value", "parameter"), which_run = "all",
    which_optimizer = "all", which_direction = "min",
    digits = getOption("digits", default = 7),
    only_comparable = FALSE, add_identifier = character(),
    verbose = getOption("ino_verbose", default = FALSE), ...
) {
  object$summary(
    which_element = which_element, which_run = which_run,
    which_optimizer = which_optimizer, which_direction = which_direction,
    digits = digits, only_comparable = only_comparable,
    add_identifier = add_identifier, verbose = verbose, ...
  )
}

#' @noRd
#' @exportS3Method

plot.Nop <- function(
    x, which_element = "seconds", group_by = NULL, relative = FALSE,
    which_run = "all", which_optimizer = "all", which_direction = "min",
    only_comparable = FALSE,
    title = paste0(which_direction, "imization of ", x$f_name),
    xlim = c(NA, NA), ...
  ) {
  x$plot(
    which_element = which_element, group_by = group_by, relative = relative,
    which_run = which_run, which_optimizer = which_optimizer,
    which_direction = which_direction, only_comparable = only_comparable,
    title = title, xlim = xlim, ...
  )
}

