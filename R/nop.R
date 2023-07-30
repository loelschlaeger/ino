#' Nop Object
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.
#'
#' @param which_optimizer
#' Selects specified numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels,
#' - a \code{numeric} (vector) of optimizer ids, see the \code{$print()} output.
#' @param which_run
#' Selects saved results of optimization runs. Either:
#' - \code{"all"} for all results,
#' - \code{"last"} for the results of the last \code{$optimize()} call,
#' - \code{"failed"}, the results from failed optimization runs,
#' - a \code{character} (vector) of optimization labels,
#' - a \code{numeric} (vector) of optimization run ids, see \code{$run_ids()}.
#' @param which_element
#' Selects elements of saved optimization results. Either:
#' - \code{"all"} for all available elements,
#' - \code{"default"}, the elements that are saved for all optimization runs by
#'   default, i.e.
#'   - \code{"optimization_label"}, the label for the optimization run,
#'   - \code{"optimizer_label"}, the label for the optimizer,
#'   - \code{"value"}, the function value at the found optimum,
#'   - \code{"parameter"}, the parameter at which the optimum value is obtained,
#'   - \code{"seconds"}, the optimization time in seconds,
#'   - \code{"initial"}, the initial parameter,
#'   - \code{"error"}, indicating whether an error occurred,
#'   - \code{"error_message"}, the error message (if any).
#' - a \code{character} (vector) with names of specific elements (see
#'   \code{$elements()} for the names of available elements).
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' @param only_comparable
#' Either \code{TRUE} to show only comparable results (i.e., results obtained
#' for the original optimization problem without any transformations),
#' or \code{FALSE} to include all optimization results.
#' @param verbose
#' Either \code{TRUE} to print progress and details, or \code{FALSE} to hide
#' such messages.
#' @param ncores
#' An \code{integer}, setting the number of CPU cores for parallel computation.
#' The default is \code{1}.
#' You can use \code{parallel::detectCores()} to detect the number of available
#' CPU cores.
#' @param seed
#' An \code{integer}, passed on to \code{\link{set.seed}} for reproducibility.
#' Can be \code{NULL} for no seed, which is the default.
#' @param return_results
#' Set to \code{TRUE} to return the optimization results as a \code{list}.
#' By default \code{return_results = FALSE}.
#' @param simplify
#' If \code{simplify = TRUE}, the nested list output of optimization results is
#' flattened if only one element is selected.
#' @param save_results
#' Set to \code{TRUE} to save the optimization results inside the \code{Nop}
#' object. These results can be analyzed via different methods, see the details.
#' By default, \code{save_results = TRUE}.
#' @param hide_warnings
#' Either \code{TRUE} or \code{FALSE} to hide (show) warning messages.
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
#'
#' @return
#' For the output of the different methods, please refer to their respective
#' documentation.
#'
#' @details
#' # Getting started
#'
#' ## Step 1: Create a new \code{Nop} object
#' Call \code{object <- Nop$new(f, npar, ...)} where
#' - \code{f} is a single-valued function to be optimized with respect to its
#'   first argument,
#' - \code{npar} is the length of the first argument of \code{f},
#' - and \code{...} are additional arguments for \code{f}. The additional
#'   arguments can also be managed via the \code{$argument()} method.
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
#' Call one of the following methods to initialize the optimization:
#' - \code{object$initialize_fixed()} for fixed initial values,
#' - \code{object$initialize_random()} for random initial values,
#' - \code{object$initialize_continue()} for initial values based on parameter
#'   estimates from previous optimization runs.
#'
#' The different initialization strategies are illustrated in the package
#' vignettes. Next, call \code{object$optimize()} for the optimization.
#'
#' # Methods for analyzing the results
#' A \code{Nop} object provides methods for the analysis of the saved
#' optimization results, with filter options for optimization runs, optimizers,
#' and elements (i.e., parts of the optimizer outputs):
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
#' - \code{$clear()} deletes optimization results,
#' - \code{$elements()} returns the names of the available elements in the
#'   optimizer outputs,
#' - \code{$number()} returns the number of saved optimization results,
#' - \code{$deviation()} calculates and plots deviations of parameters to a
#'   reference parameter (e.g., to the true parameter vector
#'   \code{self$true_parameter}),
#' - \code{$trace()} calculates and plots the trace of an optimization,
#' - \code{$f_name} stores the function name,
#' - \code{$npar} stores the length of the target argument,
#' - \code{$true_value} stores the true function value at the optimum
#'   (if available),
#' - \code{$true_parameter} stores the true parameter vector at the optimum
#'   (if available),
#' - \code{$fresh_label} generates a new label for the optimization.
#'
#' @examples
#' # Optimization of the Ackley function
#'
#' Nop$new(f = f_ackley, npar = 2)$   # initialize the Nop object
#'   set_optimizer(optimizer_nlm())$  # define the nlm optimizer
#'   initialize_random(runs = 100)$   # initialize 100 times randomly
#'   optimize(verbose = FALSE)$       # optimize
#'   optima()                         # return overview of optima
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
    #' arguments for \code{f}. They must be named. They can be managed via
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
      if (length(list(...)) > 0) self$argument("set", ...)
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
    print = function(digits = getOption("digits", default = 7), ...) {
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
            "- Additional arguments: {paste(names(arguments), collapse = ', ')}",
          ),
          "\n"
        )
      }
      true_parameter <- private$.true_parameter
      if (!is.null(true_parameter)) {
        cat(
          glue::glue(
            "- True optimum at: ",
            "{paste(round(true_parameter, digits = digits), collapse = ' ')}"
          ),
          "\n"
        )
      }
      true_value <- private$.true_value
      if (!is.null(true_value)) {
        cat(
          glue::glue(
            "- True optimum value: ",
            "{round(true_value, digits = digits)}"
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
      cat(crayon::underline("Optimization records:\n"))
      if (suppressWarnings(self$number()) == 0) {
        cat(cli::style_italic("No results saved yet.\n"))
      } else {
        suppressWarnings({
          noptimizations <- self$number()
          ncomparable <- self$number(only_comparable = TRUE)
          nfailed <- self$number(which_run = 'failed')
        })
        cat(glue::glue(
          "- Number optimizations: {noptimizations}",
          "- Comparable optimizations: {ncomparable}",
          "- Failed optimizations: {nfailed}",
          .sep = "\n"
        ), "\n")
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
      action, ..., verbose = getOption("verbose", default = FALSE)
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
    #' Validates the configuration of a \code{Nop} object.
    #' @param at
    #' A \code{numeric} of length \code{npar}, the point at which the
    #' function \code{f} and the specified optimizer are tested.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @return
    #' Invisibly \code{TRUE} if the tests are successful.
    validate = function(
      at = rnorm(self$npar), which_optimizer = "all", time_limit = 10,
      verbose = getOption("verbose", default = FALSE),
      digits = getOption("digits", default = 7)
    ) {
      private$.check_target_argument(at)
      optimizer_ids <- suppressWarnings(
        private$.get_optimizer_ids(which_optimizer = which_optimizer)
      )
      is_TRUE_FALSE(verbose)
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
              "Time limit of {time_limit}s was reached in the function call."
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
          out <- self$optimize(
            initial = at, runs = 1, which_optimizer = i, seed = NULL,
            return_results = TRUE, save_results = FALSE, ncores = 1,
            verbose = FALSE, simplify = FALSE, time_limit = time_limit,
            hide_warnings = TRUE
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
                }
              }
            }
          }
        }
      }
      invisible(TRUE)
    },

    #' @description
    #' Evaluates the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}, the point where the
    #' function is evaluated.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values drawn
    #' from a standard normal distribution.
    #' @return
    #' Either:
    #' - a \code{numeric} value, the function value at \code{at},
    #' - \code{"time limit reached"} if the time limit was reached,
    #' - the error message if the evaluation failed.
    evaluate = function(
      at = rnorm(self$npar), time_limit = NULL, hide_warnings = FALSE
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

    initialize_fixed = function(at) {

    },

    initialize_random = function(
      sampler = stats::rnorm(self$npar), runs = 1
    ) {

    },

    initialize_continue = function(
      run_ids, optimizer_ids, transform = function(x) x
    ) {

    },

    #' @description
    #' Optimizes the function.
    #' @param initial
    #' Specify the initial point where the optimizer should start. Either:
    #' - the \code{character} \code{"random"} (the default) for random initial
    #'   values drawn from a standard normal distribution,
    #' - a \code{numeric} vector of length \code{npar}, the deterministic
    #'   starting point for the optimization,
    #' - a \code{list} of such vectors (in this case, \code{runs} is set to the
    #'   length of the \code{list}),
    #' - or a \code{function} without any arguments that returns a
    #'   \code{numeric} vector of length \code{npar}.
    #' In all these cases, the same initial values are used across optimizers.
    #'
    #' For more flexibility, a \code{funtion} input for \code{initial} can have
    #' two arguments, where the first argument specifies the optimization run,
    #' and the second argument specifies the optimizer, e.g.,
    #' \code{initial <- function(run, optimizer) ...} for the
    #' \code{run}-th optimization run and the \code{optimizer}-th optimizer
    #' listed in the \code{$print()} output.
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' If \code{initial} is a \code{list}, \code{runs} is set to
    #' \code{length(initial)}.
    #' By default, \code{runs = 1}.
    #' @param optimization_label
    #' Only relevant if \code{save_results = TRUE}.
    #' In this case, a \code{character} to specify a label for the optimization.
    #' Labels are useful to distinguish optimization runs later.
    #' By default, \code{optimization_label = self$fresh_label} creates a new
    #' label.
    #' @param unique_label
    #' Either \code{TRUE} to ensure that \code{optimization_label} has not been
    #' used before (the default) or \code{FALSE} else.
    #' @param check_initial
    #' Either \code{TRUE} (default) to check the initial values and fail on
    #' misspecification, or \code{FALSE} to accept misspecifications.
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
      initial = "random", runs = 1, which_optimizer = "all", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      optimization_label = self$fresh_label, unique_label = TRUE,
      ncores = 1, verbose = getOption("verbose", default = FALSE),
      simplify = TRUE, time_limit = NULL, hide_warnings = TRUE,
      check_initial = TRUE
    ) {

      ### input checks
      private$.check_additional_arguments_complete()
      if (is.list(initial)) runs <- length(initial)
      is_count(runs, allow_zero = FALSE)
      is_TRUE_FALSE(return_results, allow_na = FALSE)
      is_TRUE_FALSE(save_results, allow_na = FALSE)
      is_name(optimization_label, allow_na = FALSE)
      is_TRUE_FALSE(unique_label, allow_na = FALSE)
      if (unique_label) {
        if (optimization_label %in% private$.records$optimization_labels) {
          ino_stop("Label {.val optimization_label} already exists.")
        }
      }
      is_count(ncores, allow_zero = FALSE)
      is_TRUE_FALSE(verbose, allow_na = FALSE)
      is_TRUE_FALSE(simplify, allow_na = FALSE)
      is_time_limit(time_limit)
      is_TRUE_FALSE(hide_warnings, allow_na = FALSE)
      is_TRUE_FALSE(check_initial, allow_na = FALSE)

      ### build progress bar
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

      ### set seed
      ino_seed(seed, verbose = verbose)

      ### optimizer ids
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (length(optimizer_ids) == 0) return(invisible(self))

      ### build initial values
      initial_values <- initial_values_helper(
        initial = initial, npar = private$.npar,
        check_initial = check_initial,
        runs = runs, optimizer_ids = optimizer_ids
      )

      ### optimization
      parallel <- ncores > 1 && runs >= 2 * ncores
      if (parallel) {
        cluster <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cluster))
        doSNOW::registerDoSNOW(cluster)
        ino_status(
          glue::glue("Parallel optimization with {ncores} cores."),
          verbose = verbose
        )
      }
      `%par_seq%` <- ifelse(parallel, `%dopar%`, `%do%`)
      if (verbose) pb$tick(0)
      results <- foreach::foreach(
        run = 1:runs, .packages = "ino", .export = "private",
        .inorder = TRUE, .options.snow = opts
      ) %par_seq% {
        lapply(optimizer_ids, function(optimizer_id) {
          private$.optimize(
            initial = initial_values[[run]][[optimizer_id]],
            optimizer_id = optimizer_id,
            time_limit = time_limit,
            hide_warnings = hide_warnings
          )
        })
      }

      ### return
      if (save_results) {
        private$.records$save(
          results = results,
          results_depth = 3,
          optimizer_label = names(self$optimizer)[optimizer_ids],
          optimization_label = optimization_label,
          comparable = length(private$.original_arguments) == 0
        )
      }
      if (return_results) {
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
    #' Returns saved optimization results.
    #' @return
    #' A nested \code{list} of optimization results.
    #' Each element corresponds to one optimization run and is a \code{list}
    #' of results for each optimizer.
    #' If \code{simplify = TRUE}, the output is flattened if possible.
    results = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      only_comparable = FALSE, simplify = TRUE
    ) {

      # TODO: if 'which_element' does not exist for some 'which_optimizer',
      #       drop it and give a warning (same for 'which_optimizer')

      run_ids <- private$.get_run_ids(which_run)
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (length(run_ids) == 0 || length(optimizer_ids) == 0) {
        return(invisible(list()))
      }
      which_element <- private$.check_which_element(
        which_element = which_element, optimizer_ids = optimizer_ids
      )
      private$.results |> filter_results(
        run_ids = run_ids, optimizer_ids = optimizer_ids,
        which_element = which_element, only_comparable = only_comparable,
        keep_empty = FALSE
      ) |> simplify_results(simplify = simplify)
    },

    #' @description
    #' Returns the number of saved optimization results.
    #' @return
    #' An \code{integer}.
    number = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      only_comparable = FALSE
    ) {
      0
    },

    #' @description
    #' Deletes optimization results.
    #' @return
    #' Invisibly the \code{Records} object.
    clear = function(which_run, which_optimizer, which_element) {
      if (missing(which_run)) {
        ino_stop("Please specify {.var which_run}.")
      }
      run_ids <- private$.get_run_ids(which_run)
      if (length(run_ids) == 0) {
        return(invisible(self))
      }
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      which_element <- suppressWarnings(
        private$.check_which_element(
          which_element = which_element, optimizer_ids = optimizer_ids,
          protected_elements = c("run", "optimizer", "label")
        )
      )
      for (i in run_ids) {
        for (j in optimizer_ids) {
          private$.results[[i]][[j]][which_element] <- NULL
        }
      }
      invisible(self)
    },

    #' @description
    #' Returns names of available elements by optimizer in the results.
    #' @return
    #' A \code{list}.
    elements = function(
      which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer = which_optimizer
      )


      ids <- private$.get_ids(
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      )
      if (length(ids) == 0) return(list())
      elements <- list()
      if (length(private$.results) == 0) {
        ino_warn(
          "No optimization results saved yet.",
          "Please call {.var $optimize(save_results = TRUE)}."
        )
      }
      for (id in optimizer_ids) {
        results <- sapply(private$.results, `[`, id)
        names <- names(unlist(results, recursive = FALSE, use.names = TRUE))
        optimizer_label <- names(self$optimizer)[id]
        elements[[optimizer_label]] <- unique(names)
      }
      return(elements)
    },

    #' @description
    #' Continues optimization runs, e.g., with a transformed parameter.
    #' @return
    #' The same as the return value of \code{$optimize()}.
    continue = function(
      which_run, which_optimizer = "all", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      ncores = 1, verbose = getOption("verbose", default = FALSE),
      simplify = TRUE, time_limit = NULL, hide_warnings = TRUE
    ) {
      run_ids <- private$.get_run_ids(which_run = which_run)
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer = which_optimizer
      )
      ino_status(
        glue::glue("Continue {length(run_ids)} optimization runs."),
        verbose = verbose
      )
      previous_results <- self$results(
        which_run = run_ids, which_optimizer = optimizer_ids,
        which_element = "all", simplify = FALSE
      )
      initial <- function(run_id, optimizer_id) {
        previous_results[[run_id]][[optimizer_id]][["parameter"]]
      }
      continued_results <- self$optimize(
        initial = initial, runs = length(run_ids),
        which_optimizer = optimizer_ids, seed = seed, return_results = TRUE,
        save_results = FALSE, label = "continued", ncores = ncores,
        verbose = verbose, simplify = FALSE, time_limit = time_limit,
        hide_warnings = hide_warnings, check_initial = FALSE
      )
      results <- private$.merge_continued_previous_results(
        continued_results = continued_results,
        previous_results = previous_results
      )
      if (save_results) {
        for (i in seq_along(run_ids)) {
          private$.save_optimization_run(
            run = results[[i]], optimizer_ids = optimizer_ids,
            run_id = run_ids[i]
          )
        }
      }
      if (return_results) {
        simplify_results(results = results, simplify = simplify)
      } else {
        invisible(self)
      }
    },

    #' @description
    #' Provides an overview of the optimization runs.
    #' @param ...
    #' Optionally named expressions of elements.
    #' See \code{$elements_available()} for the names of all available elements.
    #' In addition, \code{"true_value"}, \code{"true_parameter"},
    #' \code{"best_value"}, and \code{"best_parameter"} can be accessed
    #' As an example, you could add
    #' \code{distance = "sqrt(sum((parameter - true_parameter) ^ 2))"} for the
    #' euclidean distance between the estimated and true parameter vector.
    #' @return
    #' A \code{data.frame} with optimization results.
    #' @importFrom dplyr bind_rows
    summary = function(
      which_element = c("value", "parameter"), which_run = "all",
      which_optimizer = "all", digits = getOption("digits", default = 7),
      only_comparable = FALSE,
      ...
    ) {
      summary.Nop(
        object = self, which_element = which_element, which_run = which_run,
        which_optimizer = which_optimizer, digits = digits,
        only_comparable = only_comparable, ...
      )
    },

    #' @description
    #' Provides an overview of the identified optimum values.
    #' @param sort_by
    #' Either:
    #' - \code{"frequency"} (default) to sort rows by frequency,
    #' - \code{"value"} to sort by value.
    #' @return
    #' A \code{data.frame}.
    optima = function(
      digits = getOption("digits", default = 7), sort_by = "frequency",
      which_run = "all", which_optimizer = "all",
      only_comparable = TRUE
    ) {
      is_count(digits, allow_zero = TRUE)
      if (!(identical(sort_by, "frequency") | identical(sort_by, "value"))) {
        ino_stop(
          "Input {.var sort_by} must be {.val frequency} or {.val value}."
        )
      }
      values <- self$summary(
        which_element = "value", which_run = which_run,
        which_optimizer = which_optimizer, digits = digits,
        only_comparable = only_comparable
      )
      if (length(values) == 0) {
        return(invisible(data.frame()))
      }
      optima <- as.data.frame(
        table(values, useNA = "ifany")
      )
      colnames(optima) <- c("value", "frequency")

      ### sort rows
      decreasing <- ifelse(sort_by == "value" && self$minimized, FALSE, TRUE)
      optima <- optima[order(optima[[sort_by]], decreasing = decreasing), ]
      rownames(optima) <- NULL

      ### return optima
      return(optima)
    },

    #' @description
    #' Visualizes the optimization time or value.
    #' @param which_element
    #' Either:
    #' - \code{"seconds"} to plot the optimization times (default)
    #' - \code{"value"} to plot the optimization values
    #' @param by
    #' Either:
    #' - \code{"optimization_label"} to group by optimization label
    #' - \code{"optimizer_label"} to group by optimizer label
    #' - \code{NULL} to not group (default)
    #' @param relative
    #' Only if \code{which_element = "seconds"}.
    #' In this case, set to \code{TRUE} to plot relative time differences with
    #' respect to the overall median.
    #' @return
    #' A \code{\link[ggplot2]{ggplot}} object.
    plot = function(
      which_element = "seconds", by = NULL, relative = FALSE,
      which_run = "all", which_optimizer = "all", only_comparable = FALSE,
      title = paste("Optimization of", x$f_name), xlim = c(NA, NA)
    ) {
      plot.Nop(
        x = self, which_element = which_element, by = by, relative = relative,
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable, xlim = xlim
      )
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
      ...
    ) {
      is_count(iterations)
      is_number(tolerance)
      which_element <- match.arg(which_element, several.ok = TRUE)
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
    #' Returns the best found \code{numeric} value of \code{f}.
    #' @return
    #' A \code{numeric}, the best found \code{numeric} value of \code{f}.
    #' The output has two attributes:
    #' - \code{run}, the run id that led to the best value,
    #' - \code{optimizer}, the optimizer that led to the best value.
    #' In the case that multiple optimization runs led to the best value, the
    #' first one of them is returned.
    best_value = function(
      which_run = "all", which_optimizer = "all", only_comparable = TRUE,
      digits = getOption("digits", default = 7)
    ) {
      summary <- self$summary(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = c("value", "optimizer", "run"),
        only_comparable = only_comparable, digits = Inf
      )
      best <- do.call(
        what = ifelse(self$minimized, which.min, which.max),
        args = list(summary$value)
      )
      structure(
        round(summary[best, "value"], digits = digits),
        "run" = summary[best, "run"],
        "optimizer" = summary[best, "optimizer"]
      )
    },

    #' @description
    #' Returns the best found \code{numeric} parameter vector.
    #' @return
    #' A \code{numeric} of length \code{self$npar}.
    #' The output has two attributes:
    #' - \code{run}, the run id that led to the best parameter vector,
    #' - \code{optimizer}, the optimizer that led to the best parameter vector.
    #' Note that this parameter vector is not necessarily unique.
    best_parameter = function(
      which_run = "all", which_optimizer = "all", only_comparable = TRUE,
      digits = getOption("digits", default = 7)
    ) {
      best_value <- self$best_value(
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      )
      run_id <- attr(best_value, "run")
      optimizer <- attr(best_value, "optimizer")
      best_parameter <- self$results(
        which_run = run_id, which_optimizer = optimizer,
        which_element = "parameter", only_comparable = only_comparable,
        simplify = TRUE
      )
      structure(
        round(best_parameter, digits = digits),
        "run" = attr(best_value, "run"),
        "optimizer" = attr(best_value, "optimizer")
      )
    }

  ),
  private = list(

    ### definition of the optimization problem
    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .true_parameter = NULL,
    .true_value = NULL,
    .arguments = list(),
    .original_arguments = list(),
    .optimizer = list(),

    ### checks for the optimization problem
    .check_additional_argument_exists = function(name) {
      stopifnot(is_name(name))
      if (!name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue(
            "Function argument {.var <name>} is not yet specified, ",
            "call {.var $argument(\"set\", {.val <name>} = ...)} first.",
            .open = "<", .close = ">"
          )
        )
      } else {
        invisible(TRUE)
      }
    },
    .check_additional_arguments_complete = function() {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        if (!all(nzchar(args_all[[arg]])) && is.name(args_all[[arg]])) {
          private$.check_additional_argument_exists(arg)
        }
      }
    },
    .check_target_argument = function(target_arg) {
      arg_name <- deparse(substitute(target_arg))
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
    },

    ### storage of optimization results and ids
    .results = list(),
    .next_result_id = function() {
      length(private$.results) + 1
    },
    .result_comparable = function() {
      length(private$.original_arguments) == 0
    },
    .optimizer_id_ids = list(),
    .optimization_label_ids = list(),
    .comparable_ids = integer(),
    .failed_ids = integer(),
    .save_result = function(result, optimizer_id, optimization_label) {
      result_id <- private$.next_result_id()
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
      if ("error_message" %in% parts) {
        is_name(result[["error_message"]], allow_na = TRUE)
      } else {
        result[["error_message"]] <- NA_character_
      }
      attr(result, "optimization_label") <- optimization_label
      attr(result, "optimizer_label") <- optimizer_label
      attr(result, "comparable") <- comparable
      attr(result, "optimizer_id") <- optimizer_id
      attr(result, "result_id") <- result_id
      private$.results[[result_id]] <- result
      private$.last_runs_ids <- c(private$.last_runs_ids, result_id)

      if (is.null(private$.optimizer_id_ids[[optimizer_id]])) {
        private$.optimizer_id_ids[[optimizer_id]] <- result_id
      } else {
        private$.optimizer_id_ids[[optimizer_id]] <- c(
          private$.optimizer_id_ids[[optimizer_id]], result_id
        )
      }
      if (is.null(private$.optimization_label_ids[[optimization_label]])) {
        private$.optimization_label_ids[[optimization_label]] <- result_id
      } else {
        private$.optimization_label_ids[[optimization_label]] <- c(
          private$.optimization_label_ids[[optimization_label]], result_id
        )
      }
      if (private$.result_comparable()) {
        private$.comparable_ids <- c(private$.comparable_ids, result_id)
      }
      if (result[["error"]]) {
        private$.failed_ids <- c(private$.failed_ids, result_id)
      }
    },
    .get_results = function(result_id) {
      private$.results[result_id]
    },
    .modify_result = function(result_id) {
      stopifnot(
        is.list(continued_results), is.list(previous_results),
        length(continued_results) == length(previous_results),
        sapply(continued_results, length) == sapply(previous_results, length)
      )
      comparable <- length(private$.original_arguments) == 0
      for (run_id in seq_along(continued_results)) {
        for (optimizer_id in seq_along(continued_results[[run_id]])) {
          previous_run <- previous_results[[run_id]][[optimizer_id]]
          continued_run <- continued_results[[run_id]][[optimizer_id]]
          seconds_previous <- previous_run[["seconds"]]
          seconds_continued <- continued_run[["seconds"]]
          continued_run[["seconds"]] <- seconds_previous + seconds_continued
          continued_run[["label"]] <- previous_run[["label"]]
          continued_run[["comparable"]] <- comparable
          continued_run[["previous_run"]] <- previous_run
          continued_results[[run_id]][[optimizer_id]] <- continued_run
        }
      }
      return(continued_results)
    },
    .number_results = function() {

    },

    ### check selection of ids
    .check_which_run = function(which_run) {

    },
    .check_which_optimizer = function(which_optimizer) {
      is_name_vector(which_optimizer, allow_na = FALSE)
      if (identical(which_optimizer, "all")) {
        which_optimizer <- self$optimizer_labels
      }

      return(which_optimizer)
    },
    .check_which_element = function(which_element, which_optimizer) {
      stopifnot(sapply(optimizer_ids, is_count))
      if (length(protected_elements) > 0) sapply(protected_elements, is_name)
      all_elements <- unique(unlist(
        self$elements(which_optimizer = optimizer_ids)
      ))
      if (identical(which_element, "all")) {
        which_element <- all_elements
      }
      if (identical(which_element, "default")) {
        which_element <- c(
          "run", "optimizer", "value", "parameter", "seconds", "label"
        )
      }
      if (!all(sapply(which_element, is_name, error = FALSE))) {
        ino_stop(
          "Input {.var which_element} is misspecified.",
          "It can be {.val all} or {.val default}.",
          "It can also be a {.cls character} vector of specific element names."
        )
      }
      which_element <- unique(which_element)
      protect <- intersect(which_element, protected_elements)
      if (length(protect) > 0) {
        ino_warn(
          "The following elements cannot be selected:",
          glue::glue("{protect}")
        )
        which_element <- setdiff(which_element, protected_elements)
      }
      unavailable <- setdiff(which_element, all_elements)
      if (length(unavailable) > 0) {
        ino_warn(
          "The following elements are not available:",
          glue::glue("{unavailable}")
        )
        which_element <- intersect(which_element, all_elements)
      }
      return(which_element)
    },

    ### access of ids
    .get_optimizer_ids = function(which_optimizer) {
      ids <- seq_along(private$.optimizer)
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer specified yet.",
          "Use {.fun $set_optimizer} to specify an optimizer."
        )
        return(integer(0))
      }
      if (identical(which_optimizer, "all")) {
        return(ids)
      } else if (is_name_vector(which_optimizer, error = FALSE)) {
        ids <- which(names(private$.optimizer) %in% which_optimizer)
      } else if (is_number_vector(which_optimizer, error = FALSE)) {
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
    .get_result_ids = function(which_run, which_optimizer, only_comparable) {
      if (length(private$.results) == 0) {
        ino_warn("No optimization results saved yet.")
        return(integer())
      }
      is_name_vector(which_optimizer, allow_na = FALSE)
      is_TRUE_FALSE(only_comparable, allow_na = FALSE)
      ids <- if (is_name(which_run, allow_na = FALSE, error = FALSE)) {
        if (identical(which_run, "all")) {
          seq_along(private$.results)
        } else if (identical(which_run, "last")) {
          private$.last_runs_ids
        } else if (identical(which_run, "failed")) {
          private$.failed_runs_ids
        } else if (which_run %in% self$optimization_labels) {
          private$.optimization_label_ids[[which_run]]
        } else {
          integer()
        }
      } else if (is_index_vector(which_run, error = FALSE)) {
        which(seq_along(private$.results) %in% which_run)
      } else {
        ino_stop("Argument {.var which_run} is misspecified.")
      }
      if (!identical(which_optimizer, "all")) {
        ids <- if (which_optimizer %in% names(private$.optimizer_label_ids)) {
          intersect(ids, private$.optimizer_label_ids[[which_optimizer]])
        } else {
          integer()
        }
      }
      if (only_comparable) {
        ids <- intersect(ids, private$.comparable_runs_ids)
      }
      if (length(ids) == 0) {
        ino_warn("Your input selects no saved result.")
        return(integer())
      }
      return(ids)
    },

    ### cheap function optimization
    .optimize = function(initial, optimizer_id, time_limit, hide_warnings) {
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
              what = optimizeR::apply_optimizer,
              args = c(
                list(
                  "optimizer" = private$.optimizer[[optimizer_id]],
                  "objective" = private$.f,
                  "initial" = initial
                ),
                private$.arguments
              )
            ),
            classes = if (hide_warnings) "warning" else ""
          )
        },
        error = function(e) {
          error_message <- e$message
          time_limit_reached <- grepl(
            "reached elapsed time limit|reached CPU time limit", error_message
          )
          if (time_limit_reached) {
            error_message <- "time limit reached"
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

    #' @field true_value The true \code{numeric} optimum value of \code{f}
    #' (if available).
    true_value = function(value) {
      if (missing(value)) {
        out <- private$.true_value
        if (is.null(out)) {
          ino_warn("The true value has not been specified yet.")
        }
        return(out)
      } else {
        if (is.null(value)) {
          private$.true_value <- NULL
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_value} and {.var true_parameter}.",
            verbose = getOption("verbose", default = FALSE)
          )
        } else {
          if (!(is.vector(value) && is.numeric(value) && length(value) == 1)) {
            ino_stop(
              "{.var true_value} must be a single {.cls numeric}."
            )
          }
          if (!is.null(private$.true_parameter)) {
            true_value_old <- self$evaluate(at = private$.true_parameter)
            if (value != true_value_old) {
              ino_stop(
                "Please update {.var true_parameter} first.",
                "Alternatively, remove it via {.val true_parameter <- NULL}."
              )
            }
          }
          private$.true_value <- value
          digits <- getOption('ino_digits', default = 7)
          ino_status(
            glue::glue(
              "Set true optimum value to",
              "{round(private$.true_value, digits = digits)}.",
              .sep = " "
            ),
            verbose = getOption("verbose", default = FALSE)
          )
        }
      }
    },

    #' @field true_parameter The true optimum \code{numeric} parameter vector
    #' of length \code{npar} (if available), i.e., the point where \code{f}
    #' obtains its optimum.
    true_parameter = function(value) {
      if (missing(value)) {
        out <- private$.true_parameter
        if (is.null(out)) {
          ino_warn("The true parameter vector has not been specified yet.")
        }
        return(out)
      } else {
        if (is.null(value)) {
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_parameter}.",
            verbose = getOption("verbose", default = FALSE)
          )
        } else {
          private$.check_target_argument(value)
          self$true_value <- self$evaluate(at = value)
          private$.true_parameter <- value
          digits <- getOption('ino_digits', default = 7)
          ino_status(
            glue::glue(
              "Set true optimum parameter vector to",
              "{round(private$.true_parameter, digits = digits)}.",
              .sep = " "
            ),
            verbose = getOption("verbose", default = FALSE)
          )
        }
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
          if (!label %in% private$.records$optimization_labels) {
            return(as.character(label))
          } else {
            n <- n + 1
          }
        }
      } else {
        ino_stop("{.var $fresh_label} is read only.")
      }
    }

  )
)

#' @noRd
#' @exportS3Method

print.Nop <- function(x, digits = getOption("digits", default = 7), ...) {
  x$print()
}

#' @noRd
#' @exportS3Method

summary.Nop <- function(
    object, which_element = c("value", "parameter"), which_run = "all",
    which_optimizer = "all", digits = getOption("ino_digits", default = 2),
    only_comparable = FALSE, ...
) {
  ### extract results and combine in data.frame
  out <- data.frame()
  results <- object$results(
    which_run = which_run, which_optimizer = which_optimizer,
    which_element = which_element, only_comparable = only_comparable,
    simplify = FALSE
  )
  if (length(results) == 0) {
    return(invisible(out))
  }
  result_names <- unique(names(
    unlist(unlist(results, recursive = FALSE), recursive = FALSE)
  ))
  for (run_id in seq_along(results)) {
    for (optimizer_id in seq_along(results[[run_id]])) {
      append <- results[[run_id]][[optimizer_id]]
      missing_results <- setdiff(result_names, names(append))
      if (length(missing_results) > 0) {
        append[missing_results] <- NA
      }
      if (length(append) > 0) {
        out <- dplyr::bind_rows(out, as.data.frame(t(cbind(append))))
      }
    }
  }
  rownames(out) <- NULL

  ### add elements
  add_vars <- list(...)
  if (length(add_vars) > 0) {
    results_all <- object$results(
      which_run = which_run, which_optimizer = which_optimizer,
      which_element = "all", only_comparable = only_comparable,
      simplify = FALSE
    )
    true_value <- object$true_value
    true_parameter <- object$true_parameter
    best_value <- object$best_value()
    best_parameter <- object$best_parameter()
    for (i in seq_along(add_vars)) {
      out[[names(add_vars)[i]]] <- sapply(
        unlist(results_all, recursive = FALSE),
        function(r) {
          env <- new.env()
          list2env(r, env)
          tryCatch(
            eval(parse(text = add_vars[[i]]), env),
            error = function(e) NA
          )
        }
      )
    }
  }

  ### unlist single-valued records
  for (i in seq_len(ncol(out))) {
    unlist_try <- unlist(out[, i], recursive = FALSE)
    if (length(unlist_try) == nrow(out)) {
      out[, i] <- unlist(out[, i], recursive = FALSE)
    }
  }

  ### round numeric records
  for (i in seq_len(ncol(out))) {
    if (is.vector(out[, i]) && is.numeric(out[, i])) {
      out[, i] <- round(out[, i], digits = digits)
    }
    if (is.list(out[, i]) && all(sapply(out[, i], is.numeric))) {
      out[[i]] <- lapply(out[, i], round, digits = digits)
    }
  }

  ### return data.frame
  return(out)
}

#' @noRd
#' @importFrom stats complete.cases median
#' @importFrom dplyr summarize mutate
#' @importFrom ggplot2 ggplot aes scale_x_continuous theme_minimal theme
#' @importFrom ggplot2 geom_boxplot geom_vline annotate element_blank ggtitle
#' @importFrom ggplot2 coord_cartesian
#' @importFrom scales label_percent
#' @importFrom forcats fct_reorder
#' @importFrom rlang .data
#' @importFrom scales percent
#' @exportS3Method

plot.Nop <- function(
    x, which_element = "seconds", by = NULL, relative = FALSE,
    which_run = "all", which_optimizer = "all", only_comparable = FALSE,
    title = paste("Optimization of", x$f_name), xlim = c(NA, NA), ...) {
  ### input checks
  if (!which_element %in% c("seconds", "value")) {
    ino_stop(
      "Argument {.var which_element} must be {.val seconds} or {.val value}."
    )
  }
  if (!is.null(by)) {
    if (!by %in% c("label", "optimizer")) {
      ino_stop(
        "Argument {.var by} must be {.val label} or {.val optimizer} or {.val NULL}."
      )
    }
  }
  is_TRUE_FALSE(relative)
  if (identical(which_element, "value") && relative) {
    ino_status(
      "Argument {.var relative} cannot be {.val TRUE} if {.var which_element} is {.val value}."
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

  ### get data
  data <- x$summary(
    which_element = c(which_element, by), which_run = which_run,
    which_optimizer = which_optimizer, only_comparable = only_comparable,
    digits = Inf
  )

  ### drop incomplete cases
  incomplete_cases <- which(!stats::complete.cases(data))
  if (length(incomplete_cases) > 0) {
    ino_status(
      paste("Dropped", length(incomplete_cases), "results with missing data.")
    )
    data <- data[-incomplete_cases, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    ino_status(
      "No data to plot."
    )
    return(invisible(NULL))
  }

  ### compute relative 'seconds' wrt to median seconds
  if (identical(which_element, "seconds") && relative) {
    med <- dplyr::summarize(
      data,
      "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
    ) |> as.numeric()
    data <- data |>
      dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med)
  }

  ### sort by median 'seconds'
  if (identical(which_element, "seconds") && !is.null(by)) {
    data <- data |>
      dplyr::mutate(
        label = forcats::fct_reorder(
          .f = .data[[by]], .x = .data[["seconds"]],
          .fun = stats::median, .desc = TRUE
        )
      )
  }

  ### build base plot
  if (is.null(by)) {
    base_plot <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[which_element]], y = "")
    )
  } else {
    base_plot <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[which_element]], y = .data[[by]])
    )
  }
  base_plot <- base_plot +
    ggplot2::theme_minimal()

  ### add 'values'
  if (identical(which_element, "value")) {
    base_plot <- base_plot +
      ggplot2::geom_point(
        position = "jitter", alpha = 0.5
      ) +
      ggplot2::scale_x_continuous(
        name = "Function value at optimum"
      )
  }

  ### add 'seconds'
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
    if (!is.null(by)) {
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

  ### modify labels and axes
  if (is.null(by)) {
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
  base_plot <- base_plot + ggplot2::coord_cartesian(xlim = xlim)

  ### return plot
  return(base_plot)
}

