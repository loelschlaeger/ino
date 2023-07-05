#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a **n**umerical **o**ptimization **p**roblem.
#'
#' @param which_optimizer
#' Select specified numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels,
#' - a \code{numeric} (vector) of optimizer ids, see the \code{$print()} output.
#' @param which_run
#' Select saved results of optimization runs. Either:
#' - \code{"all"} for all results,
#' - \code{"failed"}, the results from all failed optimization runs,
#' - a \code{character} (vector) of labels specified in \code{$optimize()}.
#' @param which_element
#' Select elements of saved optimization results. Either:
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
#'   \code{$elements_available()} for the names of all available elements).
#' @param only_comparable
#' Either \code{TRUE} to show only comparable results (i.e., results obtained
#' for the original optimization problem without any transformations),
#' or \code{FALSE} to include all optimization results.
#' @param verbose
#' Either \code{TRUE} to print progress and details, or \code{FALSE} to hide
#' those messages.
#' @param ncores
#' An \code{integer}, setting the number of CPU cores for parallel computation.
#' The default is \code{1}.
#' You can use \code{parallel::detectCores()} to detect the number of available
#' CPU cores.
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' The default is \code{2}.
#' @param seed
#' An \code{integer}, passed on to \code{\link{set.seed}} for reproducibility.
#' Can be \code{NULL} for no seed, which is the default.
#' @param return_results
#' Set to \code{TRUE} to return the optimization results as a \code{list}.
#' Note that by default, \code{return_results = FALSE}, in which case
#' optimization results are saved inside the \code{Nop} object and can be
#' obtained via other methods.
#' @param simplify
#' Only relevant if \code{return_results = TRUE} and \code{runs = 1} and/or
#' only one optimizer is specified.
#' In this case, if \code{simplify = TRUE}, the nested list output
#' of optimization results is flattened if possible.
#' @param save_results
#' Set to \code{TRUE} to save the optimization results inside the \code{Nop}
#' object.
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
#' A \code{Nop} object that specifies the numerical optimization problem.
#'
#' @details
#' # Getting Started
#'
#' ## Step 1: Create a new \code{Nop} object:
#' Call \code{object <- Nop$new(f, npar, ...)} where
#' - \code{f} is the function to be optimized over its first argument,
#' - \code{npar} is the length of the first argument of \code{f},
#' - and \code{...} are additional arguments for \code{f}.
#'
#' ## Step 2: Specify one or more numerical optimizers:
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}, which can
#' be created with the \code{\link[optimizeR]{define_optimizer}} function from
#' the \{optimizeR\} package.
#' Two \code{optimizer} objects are already available:
#' - \code{\link[optimizeR]{optimizer_nlm}}
#' - \code{\link[optimizeR]{optimizer_optim}}
#'
#' ## Step 3: Test your configuration:
#' Call \code{object$test()} to validate your configuration. The \code{$print()}
#' method yields an overview of the configuration.
#'
#' # Function evaluation and optimization
#' Call \code{object$evaluate()} to evaluate the target function at some point.
#' Call \code{object$optimize()} for optimization. Furthermore,
#' - \code{object$standardize()} standardizes a function argument,
#' - \code{object$subset()} subsets a function argument,
#' - \code{object$continue()} continues optimization runs.
#'
#' # Analysis of the results
#' The \code{Nop} object provides methods for the analysis of the optimization
#' results, with filter options for optimization runs, optimizers, and elements:
#' - \code{$results()} returns the saved optimization results,
#' - \code{$summary()} summarizes the results,
#' - \code{$optima()} returns a frequency table of identified optima,
#' - \code{$plot()} visualizes the optimization time or value,
#' - \code{$best_parameter()} returns the best parameter vector,
#' - \code{$best_value()} returns the best function value.
#'
#' @examples
#' Nop$new(f = f_ackley, npar = 2)$
#'   set_optimizer(optimizer_nlm())$
#'   optimize(initial = "random", runs = 100, verbose = FALSE)$
#'   optima()
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @field runs A \code{\link{Runs}} object.
    runs = NULL,

    #' @description
    #' Creates a new \code{Nop} object.
    #' @param f
    #' The \code{function} to be optimized.
    #' It is optimized over its first argument, which should be a \code{numeric}
    #' vector of length \code{npar}.
    #' @param npar
    #' An \code{integer}, the length of the first argument of \code{f} (the
    #' argument over which \code{f} is optimized).
    #' @param ...
    #' Optionally additional and named arguments for \code{f}.
    #' @return
    #' A new \code{Nop} object.
    initialize = function(f, npar, ...) {
      if (missing(f)) {
        ino_stop("Please specify argument {.var f}.")
      }
      if (!is.function(f)) {
        ino_stop("Argument {.var f} is not a {.cls function}.")
      }
      if (is.null(formals(f))) {
        ino_stop("{.var f} must have at least one argument.")
      }
      if (missing(npar)) {
        ino_stop("Please specify argument {.var npar}.")
      }
      is_count(npar)
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
      if (length(list(...)) > 0) {
        self$set_argument(...)
      }
      self$runs <- Runs$new()
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
    print = function(digits = getOption("ino_digits", default = 2), ...) {
      cat(
        glue::glue(
          crayon::underline("Optimization problem:"),
          "- Function: {self$f_name}",
          "- Optimize over: {self$f_target} (length {self$npar})",
          .sep = "\n"
        ),
        "\n"
      )
      arguments <- suppressWarnings(self$arguments)
      if (length(arguments) > 0) {
        cat(
          glue::glue(
            "- Further arguments: {paste(names(arguments), collapse = ', ')}",
          ),
          "\n"
        )
      }
      true_parameter <- suppressWarnings(self$true_parameter)
      if (!is.null(true_parameter)) {
        cat(
          glue::glue(
            "- True optimum at: ",
            "{paste(round(true_parameter, digits = digits), collapse = ' ')}"
          ),
          "\n"
        )
      }
      true_value <- suppressWarnings(self$true_value)
      if (!is.null(true_value)) {
        cat(
          glue::glue(
            "- True optimum value: ",
            "{round(true_value, digits = digits)}"
          ),
          "\n"
        )
      }
      cat(crayon::underline("Numerical optimizers:\n"))
      optimizer <- suppressWarnings(self$optimizer)
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
      self$runs$print(digits = digits, ...)
      invisible(self)
    },

    #' @description
    #' Sets additional arguments for \code{f}.
    #' @param ...
    #' Optionally additional named arguments for \code{f}.
    #' @importFrom glue glue
    #' @return
    #' Invisibly the \code{Nop} object.
    set_argument = function(...) {
      arguments <- list(...)
      argument_names <- names(arguments)
      argument_names[which(is.null(argument_names))] <- ""
      for (i in seq_along(arguments)) {
        if (!is_name(argument_names[i], error = FALSE)) {
          ino_stop(glue::glue("Please name argument {i}."))
        }
        if (argument_names[i] %in% names(private$.arguments)) {
          ino_stop(
            glue::glue(
              "Argument {.var <argument_names[i]>} already exists, ",
              "call {.var $remove_argument({.val <argument_names[i]>})} first.",
              .open = "<", .close = ">"
            )
          )
        }
      }
      for (i in seq_along(arguments)) {
        private$.arguments[[argument_names[i]]] <- arguments[[i]]
      }
      invisible(self)
    },

    #' @description
    #' Gets an argument value of \code{f}.
    #' @param argument_name
    #' A \code{character}, the argument to extract.
    #' @return
    #' The argument.
    get_argument = function(argument_name) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      is_name(argument_name, error = TRUE)
      private$.check_additional_argument_exists(argument_name)
      private$.arguments[[argument_name]]
    },

    #' @description
    #' Removes an additional argument for \code{f}.
    #' @param argument_name
    #' A \code{character}, the argument to remove.
    #' @return
    #' Invisibly the \code{Nop} object.
    remove_argument = function(argument_name) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      is_name(argument_name, error = TRUE)
      private$.check_additional_argument_exists(argument_name)
      arg_id <- which(names(private$.arguments) == argument_name)
      private$.arguments[arg_id] <- NULL
      invisible(self)
    },

    #' @description
    #' Resets an additional argument for \code{f} after transformation with
    #' \code{$standardize()} or \code{$subset()}.
    #' @param argument_name
    #' A \code{character}, the name of the argument to reset.
    #' @return
    #' Invisibly the \code{Nop} object.
    reset_argument = function(
      argument_name, verbose = getOption("ino_verbose", default = TRUE)
    ) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      private$.check_additional_argument_exists(argument_name)
      if (!is.null(private$.original_arguments[[argument_name]])) {
        original_argument <- private$.original_arguments[[argument_name]]
        private$.arguments[[argument_name]] <- original_argument
        private$.original_arguments[[argument_name]] <- NULL
        ino_status(
          glue::glue("Reset `{argument_name}`."),
          verbose = verbose
        )
      } else {
        ino_warn("Nothing to reset.")
      }
      invisible(self)
    },

    #' @description
    #' Sets a numerical optimizer.
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
    test = function(
      at = rnorm(self$npar), which_optimizer = "all", time_limit = 10,
      verbose = getOption("ino_verbose", default = TRUE),
      digits = getOption("ino_digits", default = 2)
    ) {
      private$.check_target_argument(at)
      optimizer_ids <- suppressWarnings(
        private$.get_optimizer_ids(which_optimizer = which_optimizer)
      )
      is_TRUE_FALSE(verbose)
      ino_status("Check Nop specifications:", verbose = verbose)
      ino_success(
        glue::glue("Function specified: {self$f_name}"),
        verbose = verbose
      )
      ino_success(
        glue::glue(
          "Target argument specified: {self$f_target} (length {self$npar})"
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
            glue::glue("Return value: {round(out, digits = digits)}"),
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
              "Try to optimize the function with ",
              "`{paste(names(self$optimizer)[i], collapse = ', ')}`:"
            ),
            verbose = verbose
          )
          out <- self$optimize(
            initial = at, runs = 1, which_optimizer = i, seed = NULL,
            return_results = TRUE, save_results = FALSE, ncores = 1,
            verbose = FALSE, simplify = TRUE, time_limit = time_limit,
            hide_warnings = TRUE
          )
          if (!is.null(out$error)) {
            if (isTRUE(out$error)) {
              if (identical(out$error_message, "time limit reached")) {
                ino_warn(
                  glue::glue(
                    "Time limit of {time_limit}s was reached in the optimization."
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
    #' By default, \code{optimization_label = self$new_label} creates a new
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
      optimization_label = self$new_label, unique_label = TRUE,
      ncores = getOption("ino_ncores", default = 1),
      verbose = getOption("ino_verbose", default = TRUE), simplify = TRUE,
      time_limit = NULL, hide_warnings = TRUE, check_initial = TRUE
    ) {

      ### input checks
      private$.check_additional_arguments_complete()
      if (is.list(initial)) runs <- length(initial)
      is_count(runs)
      is_TRUE_FALSE(return_results)
      is_TRUE_FALSE(save_results)
      is_name(optimization_label)
      is_TRUE_FALSE(unique_label)
      if (unique_label) {
        if (optimization_label %in% self$runs$optimization_labels) {
          ino_stop("Label {.val optimization_label} already exists.")
        }
      }
      is_count(ncores)
      is_TRUE_FALSE(verbose)
      is_TRUE_FALSE(simplify)
      is_time_limit(time_limit)
      is_TRUE_FALSE(hide_warnings)
      is_TRUE_FALSE(check_initial)

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
      `%switch%` <- ifelse(parallel, `%dopar%`, `%do%`)
      if (verbose) pb$tick(0)
      results <- foreach::foreach(
        run = 1:runs, .packages = "ino", .export = "private",
        .inorder = TRUE, .options.snow = opts
      ) %switch% {
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
        self$runs$save_results(
          results = results,
          optimizer_label = names(self$optimizer),
          optimization_label = optimization_label,
          comparable = length(private$.original_arguments) == 0
        )
      }
      if (return_results) {
        self$runs$prepare_results(
          results = results,
          optimizer_label = names(self$optimizer),
          simplify = simplify
        )
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
    #' Returns names of available elements per optimizer.
    #' @return
    #' A \code{list}.
    elements_available = function(which_optimizer = "all") {
      runs$elements_available(which_optimizer = which_optimizer)
    },

    #' @description
    #' Standardizes the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be
    #' standardized. The argument must a \code{numeric} \code{vector},
    #' \code{matrix}, or \code{data.frame}.
    #' @param by_column
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to standardize column-wise (default) or
    #' \code{FALSE} to standardize row-wise.
    #' @param center
    #' Set to \code{TRUE} (default) for centering, resulting in zero mean.
    #' @param scale
    #' Set to \code{TRUE} (default) for scaling, resulting in unit variance.
    #' @param ignore
    #' A \code{integer} (vector) of column indices (or row indices if
    #' \code{by_column = FALSE}) to not standardize.
    #' @param jointly
    #' A \code{list} of \code{integer} vectors with column indices (or row
    #' indices if \code{by_column = FALSE}) to standardize jointly.
    #' @return
    #' Invisibly the \code{Nop} object.
    #' The \code{numeric} centering and scalings used (if any) are added as
    #' attributes \code{"standardized:center"} and \code{"standardized:scale"}
    #' to the argument specified via \code{argument_name}.
    standardize = function(
      argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
      ignore = integer(), jointly = list(),
      verbose = getOption("ino_verbose", default = TRUE)
    ) {
      original_argument <- self$get_argument(argument_name)
      standardized_argument <- standardize_helper(
        argument = original_argument, by_column = by_column, center = center,
        scale = scale, ignore = ignore, jointly = jointly
      )
      private$.arguments[[argument_name]] <- standardized_argument
      if (is.null(private$.original_arguments[[argument_name]])) {
        private$.original_arguments[[argument_name]] <- original_argument
      }
      ino_status(
        glue::glue("Standardized `{argument_name}`."),
        verbose = verbose
      )
      invisible(self)
    },

    #' @description
    #' Subsets the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be subsetted.
    #' @param by_row
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to subset row-wise (default) or
    #' \code{FALSE} to subset column-wise.
    #' @param how
    #' A \code{character}, specifying how to subset. Can be one of:
    #' - \code{"random"} (default), subset at random
    #' - \code{"first"}, subset to the first elements
    #' - \code{"last"}, subset to the last elements
    #' - \code{"similar"}, subset to similar elements
    #' - \code{"dissimilar"}, subset to dissimilar elements
    #' The options \code{"similar"} and \code{"dissimilar"} apply k-means
    #' clustering via \code{\link[stats]{kmeans}} and require that
    #' the argument \code{argument_name} is \code{numeric}.
    #' @param proportion
    #' A \code{numeric} between \code{0} and \code{1}, specifying the
    #' subset proportion.
    #' By default, \code{proportion = 0.5}.
    #' @param centers
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case, passed to \code{\link[stats]{kmeans}}.
    #' By default, \code{centers = 2}.
    #' @param ignore
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case, a \code{integer} (vector) of row indices (or column indices
    #' if \code{by_row = FALSE}) to ignore for clustering.
    #' @return
    #' Invisibly the \code{Nop} object.
    subset = function(
      argument_name, by_row = TRUE, how = "random", proportion = 0.5,
      centers = 2, ignore = integer(), seed = NULL,
      verbose = getOption("ino_verbose", default = TRUE)
    ) {
      original_argument <- self$get_argument(argument_name)
      ino_seed(seed, verbose = verbose)
      subsetted_argument <- subset_helper(
        argument = original_argument, by_row = by_row, how = how,
        proportion = proportion, centers = centers, ignore = ignore
      )
      ino_status(
        glue::glue(
          "Reduced '{argument_name}' from ",
          if (is.vector(original_argument)) {
            length_old <- length(original_argument)
            length_new <- length(subsetted_argument)
            "{length_old} to {length_new} {how} element(s)."
          } else {
            if (by_row) {
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
      private$.arguments[[argument_name]] <- subsetted_argument
      if (is.null(private$.original_arguments[[argument_name]])) {
        private$.original_arguments[[argument_name]] <- original_argument
      }
      invisible(self)
    },

    #' @description
    #' Continues optimization runs, e.g., with a transformed parameter.
    #' @return
    #' The same as the return value of \code{$optimize()}.
    continue = function(
      which_run, which_optimizer = "all", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      ncores = getOption("ino_ncores", default = 1),
      verbose = getOption("ino_verbose", default = TRUE), simplify = TRUE,
      time_limit = NULL, hide_warnings = TRUE
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
      which_optimizer = "all", digits = getOption("ino_digits", default = 2),
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
      digits = getOption("ino_digits", default = 2), sort_by = "frequency",
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
      digits = getOption("ino_digits", default = 2)
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
      digits = getOption("ino_digits", default = 2)
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

    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .arguments = list(),
    .original_arguments = list(),
    .true_parameter = NULL,
    .true_value = NULL,
    .optimizer = list(),

    ### check if argument for target function exists
    .check_additional_argument_exists = function(argument_name) {
      stopifnot(is_name(argument_name))
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue(
            "Argument {.var <argument_name>} is not yet specified, ",
            "call {.var $set_argument({.val <argument_name>} = ...)} first.",
            .open = "<", .close = ">"
          )
        )
      } else {
        invisible(TRUE)
      }
    },

    ### check if all required arguments for target function are specified
    .check_additional_arguments_complete = function() {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        ### checks if `arg` has default value in `f`
        if (!all(nzchar(args_all[[arg]])) && is.name(args_all[[arg]])) {
          private$.check_additional_argument_exists(arg)
        }
      }
    },

    ### check supplied value for target argument
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

    ### check selected elements
    .check_which_element = function(
      which_element, optimizer_ids, protected_elements = character()
    ) {
      stopifnot(sapply(optimizer_ids, is_count))
      if (length(protected_elements) > 0) sapply(protected_elements, is_name)
      all_elements <- unique(unlist(
        self$elements_available(which_optimizer = optimizer_ids)
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

    ### get ids of optimizers
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
    },

    ### merge continued with previous optimization results
    .merge_continued_previous_results = function(
      continued_results, previous_results
    ) {
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
    }

  ),

  active = list(

    #' @field f The \code{function} to be optimized.
    f = function(value) {
      if (missing(value)) {
        private$.f
      } else {
        ino_stop("{.var $f} is read only.")
      }
    },

    #' @field f_name The name of the \code{function} to be optimized.
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

    #' @field f_target The name of the target argument, i.e., the argument over
    #' which \code{f} is optimized.
    f_target = function(value) {
      if (missing(value)) {
        private$.f_target
      } else {
        ino_stop("{.var $f_target} is read only.")
      }
    },

    #' @field npar The length of the target argument, i.e., the argument over
    #' which \code{f} is optimized.
    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop("{.var $npar} is read only.")
      }
    },

    #' @field arguments A \code{list} of specified additional arguments for
    #' \code{f}.
    arguments = function(value) {
      if (missing(value)) {
        out <- private$.arguments
        if (length(out) == 0) {
          ino_warn("No function arguments have been specified yet.")
        }
        return(out)
      } else {
        ino_stop(
          "{.var $arguments} is read only.",
          "To set an argument, use {.fun $set_argument}.",
          "To get an argument, use {.fun $get_argument}.",
          "To remove an argument, use {.fun $remove_argument}.",
          "To reset an argument, use {.fun $reset_argument}."
        )
      }
    },

    #' @field true_value The true \code{numeric} optimum value of \code{f}
    #' (if available).
    true_value = function(value) {
      if (missing(value)) {
        private$.true_value
      } else {
        if (is.null(value)) {
          private$.true_value <- NULL
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_value} and {.var true_parameter}.",
            verbose = getOption("ino_verbose", default = FALSE)
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
        }
      }
    },

    #' @field true_parameter The true optimum \code{numeric} parameter vector
    #' of length \code{npar} (if available), i.e., the point where \code{f}
    #' obtains its optimum.
    true_parameter = function(value) {
      if (missing(value)) {
        private$.true_parameter
      } else {
        if (is.null(value)) {
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_parameter}.",
            verbose = getOption("ino_verbose", default = FALSE)
          )
        } else {
          private$.check_target_argument(value)
          private$.true_value <- self$evaluate(at = value)
          digits <- getOption('ino_digits', default = 2)
          ino_status(
            glue::glue(
              "Set true optimum value to",
              "{round(private$.true_value, digits = digits)}.",
              .sep = " "
            ),
            verbose = getOption("ino_verbose", default = FALSE)
          )
          private$.true_parameter <- value
        }
      }
    },

    #' @field optimizer A \code{list} of specified optimizers.
    optimizer = function(value) {
      if (missing(value)) {
        out <- private$.optimizer
        if (length(out) == 0) {
          ino_warn(
            "No optimizer specified.",
            "Please use {.fun $set_optimizer} to specify an optimizer."
          )
        }
        return(out)
      } else {
        self$set_optimizer(value)
      }
    },

    #' @field new_label A \code{character}, a new optimization label that has
    #' not been used yet.
    new_label = function(value) {
      if (missing(value)) {
        default_label <- "unlabeled"
        n <- 1
        while (TRUE) {
          label <- glue::glue("{default_label}_{n}")
          if (!label %in% self$runs$optimization_labels) {
            return(as.character(label))
          } else {
            n <- n + 1
          }
        }
      } else {
        ino_stop("{.var $new_label} is read only.")
      }
    }

  )
)

