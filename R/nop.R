#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a *N*umerical *O*ptimization *P*roblem.
#'
#' @param which_optimizer
#' Select specified numerical optimizers. Either:
#' - \code{"all"}, all specified optimizers,
#' - \code{"removed"}, all removed optimizers,
#' - \code{"active"}, all active optimizers (i.e., not removed),
#' - a \code{character} (vector) of specified optimizer labels,
#' - a \code{numeric} (vector) of optimizer ids (see \code{$print()} output).
#' @param which_run
#' Select saved results of optimization runs. Either:
#' - \code{"all"}, all results,
#' - \code{"last"}, the results from the last optimization,
#' - \code{"failed"}, the results from failed optimization runs,
#' - a \code{character} (vector) of labels specified in \code{$optimize()},
#' - a \code{numeric} (vector) of run ids.
#' @param which_element
#' Select elements of saved optimization results.
#' See \code{$elements_available()} for the names of all available elements.
#' Either:
#' - \code{"all"}, all elements,
#' - \code{"basic"}, the elements
#'   - \code{"value"}, the \code{numeric} value of the found optimum,
#'   - \code{"parameter"}, the parameter \code{vector} at which the optimum value
#'     is obtained,
#' - \code{"default"}, the elements that are saved for all optimization runs by
#'   default, i.e.
#'   - \code{"run"}, the run id,
#'   - \code{"optimizer"}, the label for the optimizer,
#'   - \code{"value"} and \code{"parameter"} (see above),
#'   - \code{"seconds"}, the optimization time in seconds,
#'   - \code{"label"}, the label for the optimization run,
#' - a \code{character} (vector), names of specific elements in the optimization
#'   output.
#' @param only_comparable
#' Either \code{TRUE} to show only comparable results (i.e., results obtained
#' for the original optimization problem), or \code{FALSE} to
#' include all optimization results.
#' @param verbose
#' A \code{logical}, which indicates whether progress/details should be printed.
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) such messages.
#' The default is \code{TRUE}.
#' @param ncores
#' An \code{integer}, the number of CPU cores for parallel computation.
#' The default is \code{1}.
#' You can use \code{parallel::detectCores()} to detect the number of available
#' CPU cores.
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' The default is \code{2}.
#' @param seed
#' An \code{integer}, passed to \code{\link{set.seed}} for reproducibility.
#' Can be \code{NULL} for no seed, which is the default.
#' @param return_results
#' A \code{logical}, which indicates whether the optimization results should be
#' returned as a \code{list}.
#' By default, \code{return_results = FALSE}.
#' @param simplify
#' Only relevant if \code{return_results = TRUE} and \code{runs = 1} and/or
#' only one optimizer is specified.
#' In this case, if \code{simplify = TRUE}, the nested list output
#' of optimization results is flattened if possible.
#' @param save_results
#' A \code{logical}, which indicates whether the optimization results should be
#' saved inside the \code{Nop} object.
#' By default, \code{save_results = TRUE}.
#' @param hide_warnings
#' A \code{logical}.
#' Set to \code{TRUE} (\code{FALSE}) to hide (show) warning messages.
#' @param time_limit
#' An \code{integer}, the time limit in seconds for computations.
#' No time limit if \code{time_limit = NULL} (the default).
#' This currently only works reliably under Windows.
#' @param title
#' A \code{character}, the plot title.
#' @param xlim
#' Passed on to \code{\link[ggplot2]{coord_cartesian}}.
#' @param ylim
#' Passed on to \code{\link[ggplot2]{coord_cartesian}}.
#'
#' @return
#' A \code{Nop} object, which is an R6 class that specifies the numerical
#' optimization problem, stores optimization results, and provides methods
#' for analyzing the results, see the details.
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
#' ## Step 2: Specify numerical optimizer:
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}, which can
#' be created with the \code{\link[optimizeR]{define_optimizer}} function from
#' the \{optimizeR\} package.
#' Two optimizer objects are already available:
#' - \code{\link[optimizeR]{optimizer_nlm}}
#' - \code{\link[optimizeR]{optimizer_optim}}
#'
#' ## Step 3: Test the configuration
#' Call \code{object$test()} to validate your configuration. An overview of
#' the configuration yields the \code{$print()} method.
#'
#' # Optimization
#' Call \code{object$evaluate()} to evaluate the target function at some point.
#' Call \code{object$optimize()} for optimization. See also the methods
#' \code{object$standardize()}, \code{object$reduce()}, and
#' \code{object$continue()} for initialization strategies.
#'
#' # Analysis of the results
#' The following are methods for the analysis of optimization results, with
#' different filter options for optimization runs, optimizers, and elements:
#' - \code{$results()} returns all saved optimization results,
#' - \code{$summary()} summarizes the results,
#' - \code{$optima()} returns a frequency table of identified optima,
#' - \code{$plot()} visualizes the optimization time or value,
#' - \code{$best_parameter()} returns the parameter vector at which the optimum value is obtained,
#' - \code{$best_value()} returns the found optimum value of \code{f},
#' - \code{$closest_parameter()} returns parameter closest to a specified value.
#'
#' @examples
#' Nop$new(f = f_ackley, npar = 2)$
#'   set_optimizer(optimizer_nlm())$
#'   optimize(runs = 100, verbose = FALSE)$
#'   optima()
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

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
        ino_stop(
          "Please specify argument {.var f}.",
          "It is the function to be optimized."
        )
      }
      if (!is.function(f)) {
        ino_stop("Argument {.var f} is not a {.cls function}.")
      }
      if (is.null(formals(f))) {
        ino_stop(
          "The {.cls function} {.var f} should have at least one argument.",
          "Mind that {.var f} is optimized over its first argument."
        )
      }
      if (missing(npar)) {
        ino_stop(
          "Please specify argument {.var npar}.",
          "It is the length of the first argument of {.var f}."
        )
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
    },

    #' @description
    #' Prints details of the numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.
    print = function(digits = getOption("ino_digits", default = 2), ...) {
      print.Nop(x = self, digits = digits, ...)
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
              "Argument {.var <argument_names[i]>} already exists.",
              .open = "<",
              .close = ">"
            ),
            glue::glue(
              "Call {.var $remove_argument({.val <argument_names[i]>})} first.",
              .open = "<",
              .close = ">"
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
    #' Gets an argument value for \code{f}.
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
    #' Sets a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer}, which can be created with the
    #' \code{\link[optimizeR]{define_optimizer}} function from the \{optimizeR\}
    #' package.
    #' @param label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
    #' @return
    #' Invisibly the \code{Nop} object.
    set_optimizer = function(optimizer, label = NULL) {
      if (missing(optimizer)) {
        ino_stop("Please specify argument {.var optimizer}.")
      }
      if (!inherits(optimizer, "optimizer")) {
        ino_stop(
          "Argument {.var optimizer} must be an {.cls optimizer} object.",
          "See {.help optimizeR::define_optimizer} to create such an object.",
          "You can also use {.fun optimizer_nlm} or {.fun optimizer_optim}."
        )
      }
      if (is.null(label)) {
        label <- optimizer$optimizer_name
      }
      is_name(label, error = TRUE)
      if (label %in% names(private$.optimizer)) {
        ino_stop(
          glue::glue(
            "Label {.val <label>} already exists, please choose another one.",
            .open = "<",
            .close = ">"
          ),
          "Note that labels for optimizers must be unique for identification."
        )
      }
      private$.optimizer[[label]] <- structure(optimizer, "active" = TRUE)
      invisible(self)
    },

    #' @description
    #' Removes numerical optimizer.
    #' @return
    #' Invisibly the \code{Nop} object.
    remove_optimizer = function(
      which_optimizer, verbose = getOption("ino_verbose", default = TRUE)
    ) {
      if (missing(which_optimizer)) {
        ino_stop("Please specify {.var which_optimizer}.")
      }
      ids <- private$.get_optimizer_ids(which_optimizer)
      for (id in ids) {
        if (attr(private$.optimizer[[id]], "active")) {
          attr(private$.optimizer[[id]], "active") <- FALSE
          ino_status(
            glue::glue("Removed optimizer {id}."),
            verbose = verbose
          )
        } else {
          ino_warn(glue::glue("Optimizer {id} has already been removed."))
        }
      }
      invisible(self)
    },

    #' @description
    #' Evaluates the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}, the point where the
    #' function is evaluated.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values drawn from a standard normal distribution.
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
      private$.evaluate(
        at = at, time_limit = time_limit, hide_warnings = hide_warnings
      )
    },

    #' @description
    #' Optimizes the function.
    #' @param initial
    #' Specify the initial point where the optimizer should start. Either:
    #' - the \code{character} \code{"random"} (the default) for random initial
    #'   values drawn from a standard normal distribution,
    #' - a \code{numeric} vector of length \code{npar}, the starting point for
    #'   optimization,
    #' - a \code{list} of such vectors (in this case, \code{runs} is set to the
    #'   length of the \code{list}),
    #' - or a \code{function} without any arguments that returns a
    #'   \code{numeric} vector of length \code{npar}.
    #' In all these cases, the same initial values are used for each optimizer.
    #' For more flexibility, a \code{funtion} for \code{initial} can have two
    #' arguments, where the first argument specifies the optimization run,
    #' and the second argument specifies the optimizer.
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' By default, \code{runs = 1}.
    #' @param label
    #' Only relevant if \code{save_results = TRUE}.
    #' In this case, an optional \code{character} to specify a custom label of
    #' the optimization.
    #' By default, \code{label = self$new_label} creates a new label.
    #' Labels can be useful to distinguish optimization runs later.
    #' @param fail_bad_initial
    #' Either \code{TRUE} to immediately fail if \code{initial} contains any
    #' misspecifications (default), or \code{FALSE} to include the failed runs
    #' in the results.
    #' @return
    #' The return value depends on the value of \code{return_results}:
    #' \itemize{
    #'   \item if \code{return_results = FALSE}, invisibly the \code{Nop} object,
    #'   \item if \code{return_results = TRUE}, a nested \code{list} of
    #'         optimization results.
    #'         Each element corresponds to one optimization run
    #'         and is a \code{list} of results for each optimizer.
    #'         The results for each optimizer is a \code{list}, the output of
    #'         \code{\link[optimizeR]{apply_optimizer}}.
    #'         If \code{simplify = TRUE}, the output is flattened if possible.
    #' }
    #' @importFrom parallel makeCluster stopCluster
    #' @importFrom doSNOW registerDoSNOW
    #' @importFrom foreach foreach %dopar% %do%
    optimize = function(
      initial = "random", runs = 1, which_optimizer = "all", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      label = self$new_label, ncores = getOption("ino_ncores", default = 1),
      verbose = getOption("ino_verbose", default = TRUE), simplify = TRUE,
      time_limit = NULL, hide_warnings = TRUE, fail_bad_initial = TRUE
    ) {

      ### input checks
      private$.check_additional_arguments_complete()
      if (is.list(initial)) runs <- length(initial)
      initial <- build_initial(
        initial = initial, npar = private$.npar,
        fail_bad_initial = fail_bad_initial
      )
      is_count(runs)
      is_TRUE_FALSE(save_results)
      is_TRUE_FALSE(return_results)
      is_count(ncores)
      is_TRUE_FALSE(verbose)
      is_time_limit(time_limit)
      is_TRUE_FALSE(hide_warnings)

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

      ### optimization
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer = which_optimizer, only_active = TRUE
      )
      if (length(optimizer_ids) == 0) {
        return(invisible(self))
      }
      ino_seed(seed)
      parallel <- ncores > 1 && runs >= 2 * ncores
      if (parallel) {
        cluster <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cluster))
        doSNOW::registerDoSNOW(cluster)
        if (verbose) pb$tick(0)
        results <- foreach::foreach(
          run_id = 1:runs, .packages = "ino", .export = "private",
          .inorder = TRUE, .options.snow = opts
        ) %dopar% {
          lapply(optimizer_ids, function(optimizer_id) {
            private$.optimize(
              initial = initial(run_id = run_id, optimizer_id = optimizer_id),
              optimizer_id = optimizer_id,
              time_limit = time_limit,
              hide_warnings = hide_warnings
            )
          })
        }
        ### results must be saved outside the loop when parallelized
        if (save_results) {
          for (run in results) {
            run <- private$.label_run(run = run, label = label)
            private$.save_optimization_run(
              run = run, optimizer_ids = optimizer_ids
            )
          }
        }
      } else {
        if (verbose) pb$tick(0)
        results <- foreach::foreach(run_id = 1:runs) %do% {
          run <- lapply(optimizer_ids, function(optimizer_id) {
            private$.optimize(
              initial = initial(run_id = run_id, optimizer_id = optimizer_id),
              optimizer_id = optimizer_id,
              time_limit = time_limit,
              hide_warnings = hide_warnings
            )
          })
          ### results are saved inside the loop
          if (save_results) {
            run <- private$.label_run(run = run, label = label)
            private$.save_optimization_run(
              run = run, optimizer_ids = optimizer_ids
            )
          }
          if (verbose) pb$tick()
          return(run)
        }
      }

      ### save results
      if (save_results) {
        run_ids <- length(private$.results) - length(results) + 1:runs
        private$.runs_last <- run_ids
        if (!label %in% private$.optimization_labels) {
          private$.optimization_labels <- c(private$.optimization_labels, label)
        }
      }

      ### return results
      if (return_results) {
        simplify_results(results = results, simplify = simplify)
      } else {
        invisible(self)
      }
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
      at = rnorm(self$npar), which_optimizer = "active", time_limit = 10,
      verbose = getOption("ino_verbose", default = TRUE),
      digits = getOption("ino_digits", default = 2)
    ) {
      private$.check_target_argument(at)
      optimizer_ids <- suppressWarnings(
        private$.get_optimizer_ids(
          which_optimizer = which_optimizer, only_active = TRUE
        )
      )
      test_nop(
        x = self, at = at, optimizer_ids = optimizer_ids,
        time_limit = time_limit, verbose = verbose, digits = digits
      )
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
    #' Currently, only \code{by_column = TRUE} is implemented.
    #' @param center
    #' Passed to \code{\link[base]{scale}}.
    #' Default is \code{TRUE}.
    #' @param scale
    #' Passed to \code{\link[base]{scale}}.
    #' Default is \code{TRUE}.
    #' @param ignore
    #' A \code{integer} (vector) of column indices (or row indices if
    #' \code{by_column = FALSE}) to not standardize.
    #' @return
    #' Invisibly the \code{Nop} object.
    standardize = function(
      argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
      ignore = integer(), verbose = getOption("ino_verbose", default = TRUE)
    ) {
      original_argument <- self$get_argument(argument_name)
      standardized_argument <- standardize_argument(
        argument = original_argument, by_column = by_column, center = center,
        scale = scale, ignore = ignore
      )
      ino_status(
        glue::glue("Standardized `{argument_name}`."),
        verbose = verbose
      )
      private$.arguments[[argument_name]] <- standardized_argument
      private$.save_original_argument(original_argument, argument_name)
      invisible(self)
    },

    #' @description
    #' Reduces the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be reduced.
    #' @param by_row
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to reduce row-wise (default) or
    #' \code{FALSE} to reduce column-wise.
    #' Currently, only \code{by_row = TRUE} is implemented.
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
    #' if \code{by_row = FALSE}) to ignore for clustering.
    #' @return
    #' Invisibly the \code{Nop} object.
    reduce = function(
      argument_name, by_row = TRUE, how = "random", proportion = 0.5,
      centers = 2, ignore = integer(), seed = NULL,
      verbose = getOption("ino_verbose", default = TRUE)
    ) {
      original_argument <- self$get_argument(argument_name)
      reduced_argument <- subset_argument(
        argument = original_argument, by_row = by_row, how = how,
        proportion = proportion, centers = centers, ignore = ignore,
        seed = seed
      )
      ino_status(
        glue::glue(
          "Reduced '{argument_name}' from ",
          if (is.vector(original_argument)) {
            length_old <- length(original_argument)
            length_new <- length(reduced_argument)
            "{length_old} to {length_new} {how} element(s)."
          } else {
            if (by_row) {
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
        ),
        verbose = verbose
      )
      private$.arguments[[argument_name]] <- reduced_argument
      private$.save_original_argument(original_argument, argument_name)
      invisible(self)
    },

    #' @description
    #' Resets an additional argument for \code{f} after transformation with
    #' \code{$standardize()} or \code{$reduce()}.
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
      private$.reset_original_argument(argument_name, verbose = verbose)
      invisible(self)
    },

    #' @description
    #' Continues optimization runs, e.g., with a transformed parameter.
    #' @return
    #' The same as the return value of \code{$optimize()}.
    continue = function(
      which_run = "last", which_optimizer = "active", seed = NULL,
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
        hide_warnings = hide_warnings, fail_bad_initial = FALSE
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
    #' Returns the number of saved optimization runs.
    #' @return
    #' An \code{integer}.
    number_runs = function(
      which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {
      results <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = "all", only_comparable = only_comparable,
        simplify = FALSE
      )
      sum(sapply(results, length) > 0)
    },

    #' @description
    #' Returns names of available elements per optimizer.
    #' @return
    #' A \code{list}.
    elements_available = function(which_optimizer = "all") {
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer = which_optimizer, only_active = FALSE
      )
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
    #' Deletes optimization results.
    #' @return
    #' Invisibly the \code{Nop} object.
    clear = function(
      which_run, which_optimizer = "all", which_element = "all"
    ) {
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
      which_element = "basic", which_run = "all", which_optimizer = "all",
      digits = getOption("ino_digits", default = 2), only_comparable = FALSE,
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
    #' - \code{"label"} to group by optimization label
    #' - \code{"optimizer"} to group by optimizer
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
        only_comparable = only_comparable
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
      nruns <- self$number_runs(
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      )
      if (nruns == 0) {
        return(invisible(NULL))
      }
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
      nruns <- self$number_runs(
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      )
      if (nruns == 0) {
        return(invisible(NULL))
      }
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
    },

    #' @description
    #' Extracts the parameter vector that led to a function value closest
    #' (in absolute value) to \code{value}.
    #' Note that this parameter vector is not necessarily unique.
    #' @param value
    #' A single \code{numeric}.
    #' @return
    #' A \code{numeric} vector of length \code{self$npar}.
    #' The output has two attributes:
    #' - \code{run}, the run id that led to this parameter vector,
    #' - \code{optimizer}, the optimizer that led to this parameter vector.
    closest_parameter = function(
      value, which_run = "all", which_optimizer = "all",
      only_comparable = TRUE, digits = getOption("ino_digits", default = 2)
    ) {
      is_number(value)
      summary <- self$summary(
        which_element = "default", which_run = which_run,
        which_optimizer = which_optimizer, digits = Inf,
        only_comparable = only_comparable
      )
      index <- which.min(abs(value - summary$value))
      if (length(index) == 0) {
        ino_stop("No parameter vector found.")
      }
      parameter <- unlist(summary[index, "parameter"], use.names = FALSE)
      structure(
        round(parameter, digits = digits),
        "run" = summary[index, "run"],
        "optimizer" = summary[index, "optimizer"]
      )
    }

  ),
  private = list(

    ### private variables
    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .arguments = list(),
    .original_arguments = list(),
    .true_parameter = NULL,
    .true_value = NULL,
    .minimized = TRUE,
    .optimizer = list(),
    .results = list(),
    .runs_last = integer(),
    .optimization_labels = character(),

    ### checks if argument for target function exists
    .check_additional_argument_exists = function(argument_name) {
      stopifnot(is_name(argument_name))
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue(
            "Argument {.var <argument_name>} is not yet specified.",
            .open = "<",
            .close = ">"
          ),
          glue::glue(
            "Call {.var $set_argument({.val <argument_name>} = ...)} first.",
            .open = "<",
            .close = ">"
          )
        )
      } else {
        invisible(TRUE)
      }
    },

    ### checks if all required arguments for function are specified
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

    ### checks supplied value for target argument
    .check_target_argument = function(target_arg) {
      arg_name <- deparse(substitute(target_arg))
      if (!is.vector(target_arg) || !is.numeric(target_arg)) {
        ino_stop(
          glue::glue(
            "Input {.var <arg_name>} must be a {.cls numeric}.",
            .open = "<",
            .close = ">"
          )
        )
      }
      if (length(target_arg) != self$npar) {
        ino_stop(
          glue::glue(
            "Input {.var <arg_name>} must be of length {<self$npar>}.",
            .open = "<",
            .close = ">"
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
      if (identical(which_element, "basic")) {
        which_element <- c("value", "parameter")
      }
      if (identical(which_element, "default")) {
        which_element <- c(
          "run", "optimizer", "value", "parameter", "seconds", "label"
        )
      }
      if (!all(sapply(which_element, is_name, error = FALSE))) {
        ino_stop(
          "Input {.var which_element} is misspecified.",
          "It can be {.val all}, {.val basic}, or {.val default}.",
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

    ### cheap function evaluation
    .evaluate = function(at, time_limit, hide_warnings) {
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
          msg <- e$message
          tl <- grepl("reached elapsed time limit|reached CPU time limit", msg)
          return(
            list(
              "value" = NA_real_,
              "parameter" = NA_real_,
              "seconds" = NA_real_,
              "initial" = initial,
              "error" = if (tl) "time limit reached" else msg
            )
          )
        }
      )
    },

    ### label one optimization run
    .label_run = function(run, label) {
      lapply(
        X = run,
        FUN = function(x) {
          x[["label"]] <- label
          return(x)
        }
      )
    },

    ### save results of one optimization run inside `Nop` object
    .save_optimization_run = function(
      run, optimizer_ids, run_id = length(private$.results) + 1
    ) {
      comparable <- length(private$.original_arguments) == 0
      optimizer <- private$.optimizer
      optimizer_labels <- names(optimizer)
      for (i in seq_along(optimizer_ids)) {
        run[[i]][["run"]] <- run_id
        run[[i]][["optimizer"]] <- optimizer_labels[optimizer_ids[i]]
        run[[i]][["comparable"]] <- comparable
      }
      full_run <- replicate(length(optimizer), list())
      for (i in seq_along(optimizer_ids)) {
        full_run[[optimizer_ids[i]]] <- run[[i]]
      }
      private$.results[[run_id]] <- full_run
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
    },

    ### save original arguments before transformation
    .save_original_argument = function(original_argument, argument_name) {
      if (is.null(private$.original_arguments[[argument_name]])) {
        private$.original_arguments[[argument_name]] <- original_argument
      }
    },

    ### reset transformed argument to original argument
    .reset_original_argument = function(argument_name, verbose = FALSE) {
      if (!is.null(private$.original_arguments[[argument_name]])) {
        original_argument <- private$.original_arguments[[argument_name]]
        private$.arguments[[argument_name]] <- original_argument
        private$.original_arguments[[argument_name]] <- NULL
        ino_status(
          glue::glue("Reset `{argument_name}`."),
          verbose = verbose
        )
      }
    },

    ### get ids of optimization runs
    .get_run_ids = function(which_run) {
      if (length(private$.results) == 0) {
        ino_warn(
          "No optimization results saved yet.",
          "Please call {.var $optimize(save_results = TRUE)}."
        )
        return(integer(0))
      }
      if (is.character(which_run)) {
        if (identical(which_run, "all")) {
          ids <- seq_along(private$.results)
        } else if (identical(which_run, "last")) {
          ids <- private$.runs_last
        } else {
          ids <- which(sapply(lapply(private$.results, `[[`, 1), `[[`, "label") %in% which_run)
        }
      } else if (is.numeric(which_run)) {
        ids <- which(seq_along(private$.results) %in% which_run)
      } else {
        ids <- integer(0)
      }
      if (length(ids) == 0) {
        ino_warn(
          "Please check argument {.var which_run}.",
          "Your input selects no saved result."
        )
        return(integer(0))
      }
      return(ids)
    },

    ### get ids of optimizers
    .get_optimizer_ids = function(which_optimizer, only_active = FALSE) {
      stopifnot(isTRUE(only_active) || isFALSE(only_active))
      optimizer <- private$.optimizer
      ids <- seq_along(optimizer)
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer specified yet.",
          "Use {.fun $set_optimizer} to specify an optimizer."
        )
        return(integer(0))
      }
      optimizer_active <- which(sapply(optimizer, attr, "active"))
      if (only_active) {
        ids <- intersect(ids, optimizer_active)
      }
      if (identical(which_optimizer, "all")) {
        return(ids)
      }
      if (identical(which_optimizer, "active")) {
        ids <- optimizer_active
      } else if (identical(which_optimizer, "removed")) {
        ids <- setdiff(ids, optimizer_active)
      } else if (is.character(which_optimizer)) {
        ids <- which(names(private$.optimizer) %in% which_optimizer)
      } else if (is.numeric(which_optimizer)) {
        ids <- which(seq_along(private$.optimizer) %in% which_optimizer)
      } else {
        ino_stop(
          "Argument {.var which_optimizer} is misspecified.",
          "Please see the documentation for possible values."
        )
      }
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer selected."
        )
        return(integer(0))
      }
      return(ids)
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

    #' @field minimized A \code{logical}, set to \code{TRUE} (default) to
    #' show best minimum in \code{$best_value()}, \code{$best_parameter()}, and
    #' \code{$optima()}.
    minimized = function(value) {
      if (missing(value)) {
        private$.minimized
      } else {
        if (!isTRUE(value) && !isFALSE(value)) {
          ino_stop("{.var minimized} must be {.val TRUE} or {.val FALSE}.")
        }
        private$.minimized <- value
      }
    },

    #' @field optimizer A \code{list} of specified optimizers.
    optimizer = function(value) {
      if (missing(value)) {
        out <- private$.optimizer
        if (length(out) == 0) {
          ino_warn(
            "No optimizer specified yet.",
            "Please use {.fun $set_optimizer} to specify an optimizer."
          )
        } else {
          active <- suppressWarnings(
            private$.get_optimizer_ids(which_optimizer = "active")
          )
          if (length(active) == 0) {
            ino_warn("No optimizer currently active.")
          }
        }
        return(out)
      } else {
        ino_stop(
          "{.var $optimizer} is read only.",
          "To set an optimizer, use {.fun $set_optimizer}.",
          "To remove an optimizer, use {.fun $remove_optimizer}."
        )
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
          if (!label %in% private$.optimization_labels) {
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

