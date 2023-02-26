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
#' TODO
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
#' Set a seed for reproducibility.
#' No seed by default.
#' @param return_results
#' A \code{logical}, which indicates whether the results should be returned as
#' a \code{list}.
#' By default, \code{return_results = FALSE}.
#' @param simplify
#' Only relevant if \code{return_results = TRUE} and \code{runs = 1} and/or
#' only one optimizer is specified.
#' In this case, if \code{simplify = TRUE} (default), the nested list output
#' of optimization results is flattened.
#' @param save_results
#' A \code{logical}, which indicates whether the results should be saved
#' inside the \code{Nop} object.
#' By default, \code{save_results = TRUE}.
#' @param hide_warnings
#' A \code{logical}.
#' Set to \code{TRUE} (\code{FALSE}) to hide (show) warnings.
#' @param time_limit
#' An \code{integer}, the time limit in seconds for computations.
#' No time limit if \code{time_limit = NULL}.
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
#' - \code{$results()} returns a \code{list} of optimization results.
#' - \code{$summary()} returns a \code{data.frame} which summarizes the results.
#' - \code{$optima()} returns a \code{table} of identified optima.
#' - \code{$plot()} visualizes the optimization times.
#' - \code{$best_parameter()} returns the optimal found parameter vector.
#' - \code{$best_value()} returns the optimal found function value.
#' - \code{$closest_parameter()} returns a parameter vector closest to a
#'   specified value.
#'
#' @examples
#' Nop$new(f = f_ackley, npar = 2)$
#'   set_optimizer(optimizer_nlm())$
#'   optimize(runs = 100)$
#'   optima()
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Create a new \code{Nop} (numerical optimization problem) object.
    #' @param f
    #' The \code{function} to be optimized.
    #' It is optimized over its first argument, which should be a \code{numeric}
    #' vector of length \code{npar}.
    #' @param npar
    #' An \code{integer}, the length of the first argument of \code{f} (the
    #' argument over which \code{f} is optimized).
    #' @param ...
    #' Optionally additional named arguments for \code{f}.
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
        ino_stop(
          "Argument {.var f} is not a function.",
          "Please specify a {.cls function} object as argument {.var f}."
        )
      }
      if (is.null(formals(f))) {
        ino_stop(
          "The function {.var f} should have at least one argument.",
          "Mind that {.var f} is optimized over its first argument.",
          "It should be a {.cls numeric} vector of length {.var npar}."
        )
      }
      if (missing(npar)) {
        ino_stop(
          "Please specify argument {.var npar}.",
          "It is the length of the first argument of {.var f}."
        )
      }
      if (!is_number(npar)) {
        ino_stop(
          "Argument {.var npar} is not a positive {.cls integer}.",
          "Please specify {.var npar} as the first argument length of {.var f}."
        )
      }
      private$.f <- f
      f_name <- deparse(substitute(f))
      if (!is_name(f_name)) {
        f_name <- "unnamed_function"
        ino_warn(
          "Function {.var f} is unnamed.",
          "You can set a name via {.var object$f_name <- {.val name}}."
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
    #' Prints details of numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.
    print = function(digits = getOption("ino_digits", default = 2), ...) {
      print.Nop(x = self, digits = digits, ...)
    },

    #' @description
    #' Set additional arguments for \code{f}.
    #' @param ...
    #' Optionally additional arguments for \code{f}.
    #' @importFrom glue glue
    #' @return
    #' Invisibly the \code{Nop} object.
    set_argument = function(...) {
      arguments <- list(...)
      if (length(arguments) == 0) {
        ino_stop("Please specify an argument for {.var f}.")
      }
      argument_names <- names(arguments)
      argument_names[which(is.null(argument_names))] <- ""
      for (i in seq_along(arguments)) {
        if (nchar(argument_names[i]) < 1) {
          ino_stop(glue::glue("Please name argument {i}."))
        }
        if (argument_names[i] %in% names(private$.arguments)) {
          ino_stop(
            glue::glue("Argument `{argument_names[i]}` already exists."),
            glue::glue(
              "Please call `$remove_argument('{argument_names[i]}')` first."
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
    #' Get value of an argument for \code{f}.
    #' @param argument_name
    #' A \code{character}, the argument to extract.
    get_argument = function(argument_name) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      if (!is_name(argument_name)) {
        ino_stop(
          "Input {.var argument_name} must be a single {.cls character}."
        )
      }
      private$.check_additional_argument_exists(argument_name)
      private$.arguments[[argument_name]]
    },

    #' @description
    #' Remove additional arguments for \code{f}.
    #' @param argument_name
    #' A \code{character} (vector), the argument(s) to remove.
    remove_argument = function(argument_name) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      if (!all(sapply(argument_name, is_name))) {
        ino_stop(
          "Input {.var argument_name} must be a (vector of) name(s)."
        )
      }
      for (i in seq_along(argument_name)) {
        private$.check_additional_argument_exists(argument_name[i])
        arg_id <- which(names(private$.arguments) == argument_name[i])
        private$.arguments[arg_id] <- NULL
      }
      invisible(self)
    },

    #' @description
    #' Reset an additional argument for \code{f} after transformation with
    #' \code{$standardize()} or \code{$reduce()}.
    #' @param argument_name
    #' A \code{character} (vector), the argument(s) to reset.
    reset_argument = function(
      argument_name, verbose = getOption("ino_verbose", default = TRUE)
    ) {
      if (missing(argument_name)) {
        ino_stop("Please specify {.var argument_name}.")
      }
      if (!is.character(argument_name)) {
        ino_stop(
          "Input {.var argument_name} must be a {.cls character} (vector)."
        )
      }
      for (arg in argument_name) {
        private$.check_additional_argument_exists(argument_name)
        private$.reset_original_argument(arg, verbose = verbose)
      }
      invisible(self)
    },

    #' @description
    #' Evaluate the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values.
    #' @return
    #' Either:
    #' - a \code{numeric} value, the function value at \code{at},
    #' - \code{"time limit reached"} if the time limit was reached,
    #' - the error message if the evaluation failed.
    evaluate = function(
      at = rnorm(self$npar), time_limit = NULL, hide_warnings = FALSE
    ) {

      ### input checks
      private$.check_additional_arguments_complete()
      private$.check_target_argument(at, arg_name = "at")
      if (!is.null(time_limit)) {
        if (!is_number(time_limit)) {
          ino_stop(
            "Argument {.var time_limit} is not a positive {.cls integer}.",
            "It should be a number of seconds.",
            "Alternatively, it can be {.val NULL} for no time limit."
          )
        }
      }
      if (!isTRUE(hide_warnings) && !isFALSE(hide_warnings)) {
        ino_stop(
          "Input {.var hide_warnings} must be {.val TRUE} or {.val FALSE}."
        )
      }

      ### evaluation
      private$.evaluate(
        at = at, time_limit = time_limit, hide_warnings = hide_warnings
      )
    },

    #' @description
    #' Set a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer}, which can be created with the
    #' \code{\link[optimizeR]{define_optimizer}} function from the \{optimizeR\}
    #' package.
    #' @param label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
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
      if (!is_name(label)) {
        ino_stop(
          "Argument {.var label} must be a single {.cls character}."
        )
      }
      if (label %in% names(private$.optimizer)) {
        ino_stop(
          glue::glue("Label `{label}` already exists, choose another one."),
          "Note that labels for optimizers must be unique for identification."
        )
      }
      private$.optimizer[[label]] <- optimizer
      invisible(self)
    },

    #' @description
    #' Remove numerical optimizer.
    remove_optimizer = function(which_optimizer) {
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      for (id in optimizer_ids) {
        if (!check_optimizer_active(private$.optimizer, id)) {
          ino_warn(
            glue::glue("Optimizer {id} already has been removed.")
          )
        } else {
          private$.optimizer[[id]] <- glue::glue(
            "Optimizer {names(private$.optimizer)[id]} has been removed."
          )
          names(private$.optimizer)[id] <- ""
          ino_status(glue::glue("Removed optimizer {id}."))
        }
      }
      invisible(self)
    },

    #' @description
    #' Optimize the function.
    #' @param initial
    #' Specify the initial point where the optimizer should start. Either:
    #' - the \code{character} \code{"random"} (the default) for random initial
    #'   values drawn from a standard normal distribution,
    #' - a \code{numeric} vector of length \code{npar}, the starting point for
    #'   optimization,
    #' - a \code{list} of such vectors (in this case, \code{runs} is set to the
    #'   length of the \code{list}),
    #' - or a \code{function} that returns a \code{numeric} vector of length
    #'   \code{npar} (the function can but not has to have a single integer
    #'   argument that specifies the optimization run).
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' By default, \code{runs = 1}.
    #' @param label
    #' Only relevant if \code{save_results = TRUE}.
    #' In this case, optionally a \code{character} for a custom label of the
    #' optimization.
    #' By default, \code{label = self$new_label} creates a new label.
    #' Labels can be useful to distinguish optimization runs later.
    #' @param reset_arguments_afterwards
    #' A \code{logical}. Set to \code{TRUE} to reset all transformed
    #' arguments after the optimization runs.
    #' By default, \code{reset_arguments_afterwards = FALSE}.
    #' Alternatively, reset arguments using \code{$reset_argument()}.
    #' @return
    #' The return value depends on the value of \code{return_results}:
    #' - if \code{return_results = FALSE}, invisibly the \code{Nop} object,
    #' - if \code{return_results = TRUE}, the output of
    #'   \code{\link[optimizeR]{apply_optimizer}}.
    #' @importFrom parallel makeCluster stopCluster
    #' @importFrom doSNOW registerDoSNOW
    #' @importFrom foreach foreach %dopar% %do%
    optimize = function(
      initial = "random", runs = 1, which_optimizer = "all", seed = NULL,
      return_results = FALSE, save_results = TRUE,
      label = self$new_label, ncores = getOption("ino_ncores", default = 1),
      verbose = getOption("ino_verbose", default = TRUE), simplify = TRUE,
      reset_arguments_afterwards = TRUE, time_limit = NULL, hide_warnings = TRUE
    ) {

      ### input checks
      private$.check_additional_arguments_complete()
      initial <- build_initial(initial = initial, npar = private$.npar)
      if (!is_number(runs)) {
        ino_stop("Input {.var runs} must be a positive {.cls integer}.")
      }
      if (!isTRUE(save_results) && !isFALSE(save_results)) {
        ino_stop(
          "Input {.var save_results} must be {.val TRUE} or {.val FALSE}."
        )
      }
      if (!isTRUE(return_results) && !isFALSE(return_results)) {
        ino_stop(
          "Input {.var return_results} must be {.val TRUE} or {.val FALSE}."
        )
      }
      if (!is_number(ncores)) {
        ino_stop("Input {.var ncores} must be a positive {.cls integer}.")
      }
      if (!isTRUE(verbose) && !isFALSE(verbose)) {
        ino_stop(
          "Input {.var verbose} must be {.val TRUE} or {.val FALSE}."
        )
      }
      if (!isTRUE(reset_arguments_afterwards) && !isFALSE(reset_arguments_afterwards)) {
        ino_stop(
          "Input {.var reset_arguments_afterwards} must be {.val TRUE} or {.val FALSE}."
        )
      }
      if (!is.null(time_limit)) {
        if (!is_number(time_limit)) {
          ino_stop(
            "Argument {.var time_limit} is not a positive {.cls integer}.",
            "It should be a number of seconds.",
            "Alternatively, it can be {.val NULL} for no time limit."
          )
        }
      }
      if (!isTRUE(hide_warnings) && !isFALSE(hide_warnings)) {
        ino_stop(
          "Input {.var hide_warnings} must be {.val TRUE} or {.val FALSE}."
        )
      }

      ### build progress bar
      pb <- progress::progress_bar$new(
        format = ":current of :total, ETA :eta", total = runs,
        clear = FALSE
      )
      opts <- structure(
        list(function(n) {
          if (verbose) if (pb$.__enclos_env__$private$total > 1) pb$tick()
        }),
        names = "progress"
      )

      ### optimization
      # TODO: cannot have optimizer = all here
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (!is.null(seed)) {
        set.seed(seed)
      }
      parallel <- ncores > 1 && runs >= 2 * ncores
      if (parallel) {
        cluster <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cluster))
        doSNOW::registerDoSNOW(cluster)
        results <- foreach::foreach(
          run = 1:runs, .packages = "ino", .export = "private",
          .inorder = FALSE, .options.snow = opts
        ) %dopar% {
          lapply(optimizer_ids, function(i) {
            private$.optimize(
              initial = initial(run),
              optimizer_id = i,
              time_limit = time_limit,
              hide_warnings = hide_warnings
            )
          })
        }
      } else {
        results <- foreach::foreach(run = 1:runs) %do% {
          pb$tick()
          lapply(optimizer_ids, function(i) {
            private$.optimize(
              initial = initial(run),
              optimizer_id = i,
              time_limit = time_limit,
              hide_warnings = hide_warnings
            )
          })
        }
      }

      ### save results
      if (save_results) {
        private$.runs_last <- run_ids <- length(private$.results) + 1:runs
        private$.save_results(
          results = results, run_ids = run_ids, optimizer_ids = optimizer_ids,
          label = label
        )
      }

      ### reset transformed arguments
      if (reset_arguments_afterwards) {
        lapply(
          names(private$.original_arguments),
          private$.reset_original_argument,
          verbose = verbose
        )
      }

      ### return results
      if (return_results) {
        simplify_results(results = results, simplify = simplify)
      } else {
        invisible(self)
      }
    },

    #' @description
    #' Test the configuration of a \code{Nop} object.
    #' @param at
    #' A \code{numeric} of length \code{npar}, the point at which the
    #' function \code{f} and the specified optimizer are tested.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values.
    #' @return
    #' Invisibly \code{TRUE} if the tests are successful.
    test = function(
      at = rnorm(self$npar), which_optimizer = "active", time_limit = 10,
      verbose = getOption("ino_verbose", default = TRUE),
      digits = getOption("ino_digits", default = 2)
      ) {

      ### input checks
      # TODO: cannot have which_optimizer = "all" here
      private$.check_target_argument(at, "at")
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      optimizer_selected <- length(optimizer_ids) > 0
      if (!isTRUE(verbose) && !isFALSE(verbose)) {
        ino_stop(
          "Input {.var verbose} must be {.val TRUE} or {.val FALSE}."
        )
      }

      ### test configurations
      ino_status("Test configuration", verbose = verbose)
      ino_success(
        glue::glue("Function specified: {private$.f_name}"), verbose = verbose
      )
      ino_success(
        glue::glue(
          "Target argument specified: ",
          "{private$.f_target} (length {private$.npar})"
        ), verbose = verbose
      )
      if (optimizer_selected) {
        ino_success(
          glue::glue(
            "Optimizer specified: ",
            "{paste(names(private$.optimizer)[optimizer_ids], collapse = ', ')}"
          ), verbose = verbose
        )
      }
      ino_success(
        glue::glue(
          "Test values specified: ",
          {paste(round(at, digits = digits), collapse = ' ')}
        ), verbose = verbose
      )

      ### test function call
      ino_status("Test function call", verbose = verbose)
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
            "Test function call returned:",
            out
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

      ### test optimization
      if (!optimizer_selected) {
        ino_warn(
          "No optimizer specified, testing optimizer is skipped.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
        )
      } else {
        ino_status("Test optimization", verbose = verbose)
        for (i in seq_along(optimizer_ids)) {
          out <- self$optimize(
            initial = at, runs = 1, which_optimizer = i, seed = NULL,
            return_results = TRUE, save_results = FALSE, ncores = 1,
            verbose = FALSE, simplify = TRUE, time_limit = time_limit,
            hide_warnings = TRUE
          )
          if (!is.null(out$error)) {
            if (identical(out$error, "time limit reached")) {
              ino_warn(
                glue::glue(
                  "Time limit of {time_limit}s was reached in the optimization."
                ),
                "Consider increasing {.var time_limit}."
              )
            } else {
              ino_stop(
                "Optimization returned an error:",
                out$error
              )
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
                      "Return {value}: ",
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
    #' Standardize the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be
    #' standardized. The argument must be \code{numeric}.
    #' @param by_column
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to standardize column-wise (default) or
    #' \code{FALSE} to standardize row-wise.
    #' @param center
    #' Passed to \code{\link[base]{scale}}.
    #' Default is \code{TRUE}.
    #' @param scale
    #' Passed to \code{\link[base]{scale}}.
    #' Default is \code{TRUE}.
    #' @param ignore
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, a \code{integer} (vector) of column indices (or row indices
    #' if \code{by_column = FALSE}) to not standardize.
    standardize = function(
      argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
      ignore = integer(), verbose = getOption("ino_verbose", default = TRUE)
    ) {

      ### input checks
      if (missing(argument_name)) {
        ino_stop(
          "Please specify {.var argument_name}.",
          "It is the name of the argument to be standardized."
        )
      }
      argument <- self$get_argument(argument_name)
      original_argument <- argument
      attr_argument <- attributes(argument)
      if (is.vector(argument) && length(argument) > 1) {
        argument <- as.data.frame(argument)
        vector_flag <- TRUE
        by_column <- TRUE
        ignore <- integer()
      } else if (is.data.frame(argument) || is.matrix(argument)) {
        if (!isTRUE(by_column) && !isFALSE(by_column)) {
          ino_stop(
            "Argument {.var by_column} must be {.val TRUE} or {.val FALSE}."
          )
        }
        if (!is.numeric(ignore) || !all(sapply(ignore, is_number))) {
          ino_stop(
            "Argument {.var ignore} must be an index vector."
          )
        }
        vector_flag <- FALSE
      } else {
        ino_stop(
          glue::glue(
            "Argument `{argument_name}` is not suited for standardization."
          )
        )
      }

      ### standardizing
      if (!by_column) {
        argument <- t(argument)
      }
      if (length(ignore) > 0) {
        argument[, -ignore] <- scale(argument[, -ignore], center = center, scale = scale)
      } else {
        argument <- scale(argument, center = center, scale = scale)
      }
      if (vector_flag) {
        argument <- argument[, 1]
      } else {
        if (!by_column) {
          argument <- t(argument)
        }
        if (is.data.frame(original_argument)) {
          argument <- as.data.frame(argument)
        }
      }
      attributes(argument) <- attr_argument

      ### report standardization
      ino_status(
        glue::glue("Standardized {class(argument)[1]} `{argument_name}`."),
        verbose = verbose
      )

      ### save standardized argument
      private$.arguments[[argument_name]] <- argument

      ### temporally save original argument
      private$.save_original_argument(original_argument, argument_name)

      ### return object
      invisible(self)
    },

    #' @description
    #' Reduce the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be reduced.
    #' @param by_row
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
    #' Only relevant, if \code{how = "(dis)similar"} and the argument
    #' \code{argument_name} is a \code{matrix} or a \code{data.frame}.
    #' In that case a \code{integer} (vector) of row indices (or column indices
    #' if \code{by_row = FALSE}) to ignore for clustering.
    reduce = function(
      argument_name, by_row = TRUE, how = "random", proportion = 0.5,
      centers = 2, ignore = integer(), seed = NULL,
      verbose = getOption("ino_verbose", default = TRUE)
    ) {

      ### input checks
      if (missing(argument_name)) {
        ino_stop(
          "Please specify {.var argument_name}.",
          "It is the name of the argument to be reduced."
        )
      }
      argument <- self$get_argument(argument_name)
      original_argument <- argument
      if (!is_name(how)) {
        ino_stop(
          "Input {.var how} must be a single {.cls character}."
        )
      }
      if (!how %in% c("random", "first", "last", "similar", "dissimilar")) {
        ino_stop(
          "Argument {.var how} is misspecified.",
          paste(
            "It must be one of {.val random}, {.val first}, {.val last},",
            "{.val similar} or {.val dissimilar}."
          )
        )
      }
      if (!(is.numeric(proportion) && length(proportion) == 1 &&
            proportion > 0 && proportion < 1)) {
        ino_stop(
          "Argument {.var proportion} is misspecified.",
          "It must be a {.cls numeric} between 0 and 1."
        )
      }
      if (!is.null(seed)) {
        set.seed(seed)
      }
      if (is.vector(argument) && length(argument) > 1) {
        argument <- as.data.frame(argument)
        vector_flag <- TRUE
        by_row <- TRUE
        ignore <- integer()
      } else if (is.data.frame(argument) || is.matrix(argument)) {
        if (!isTRUE(by_row) && !isFALSE(by_row)) {
          ino_stop(
            "Argument {.var by_row} must be {.val TRUE} or {.val FALSE}."
          )
        }
        if (how %in% c("similar", "dissimilar")) {
          if (!is.numeric(ignore) || !all(sapply(ignore, is_number))) {
            ino_stop(
              "Argument {.var ignore} must be an index vector."
            )
          }
        }
        vector_flag <- FALSE
      } else {
        ino_stop(
          glue::glue(
            "Argument `{argument_name}` is not suited for reduction."
          )
        )
      }

      ### subsetting
      if (!by_row) {
        argument <- t(argument)
      }
      n <- nrow(argument)
      m <- ceiling(n * proportion)
      if (how == "random") {
        ind <- sort(sample.int(n, m))
      } else if (how == "first") {
        ind <- seq_len(m)
      } else if (how == "last") {
        ind <- tail(seq_len(n), m)
      } else {
        stopifnot(how == "similar" || how == "dissimilar")
        argument_ign <- argument
        if (length(ignore) > 0) {
          argument_ign <- argument_ign[, -ignore, drop = FALSE]
        }
        cluster <- stats::kmeans(argument_ign, centers = centers)$cluster
        ind <- integer(0)
        if (how == "similar") {
          i <- 1
          while (length(ind) < m && i <= centers) {
            ind_i <- which(cluster == i)
            ind <- c(ind, ind_i[seq_len(min(m - length(ind), length(ind_i)))])
            i <- i + 1
          }
        } else if (how == "dissimilar") {
          ind_cluster <- split(1:n, cluster)
          i <- 0
          while (length(ind) < m) {
            i_mod <- i %% centers + 1
            if (length(ind_cluster[[i_mod]]) == 0) next
            ind <- c(ind, ind_cluster[[i_mod]][1])
            ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
            i <- i + 1
          }
        }
        ind <- sort(ind)
      }
      argument <- argument[ind, , drop = FALSE]
      if (vector_flag) {
        argument <- argument[, 1]
      } else {
        if (!by_row) {
          argument <- t(argument)
        }
      }

      ### report reduction
      ino_status(
        glue::glue(
          "Reduced `{class(argument)[1]}` '{argument_name}' from ",
          if (is.vector(argument)) {
            "length {length(original_argument)} to {length(argument)} {how} element(s)."
          } else {
            if (by_row) {
              glue::glue(
                "{nrow(original_argument)} to {nrow(argument)} {how} row(s).",
              )
            } else {
              glue::glue(
                "{ncol(original_argument)} to {ncol(argument)} {how} column(s).",
              )
            }
          }
        ),
        verbose = verbose
      )

      ### save reduced argument
      private$.arguments[[argument_name]] <- argument

      ### temporally save original argument
      private$.save_original_argument(original_argument, argument_name)

      ### return object
      invisible(self)
    },

    #' @description
    #' Continue last optimization runs, e.g., with a transformed parameter.
    continue = function(
      save_results = TRUE, return_results = FALSE,
      time_limit = NULL, hide_warnings = TRUE
    ) {

      ### input checks
      if (!isTRUE(hide_warnings) && !isFALSE(hide_warnings)) {
        ino_stop(
          "Input {.var hide_warnings} must be {.val TRUE} or {.val FALSE}."
        )
      }

      ### continue optimization
      results_old <- private$.results[private$.runs_last]
      results <- list()
      for (i in 1:length(results_old)) {
        results[[i]] <- list()
        results_old <- results_old[[i]]
        for (o in 1:length(results_old)) {
          results_old_o <- results_old[[o]]
          initial <- results_old_o$parameter
          results_new <- private$.optimize(
            initial = initial,
            optimizer_id = o,
            time_limit = time_limit,
            hide_warnings = hide_warnings
          )
          results_new$label <- results_old_o$label
          results_new$optimizer <- results_old_o$optimizer
          results_new$seconds <- results_new$seconds + results_old_o$seconds
          results_new$comparable <- length(private$.original_arguments) == 0
          results_new$sub_run <- results_o
          results[[i]][[o]] <- results
        }
      }

      ### save results
      if (save_results) {
        private$.results[private$.runs_last] <- results
      }

      ### return results
      if (return_results) {
        simplify_results(results = results, simplify = TRUE)
      } else {
        invisible(self)
      }
    },

    #' @description
    #' Overview of the optimization runs.
    #' @param columns
    #' A \code{character} (vector), the names of columns to include in the
    #' summary output.
    #' The following columns are included by default:
    #' 1. \code{"value"}, the value of the estimated function optimum
    #' 2. \code{"parameter"}, the parameter vector at which the optimum is
    #'    obtained
    #' 3. \code{"seconds"}, the optimization time in seconds
    #' 4. \code{"optimizer"}, the optimizer name
    #' 5. \code{"label"}, the optimization label
    #' See \code{$summary_columns()} for an overview of the available column
    #' names.
    #' TODO
    #' Set \code{columns = "all"} to include all of them in the output.
    #' @param ...
    #' Optionally named expressions of variables from summary columns as
    #' \code{character}.
    #' See \code{$summary_columns()} for an overview of the available columns.
    #' Also, \code{"true_value"} and \code{"true_parameter"} are available
    #' (if specified).
    #' @return
    #' A \code{data.frame}.
    summary = function(
      columns = c("value", "parameter", "seconds", "optimizer", "label"),
      which_run = "all", which_optimizer = "all",
      digits = getOption("ino_digits", default = 2),
      only_comparable = FALSE, ...
    ) {
      summary.Nop(
        object = self, columns = columns, which_run = which_run,
        which_optimizer = which_optimizer, digits = digits,
        only_comparable = only_comparable, ...
      )
    },

    #' @description
    #' Delete optimization results.
    clear = function(which_run, which_optimizer, which_element) {
      run_ids <- self$find_runs(which_run) # TODO
      private$.results[run_ids] <- NULL
    },

    #' @description
    #' Overview of the identified optima.
    #' @param sort_by
    #' Either \code{"frequency"} (default) to sort rows by frequency or
    #' \code{"value"} to sort rows by value.
    optima = function(
      digits = getOption("ino_digits", default = 2), sort_by = "frequency",
      which_run = "all", which_optimizer = "all",
      only_comparable = TRUE
    ) {
      if (!is_number(digits)) {
        ino_stop(
          "Input {.var digits} must be an {.cls integer}."
        )
      }
      if (!(identical(sort_by, "frequency") | identical(sort_by, "value"))) {
        ino_stop(
          "Input {.var sort_by} must be {.val frequency} or {.val value}."
        )
      }
      optima <- as.data.frame(
        table(
          self$summary(
            columns = "value", which_run = which_run,
            which_optimizer = which_optimizer, digits = digits,
            only_comparable = only_comparable
          ),
          useNA = "ifany"
        )
      )
      colnames(optima) <- c("value", "frequency")

      ### sort rows
      optima <- optima[order(
        optima[[sort_by]],
        decreasing = ifelse(sort_by == "value" && private$.show_minimum, FALSE, TRUE)
        ), ]
      rownames(optima) <- NULL

      return(optima)
    },

    #' @description
    #' Visualization of optimization time.
    #' @param by
    #' Either:
    #' - \code{"label"} to group by optimization label
    #' - \code{"optimizer"} to group by optimizer
    #' - \code{NULL} to not group (default)
    #' @param relative
    #' Either \code{TRUE} (default) to plot relative time differences with
    #' respect to the median of the top boxplot, or \code{FALSE} to plot
    #' absolute computation time.
    #' @param log
    #' Either \code{TRUE} to plot a log10-x-axis, or \code{FALSE} (default)
    #' if not.
    plot = function(by = NULL, relative = TRUE, log = FALSE) {
      plot.Nop(x = self, by = by, relative = relative, log = log)
    },

    #' @description
    #' From all saved optimization results, extract the parameter vector that
    #' led to a function value closest to \code{value}.
    #' @param value
    #' A single \code{numeric}.
    closest_parameter = function(
      value, which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {
      if (!is_number(value)) {
        ino_stop("Input {.var value} must be a single {.cls numeric}.")
      }
      results <- self$summary(
        columns = c("value", "parameter", "comparable"), which_run = which_run,
        which_optimizer = which_optimizer, digits = Inf,
        only_comparable = only_comparable
      )
      index <- which.min(abs(value - results$value))
      if (length(index) == 0) {
        ino_stop(
          "No parameter vector found."
        )
      }
      results$parameter[[index]]
    },

    #' @description
    #' TODO
    #' @return
    #' TODO
    results = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      only_comparable = FALSE, simplify = TRUE
    ) {

      ### input checks
      if (!is.character(which_element)) {
        ino_stop(
          "Argument {.var which_element} must be a {.cls character} (vector)."
        )
      }

      ### get results
      run_ids <- private$.get_run_ids(which_run)
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      private$.results |>
        {function(x) x[run_ids]}() |>
        lapply(`[`, optimizer_ids) |>
        filter_comparable(only_comparable) |>
        simplify_results(simplify)

      # TODO: also filter for 'which_element'. document the standard elements.
    },

    #' @description
    #' Returns the best found \code{numeric} parameter vector (if available).
    #' TODO
    best_parameter = function(
      which_run = "all", which_optimizer = "all", only_comparable = TRUE
    ) {
      if (missing(value)) {
        self$which_parameter(self$best_value, only_comparable = TRUE)
      } else {
        ino_stop("{.var $best_parameter} is read only.")
      }
      # TODO: attributes with run and optimizer id
    },

    #' @description
    #' Returns the best found \code{numeric} value of \code{f} (if available).
    #' TODO
    best_value = function(
      which_run = "all", which_optimizer = "all", only_comparable = TRUE
    ) {
      value <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = "value", only_comparable = only_comparable,
        simplify = TRUE
      )

      if (private$.show_minimum) {
        # min(self$summary("value", only_comparable = TRUE)$value, na.rm = TRUE)
      } else {
        # max(self$summary("value", only_comparable = TRUE)$value, na.rm = TRUE)
      }
      # TODO: attributes with run and optimizer id
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
    .optimizer = list(),
    .results = list(),
    .runs_last = integer(),
    .optimization_labels = character(),
    .show_minimum = TRUE,

    ### checks supplied value for target argument
    .check_target_argument = function(target_arg, arg_name) {
      stopifnot(is_name(arg_name))
      if (!is.vector(target_arg) || !is.numeric(target_arg)) {
        ino_stop(
          glue::glue("Input `{arg_name}` is misspecified."),
          "It must be a {.cls numeric}."
        )
      }
      if (length(target_arg) != self$npar) {
        ino_stop(
          glue::glue("Input `{arg_name}` is misspecified."),
          glue::glue("It must be of length {self$npar}.")
        )
      }
    },

    ### checks if argument for target function exists
    .check_additional_argument_exists = function(argument_name) {
      stopifnot(is_name(argument_name))
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue(
            "Argument `{argument_name}` is not yet specified",
            "for function `{private$.f_name}`.",
            .sep = " "
          ),
          glue::glue("Use `$set_argument(\"{argument_name}\" = ...)`.")
        )
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
          if (grepl("reached elapsed time limit|reached CPU time limit", msg)) {
            return("time limit reached")
          } else {
            msg
          }
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
          return(
            list(
              "value" = NA_real_, "parameter" = NA_real_, "seconds" = NA_real_,
              "initial" = initial, "error" = if (
                grepl("reached elapsed time limit|reached CPU time limit", msg)
              ) {
                "time limit reached"
              } else {
                msg
              }
            )
          )
        }
      )
    },

    ### save optimization results inside `Nop` object
    .save_results = function(results, run_ids, optimizer_ids, label) {
      for (i in seq_along(results)) {
        for (j in seq_along(optimizer_ids)) {
          results[[i]][[j]][["optimizer"]] <- names(private$.optimizer)[optimizer_ids[j]]
          results[[i]][[j]][["label"]] <- label
          results[[i]][[j]][["comparable"]] <- length(private$.original_arguments) == 0
        }
      }
      private$.results[run_ids] <- results
      private$.optimization_labels <- unique(c(private$.optimization_labels, label))
    },

    ### save original arguments before standardization / reducing
    .save_original_argument = function(original_argument, argument_name) {
      if (is.null(private$.original_arguments[[argument_name]])) {
        private$.original_arguments[[argument_name]] <- original_argument
      }
    },

    ### reset transformed argument to original argument
    .reset_original_argument = function(argument_name, verbose = FALSE) {
      if (!is.null(private$.original_arguments[[argument_name]])) {
        private$.arguments[[argument_name]] <- private$.original_arguments[[argument_name]]
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
          ids <- seq_along(private$.runs_last)
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
    .get_optimizer_ids = function(which_optimizer) {
      optimizer <- private$.optimizer
      ids <- seq_along(optimizer)
      if (length(ids) == 0) {
        ino_warn(
          "No optimizer specified yet.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
        )
        return(integer(0))
      } else if (identical(which_optimizer, "all")) {
        return(ids)
      }
      optimizer_active <- which(check_optimizer_active(optimizer, ids))
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
        if (!is_name(value)) {
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
        if (is.null(out)) {
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

    #' @field true_parameter The true optimum \code{numeric} parameter vector
    #' of length \code{npar} (if available), i.e., the point where \code{f}
    #' obtains its optimum.
    true_parameter = function(value) {
      if (missing(value)) {
        out <- private$.true_parameter
        if (is.null(out)) {
          ino_warn(
            "The true optimum parameter vector has not been specified yet."
          )
        }
        return(out)
      } else {
        if (is.null(value)) {
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_parameter}."
          )
        } else {
          private$.check_target_argument(value, "true_parameter")
          private$.true_value <- self$evaluate(at = value)
          digits <- getOption('ino_digits', default = 2)
          ino_status(
            glue::glue(
              "Set true optimum value to",
              "{round(private$.true_value, digits = digits)}.",
              .sep = " "
            )
          )
          private$.true_parameter <- value
        }
      }
    },

    #' @field true_value The true \code{numeric} optimum value of \code{f}
    #' (if available).
    true_value = function(value) {
      if (missing(value)) {
        out <- private$.true_value
        if (is.null(out)) {
          ino_warn(
            "The true optimum function value has not been specified yet."
          )
        }
        return(out)
      } else {
        if (is.null(true_value)) {
          private$.true_value <- NULL
          private$.true_parameter <- NULL
          ino_status(
            "Removed {.var true_value} and {.var true_parameter}."
          )
        } else {
          if (!(is.vector(value) && is.numeric(value) && length(value) == 1)) {
            ino_stop(
              "{.var true_value} must be a single {.cls numeric}."
            )
          }
          if (!is.null(private$.true_parameter)) {
            true_value_old <- self$evaluate(at = private$.true_parameter)
            if (true_value != true_value_old) {
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

    #' @field show_minimum A \code{logical}, set to \code{TRUE} (default) to
    #' show best minimum in \code{$best_value()} and \code{$best_parameter()}.
    show_minimum = function(value) {
      if (missing(value)) {
        private$.show_minimum
      } else {
        if (!isTRUE(value) && !isFALSE(value)) {
          ino_stop("{.var show_minimum} must be {.val TRUE} or {.val FALSE}.")
        }
        private$.show_minimum <- value
      }
    },

    #' @field optimizer A \code{list} of specified optimizers.
    optimizer = function(value) {
      if (missing(value)) {
        out <- private$.optimizer
        if (is.null(out)) {
          ino_warn(
            "No optimizer specified yet.",
            "Please use {.fun $set_optimizer} to specify an optimizer."
          )
        }
        # TODO check if there are active optimizers
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
            return(label)
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

