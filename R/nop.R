#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a *N*umerical *O*ptimization *P*roblem.
#'
#' @param which_optimizer
#' Select specified numerical optimizer. Either:
#' - \code{"all"} for all specified optimizer (default)
#' - a \code{character} vector of specified optimizer labels
#' - a \code{numeric} vector of optimizer IDs (see the output of \code{$print()})
#' @param verbose
#' A \code{logical}, which indicates whether progress/details should be printed.
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) such messages.
#' The default is \code{getOption("ino_verbose")}, which is set to \code{TRUE}
#' when the package is loaded.
#' @param ncores
#' An \code{integer}, the number of cores for parallel computation.
#' The default is \code{getOption("ino_ncores")}, which is set to \code{1}
#' when the package is loaded.
#' @param digits
#' TODO: number of significant decimal places, by default \code{getOption("ino_digits")}
#' @param seed
#' Set a seed for reproducibility. No seed by default.
#'
#' @details
#' # Getting Started
#'
#' ## Step 1: Create a new \code{Nop} object.
#' TODO
#'
#' ## Step 2: Add additional arguments.
#' TODO
#'
#' ## Step 2: Specify numerical optimizer.
#' TODO
#'
#' # Optimize
#' TODO
#'
#' # Evaluate
#' TODO
#'
#' @examples
#' Nop$new(f = f_ackley, npar = 2)$
#'   set_optimizer(optimizer_nlm())$
#'   optimize(runs = 10)$
#'   summary()
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
    #' Optionally additional arguments for \code{f}.
    #' @return
    #' A new \code{Nop} object.
    initialize = function (f, npar, ...) {
      if (missing(f)) {
        ino_stop(
          "Please specify argument `f`.",
          "It is the function to be optimized."
        )
      }
      if (!is.function(f)) {
        ino_stop(
          "Argument `f` is not a function.",
          "Please specify a function as argument `f`."
        )
      }
      if (is.null(formals(f))) {
        ino_stop(
          "The function `f` should have at least one argument.",
          "Mind that `f` is optimized over its first argument which should be a numeric vector of length `npar`."
        )
      }
      if (missing(npar)) {
        ino_stop(
          "Please specify argument `npar`.",
          "It is the length of the first argument of `f`."
        )
      }
      if (!(is.numeric(npar) && length(npar) == 1 && npar > 0 && npar %% 1 == 0)) {
        ino_stop(
          "Argument `npar` is not a positive integer.",
          "Please specify `npar` as the length of the first argument of `f`."
        )
      }
      private$.f <- f
      f_name <- deparse(substitute(f))
      if (!is.character(f_name) || length(f_name) != 1) {
        f_name <- "unnamed function"
      }
      private$.f_name <- f_name
      private$.f_target <- names(formals(f))[1]
      private$.npar <- as.integer(npar)
      if(length(list(...)) > 0) self$set_argument(...)
    },

    #' @description
    #' Prints details of numerical optimization problem.
    #' @importFrom crayon underline
    #' @importFrom glue glue
    #' @param ...
    #' Currently not used.
    print = function (...) {
      cat(
        crayon::underline("Optimization problem:"), "\n",
        glue::glue(" Function: {private$.f_name}"), "\n",
        glue::glue(" Optimize over: {private$.f_target} (length {private$.npar})"), "\n",
        sep = ""
      )
      if (private$.narguments > 0) {
        cat(" Additional arguments:", paste(names(private$.arguments), collapse = ", "), "\n")
      }
      if (!is.null(private$.true_parameter)) {
        cat(
          glue::glue(" True optimum at: {paste(private$.true_parameter, collapse = ' ')}"),
          "\n", sep = ""
        )
      }
      if (!is.null(private$.true_value)) {
        cat(
          glue::glue(" True optimum value: {paste(private$.true_value, collapse = ' ')}"),
          "\n", sep = ""
        )
      }
      cat(
        crayon::underline("Numerical optimizer:"), "\n", sep = ""
      )
      if (length(private$.optimizer) == 0) {
        cat(
          " No optimizer specified.\n"
        )
      } else {
        for (i in seq_along(private$.optimizer)) {
          cat(
            glue::glue(" {i}: {private$.optimizer_label[i]}"), "\n", sep = ""
          )
        }
      }
      cat(
        crayon::underline("Optimization results:"), "\n", sep = ""
      )
      if (length(private$.records) == 0) {
        cat(
          " No optimization records.\n"
        )
      } else {
        cat(
          glue::glue(" Optimization records: {length(private$.records)}"), "\n",
          glue::glue(" Found optimum at: {paste(self$best_parameter, collapse = ' ')}"), "\n",
          glue::glue(" Found optimum value: {paste(self$best_value, collapse = ' ')}"), "\n",
          sep = ""
        )
      }
      invisible(self)
    },

    #' @description
    #' Set additional arguments for \code{f}.
    #' @param ...
    #' Optionally additional arguments for \code{f}.
    set_argument = function (...) {
      arguments <- list(...)
      if (length(arguments) == 0) {
        ino_stop(
          "Please specify an argument for `f`."
        )
      }
      argument_names <- names(arguments)
      argument_names[which(is.null(argument_names))] <- ""
      for (i in seq_along(arguments)) {
        if (nchar(argument_names[i]) < 1) {
          ino_stop(
            glue::glue("Please name argument {i}.")
          )
        }
        if (argument_names[i] %in% names(private$.arguments)) {
          ino_stop(
            glue::glue("Argument `{argument_names[i]}` already exists."),
            glue::glue("Please call `$remove_argument('{argument_names[i]}')` first.")
          )
        }
      }
      for (i in seq_along(arguments)) {
        private$.arguments[[argument_names[i]]] <- arguments[[i]]
        private$.narguments <- private$.narguments + 1
      }
      invisible(self)
    },

    #' @description
    #' Get value of additional argument for \code{f}.
    #' @param argument_name
    #' A \code{character}, the argument to extract.
    get_argument = function (argument_name) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify `argument_name`."
        )
      }
      if (!is.character(argument_name) || length(argument_name) != 1) {
        ino_stop(
          "Input `argument_name` must be a single character."
        )
      }
      private$.check_add_arg_exists(argument_name)
      private$.arguments[[argument_name]]
    },

    #' @description
    #' Remove additional arguments for \code{f}.
    #' @param argument_name
    #' A \code{character} (vector), the argument(s) to remove.
    remove_argument = function (argument_name) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify `argument_name`."
        )
      }
      if (!is.character(argument_name)) {
        ino_stop(
          "Input `argument_name` must be a `character` (vector)."
        )
      }
      for (i in seq_along(argument_name)) {
        private$.check_add_arg_exists(argument_name[i])
        arg_id <- which(names(private$.arguments) == argument_name[i])
        private$.arguments[arg_id] <- NULL
        private$.narguments <- private$.narguments - 1
      }
      invisible(self)
    },

    #' @description
    #' Reset additional arguments for \code{f} after transformation with
    #' \code{$standardize()} or \code{$reduce()}.
    #' @param argument_name
    #' A \code{character} (vector), the argument(s) to reset.
    reset_argument = function (argument_name, verbose = getOption("ino_verbose")) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify `argument_name`."
        )
      }
      if (!is.character(argument_name)) {
        ino_stop(
          "Input `argument_name` must be a `character` (vector)."
        )
      }
      for (arg in argument_name) {
        private$.check_add_arg_exists(argument_name)
        private$.reset_orig_argument(arg, verbose = verbose)
      }
    },

    #' @description
    #' Evaluate the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}.
    #' @return
    #' A \code{numeric} value.
    evaluate = function (at) {
      private$.check_add_args_complete()
      private$.check_target_arg(at, arg_name = "at")
      private$.evaluate(at)
    },

    #' @description
    #' Optionally set the true optimum value.
    #' @param true_value
    #' A single \code{numeric}, the value of \code{f} at its optimum.
    set_true_value = function (true_value) {
      if (!(is.numeric(true_value) && length(true_value) == 1)) {
        ino_stop(
          "Argument `true_value` must be a single numeric."
        )
      }
      private$.true_value <- true_value
      invisible(self)
    },

    #' @description
    #' Optionally set the true optimum parameter vector.
    #' @param true_parameter
    #' A \code{numeric} vector of length \code{npar}, the point where \code{f}
    #' obtains its optimum.
    #' @param set_true_value
    #' Set to \code{TRUE} to use \code{true_parameter} to compute the true
    #' optimum value of \code{f}.
    #' Set to \code{FALSE} (default) if not.
    set_true_parameter = function (true_parameter, set_true_value = FALSE) {
      private$.check_target_arg(true_parameter, "true_parameter")
      if (!(isTRUE(set_true_value) || isFALSE(set_true_value))) {
        ino_stop(
          "Argument `set_true_value` must be `TRUE` or `FALSE`."
        )
      }
      if (!is.null(private$.true_value) && isFALSE(set_true_value)) {
        ino_stop(
          "Please set `set_true_value = TRUE` to also update the true optimum value."
        )
      }
      private$.true_parameter <- true_parameter
      if (set_true_value) {
        private$.true_value <- self$evaluate(at = true_parameter)
      }
      invisible(self)
    },

    #' @description
    #' Set a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer}, which can be created with the
    #' \code{\link[optimizeR]{set_optimizer}} function from the \{optimizeR\}
    #' package.
    #' @param label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
    set_optimizer = function (optimizer, label = NULL) {
      if (missing(optimizer)) {
        ino_stop(
          "Please specify argument `optimizer`."
        )
      }
      if (!inherits(optimizer, "optimizer")) {
        ino_stop(
          "Argument `optimizer` must be an object of class `optimizer`.",
          "Please use `optimizeR::set_optimizer()` to create such an object."
        )
      }
      if (is.null(label)) {
        label <- optimizer$opt_name
      }
      if (!(is.character(label) && length(label) == 1)) {
        ino_stop(
          "Argument `label` must be a single character."
        )
      }
      if (label %in% private$.optimizer_label) {
        ino_stop(
          glue::glue("Label `{label}` already exists, please choose another one."),
          "Note that label for optimizer must be unique for identification."
        )
      }
      private$.optimizer[[length(private$.optimizer) + 1]] <- optimizer
      private$.optimizer_label <- c(private$.optimizer_label, label)
      invisible(self)
    },

    #' @description
    #' Remove numerical optimizer.
    remove_optimizer = function (which_optimizer = "all") {
      opt_ids <- private$.get_optimizer_ids(which_optimizer)
      private$.optimizer <- private$.optimizer[-opt_ids]
      private$.optimizer_label <- private$.optimizer_label[-opt_ids]
      invisible(self)
    },

    #' @description
    #' Optimize the function.
    #' @param initial
    #' Specify the initial point where the optimizer should start. Either:
    #' - the \code{character} \code{"random"} (the default) for random initial
    #'   values drawn from a standard normal distribution
    #' - a \code{numeric} vector of length \code{npar}, the starting point for
    #'   optimization
    #' - a \code{function} without any arguments that returns a \code{numeric}
    #'   vector of length \code{npar}
    #' @param runs
    #' An \code{integer}, the number of optimization runs.
    #' By default, \code{runs = 1}.
    #' @param save_results
    #' A \code{logical}, which indicates whether the results should be saved
    #' inside the \code{Nop} object.
    #' By default, \code{save_results = TRUE}.
    #' @param return_results
    #' A \code{logical}, which indicates whether the results should be returned.
    #' By default, \code{return_results = FALSE}.
    #' @param label
    #' Only relevant if \code{save_results = TRUE}.
    #' In this case, optionally a \code{character} for a custom label of the
    #' optimization.
    #' @param simplify
    #' Only relevant if \code{return_results = TRUE} and \code{runs = 1} and/or
    #' only one optimizer is specified.
    #' In this case, if \code{simplify = TRUE} (default), the nested list output
    #' is flattened.
    #' @param reset_arguments_afterwards
    #' TODO
    #' @return
    #' The return value depends on the value of \code{return_results}:
    #' - if \code{return_results = FALSE} (default), invisibly the \code{Nop} object
    #' - if \code{return_results = TRUE}, the output of \code{\link[optimizeR]{apply_optimizer}}.
    #' @importFrom parallel makeCluster stopCluster
    #' @importFrom doSNOW registerDoSNOW
    #' @importFrom foreach foreach %dopar% %do%
    optimize = function(
      initial = "random", runs = 1, which_optimizer = "all", seed = NULL,
      save_results = TRUE, return_results = FALSE, label = "",
      ncores = getOption("ino_ncores"), verbose = getOption("ino_verbose"),
      simplify = TRUE, reset_arguments_afterwards = TRUE
    ) {

      ### check `initial` and make it to function call `get_initial`
      get_initial <- if (identical(initial, "random")) {
        function() rnorm(private$.npar)
      } else if (is.numeric(initial) && length(initial) == private$.npar) {
        function() initial
      } else if (is.function(initial)) {
        try_initial <- try(initial(), silent = TRUE)
        if (!(is.numeric(try_initial) && length(try_initial) == private$.npar)) {
          ino_stop(
            glue::glue("The function `initial` should return a numeric vector of length {private$.npar}.")
          )
        }
        initial
      } else {
        ino_stop(
          "Input `initial` is misspecified."
        )
      }

      ### other input checks
      if (!(is.numeric(runs) && length(runs) == 1 && runs > 0 && runs %% 1 == 0)) {
        ino_stop(
          "Input `runs` must be a positive integer."
        )
      }
      if (!isTRUE(save_results) && !isFALSE(save_results)) {
        ino_stop(
          "Input `save_results` must be either `TRUE` or `FALSE`."
        )
      }
      if (!isTRUE(return_results) && !isFALSE(return_results)) {
        ino_stop(
          "Input `return_results` must be either `TRUE` or `FALSE`."
        )
      }
      if (isTRUE(return_results)) {
        if (!isTRUE(simplify) && !isFALSE(simplify)) {
          ino_stop(
            "Input `simplify` must be either `TRUE` or `FALSE`."
          )
        }
      }
      if (!(is.numeric(ncores) && length(ncores) == 1 && ncores > 0 && ncores %% 1 == 0)) {
        ino_stop(
          "Input `ncores` must be a positive integer."
        )
      }
      if (!isTRUE(verbose) && !isFALSE(verbose)) {
        ino_stop(
          "Input `verbose` must be either `TRUE` or `FALSE`."
        )
      }

      ### optimization
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (!is.null(seed)) set.seed(seed)
      parallel <- ncores > 1 && runs >= 2 * ncores
      if (parallel) {
        cluster <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cluster))
        doSNOW::registerDoSNOW(cluster)
        `%par_seq%` <- foreach::`%dopar%`
      } else {
        `%par_seq%` <- foreach::`%do%`
      }
      # TODO: progress bar
      results <- foreach::foreach(
        run = 1:runs, .packages = "ino", .export = "private", .inorder = FALSE
      ) %par_seq% {
        initial <- get_initial()
        lapply(optimizer_ids, function (i) private$.optimize(initial, i))
      }

      ### save results
      if (save_results) {
        private$.save_results(results, optimizer_ids, label)
      }

      ### reset transformed arguments
      if (reset_arguments_afterwards) {
        lapply(names(private$.orig_arguments), private$.reset_orig_argument, verbose = verbose)
      }

      ### return results
      if (return_results) {
        if (simplify) {
          if (runs == 1) {
            results <- unlist(results, recursive = FALSE)
          }
          if (length(optimizer_ids) == 1) {
            results <- unlist(results, recursive = FALSE)
          }
        }
        return(results)
      } else {
        return(invisible(self))
      }
    },

    #' @description
    #' Test the configuration of a \code{Nop} object.
    #' @param at
    #' A \code{numeric} of length \code{npar}, the point at which the
    #' function \code{f} and the specified optimizer are tested.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values.
    #' @param time_limit_fun
    #' An \code{integer}, the time limit in seconds for testing the
    #' function call.
    #' If no error occurred after \code{time_limit_fun} seconds, the test is
    #' considered to be successful.
    #' By default, \code{time_limit_fun = 10}.
    #' @param time_limit_opt
    #' An \code{integer}, the time limit in seconds for testing the
    #' optimization call.
    #' If no error occurred after \code{time_limit_opt} seconds, the test is
    #' considered to be successful.
    #' By default, \code{time_limit_opt = 10}.
    #' @return
    #' Returns invisibly \code{TRUE} if the tests are successful.
    test = function (
      at = rnorm(self$npar), which_optimizer = "all", time_limit_fun = 10,
      time_limit_opt = time_limit_fun, verbose = getOption("ino_verbose")
      ) {

      ### input checks
      private$.check_target_arg(at, "at")
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer, no_optimizer = "ignored")
      optimizer_selected <- length(optimizer_ids) > 0
      if (!(is.numeric(time_limit_fun) && length(time_limit_fun) == 1 && time_limit_fun > 0 && time_limit_fun %% 1 == 0)) {
        ino_stop(
          "Argument `time_limit_fun` is not a positive integer.",
          "Please specify `time_limit_fun` as the number of seconds for testing the function call."
        )
      }
      if (!(is.numeric(time_limit_opt) && length(time_limit_opt) == 1 && time_limit_opt > 0 && time_limit_opt %% 1 == 0)) {
        ino_stop(
          "Argument `time_limit_opt` is not a positive integer.",
          "Please specify `time_limit_opt` as the number of seconds for testing the function call."
        )
      }
      if (!isTRUE(verbose) && !isFALSE(verbose)) {
        ino_stop(
          "Input `verbose` must be either `TRUE` or `FALSE`."
        )
      }

      ### test configurations
      ino_status("Test configurations:", verbose = verbose)
      ino_success(glue::glue("Function specified: {private$.f_name}") , verbose = verbose)
      ino_success(glue::glue("Target argument specified: {private$.f_target} (length {private$.npar})") , verbose = verbose)
      if (optimizer_selected) {
        ino_success(glue::glue("Optimizer specified: {paste(private$.optimizer_label[optimizer_ids], collapse = ', ')}"), verbose = verbose)
      }

      ### test function call
      ino_status("Test function call:", verbose = verbose)
      setTimeLimit(cpu = time_limit_fun, elapsed = time_limit_fun, transient = TRUE)
      on.exit({
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      })
      out <- tryCatch(
        {
          self$evaluate(at)
        },
        error = function(e) {
          if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
            return(".ino_time_limit_fun_reached")
          } else {
            ino_stop("Function call failed.")
          }
        }
      )
      ino_success("Calling the function did not throw an error.", verbose = verbose)
      if (identical(out, ".ino_time_limit_fun_reached")) {
        ino_warn(
          glue::glue("The time limit of {time_limit_fun}s was reached in the test function call."),
          "To make sure that the test function call returns a single numeric value, consider increasing `time_limit_fun`."
        )
      } else {
        if (!is.numeric(out)) {
          ino_stop(
            glue::glue("Test function call returned an object of class `{class(out)[1]}`."),
            "It should return a single `numeric` value."
          )
        } else {
          ino_success("Test function call returned a `numeric`.", verbose = verbose)
        }
        if (length(out) != 1) {
          ino_stop(
            glue::glue("Test function call returned a `numeric` of length {length(out)}."),
            "It should return a single `numeric` value."
          )
        } else {
          ino_success(glue::glue("Return value is {out}."), verbose = verbose)
        }
      }

      ### test optimization
      if (!optimizer_selected) {
        ino_warn("No optimizer specified, testing optimizer is skipped.")
      } else {
        ino_status("Test optimization:", verbose = verbose)
        for (i in seq_along(optimizer_ids)) {
          setTimeLimit(cpu = time_limit_opt, elapsed = time_limit_opt, transient = TRUE)
          out <- tryCatch(
            {
              private$.optimize(initial = at, optimizer_id = i)
            },
            error = function(e) {
              if (grepl("reached elapsed time limit|reached CPU time limit", e$message)) {
                return(".ino_time_limit_opt_reached")
              } else {
                ino_stop(
                  glue::glue("Optimization with optimizer `{private$.optimizer_label[i]}` failed.")
                )
              }
            }
          )
          ino_success(
            glue::glue("Calling optimizer `{private$.optimizer_label[i]}` did not throw an error."),
            verbose = verbose
          )
          if (identical(out, ".ino_time_limit_opt_reached")) {
            ino_warn(
              glue::glue("The time limit of {time_limit_opt}s was reached in the test optimization call with optimizer `{private$.optimizer_label[i]}`."),
              "To make sure that the optimization ends successful, consider increasing `time_limit_opt`."
            )
          }
        }
      }

      invisible(TRUE)
    },

    #' @description
    #' Standardize the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be standardized.
    #' The argument must be either a \code{data.frame} or a \code{matrix}.
    #' @param by_column
    #' Either \code{TRUE} to standardize column-wise (default) or \code{FALSE}
    #' to standardize row-wise.
    #' @param center
    #' Passed to \code{\link[base]{scale}}. Default is \code{TRUE}.
    #' @param scale
    #' Passed to \code{\link[base]{scale}}. Default is \code{TRUE}.
    #' @param ignore
    #' A \code{integer} (vector) of column indices (or row indices if
    #' \code{by_column = FALSE}) to not standardize.
    standardize = function(
      argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
      ignore = integer(), verbose = getOption("ino_verbose")
    ) {

      ### input checks
      if (missing(argument_name)) {
        ino_stop(
          "Please specify `argument_name`.",
          "It is the name of the argument of `f` to be standardized."
        )
      }
      if (!is.character(argument_name) && length(argument_name) == 1) {
        ino_stop(
          "Input `argument_name` must be a `character`."
        )
      }
      private$.check_add_arg_matrix_df(argument_name, verbose = verbose)
      if (!isTRUE(by_column) && !isFALSE(by_column)) {
        ino_stop(
          "Argument `by_column` must be `TRUE` or `FALSE`."
        )
      }
      if (!is.numeric(ignore) || !all(ignore > 0 & ignore %% 1 == 0)) {
        ino_stop(
          "Argument 'ignore' must be a vector of indices."
        )
      }

      ### standardizing
      argument <- self$get_argument(argument_name)
      orig_argument <- argument
      attr_argument <- attributes(argument)
      if (!by_column) argument <- t(argument)
      if (length(ignore) > 0) {
        argument[, -ignore] <- scale(argument[, -ignore], center = center, scale = scale)
      } else {
        argument <- scale(argument, center = center, scale = scale)
      }
      if (!by_column) argument <- t(argument)
      if (is.data.frame(orig_argument)) argument <- as.data.frame(argument)
      attributes(argument) <- attr_argument
      ino_status(
        glue::glue("Standardized {class(argument)[1]} `{argument_name}`."),
        verbose = verbose
      )

      ### save standardized argument
      private$.arguments[[argument_name]] <- argument

      ### temporally save original argument
      private$.save_orig_argument(orig_argument, argument_name)

      invisible(self)
    },

    #' @description
    #' Reduce the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be subsetted.
    #' The argument must be either a \code{data.frame} or a \code{matrix}.
    #' @param by_row
    #' Either \code{TRUE} to subset row-wise (default) or \code{FALSE}
    #' to subset column-wise.
    #' @param how
    #' A \code{character}, specifying how to select the subset. Can be one of:
    #' - \code{"random"} (default)
    #' - \code{"first"}
    #' - \code{"last"}
    #' - \code{"similar"}
    #' - \code{"unsimilar"}
    #' @param proportion
    #' A \code{numeric} between \code{0} and \code{1}, specifying the proportion
    #' of the subset.
    #' By default, \code{proportion = 0.5}.
    #' @param centers
    #' Only relevant if \code{how = "(un)similar"}.
    #' In that case, passed to \code{\link[stats]{kmeans}}.
    #' By default, \code{centers = 2}.
    #' @param ignore
    #' Only relevant if \code{how = "(un)similar"}.
    #' In that case a \code{integer} (vector) of row indices (or column indices
    #' if \code{by_row = FALSE}) to ignore for clustering.
    reduce = function(
      argument_name, by_row = TRUE, how = "random", proportion = 0.5, centers = 2,
      ignore = integer(), seed = NULL, verbose = getOption("ino_verbose")
    ) {

      ### input checks
      if (missing(argument_name)) {
        ino_stop(
          "Please specify argument `argument_name`.",
          "It is the name of the argument of `f` to be standardized."
        )
      }
      private$.check_add_arg_matrix_df(argument_name, verbose = verbose)
      if (!isTRUE(by_row) && !isFALSE(by_row)) {
        ino_stop(
          "Argument 'by_row' must be `TRUE` or `FALSE`."
        )
      }
      if (!(is.character(how) && length(how) == 1 &&
          how %in% c("random", "first", "last", "similar", "unsimilar"))) {
        ino_stop(
          "Argument 'how' must be one of `random`, `first`, `last`, `similar` or `unsimilar`."
        )
      }
      if (how %in% c("similar", "unsimilar")) {
        if (!is.numeric(ignore) || !all(ignore > 0 & ignore %% 1 == 0)) {
          ino_stop(
            "Argument 'ignore' must be a vector of indices."
          )
        }
      }
      if (!(is.numeric(proportion) && length(proportion) == 1 && proportion > 0 && proportion < 1)) {
        ino_stop(
          "Argument 'proportion' must be a numeric between 0 and 1."
        )
      }
      if (!is.null(seed)) set.seed(seed)

      ### subsetting
      argument <- self$get_argument(argument_name)
      orig_argument <- argument
      if (!by_row) argument <- t(argument)
      n <- nrow(argument)
      m <- ceiling(n * proportion)
      if (how == "random") {
        ind <- sort(sample.int(n, m))
      } else if (how == "first") {
        ind <- seq_len(m)
      } else if (how == "last") {
        ind <- tail(seq_len(n), m)
      } else {
        stopifnot(how == "similar" || how == "unsimilar")
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
        } else if (how == "unsimilar") {
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
      argument <- argument[ind, ]
      ino_status(
        glue::glue("Reduced {class(argument)[1]} `{argument_name}` from {nrow(orig_argument)} to {how} {nrow(argument)} {ifelse(by_row, 'row(s)', 'column(s)')}."),
        verbose = verbose
      )
      if (!by_row) argument <- t(argument)

      ### save reduced argument
      private$.arguments[[argument_name]] <- argument

      ### temporally save original argument
      private$.save_orig_argument(orig_argument, argument_name)

      invisible(self)
    },

    #' @description
    #' TODO
    continue = function () {
      # continue runs from last `optimize()` call
    },

    #' @description
    #' TODO
    #' @param columns
    #' TODO
    #' @param ...
    #' Expressions of variables from summary columns as characters.
    #' TODO \code{$summary_columns()}
    #' @return
    #' TODO
    #' @importFrom dplyr bind_rows
    summary = function (
      columns = c("value", "parameter", "seconds", "optimizer"), ...
    ) {

      ### check if records exist
      records <- private$.records
      if (length(records) == 0) {
        ino_stop(
          "No records exist."
        )
      }

      ### combine records in data.frame
      out <- data.frame()
      for (run in 1:length(records)) {
        for (opt in 1:length(records[[run]])) {
          out_tmp <- as.data.frame(t(cbind(records[[run]][[opt]])))
          out <- dplyr::bind_rows(out, out_tmp)
        }
      }

      ### add elements
      # TODO
      # add_vars <- list(...)
      # for (i in seq_along(add_vars)) {
      #   out[[names(add_vars)[i]]] <- sapply(object$runs, function(r) {
      #     env <- new.env()
      #     env$.global <- object$prob$global
      #     list2env(r, env)
      #     tryCatch(
      #       expr = {
      #         out <- eval(parse(text = add_vars[[i]]), env)
      #         stopifnot(length(out) == 1)
      #         out
      #       },
      #       error = function(cond) NA
      #     )
      #   })
      # }

      ### unlist single-valued records
      for (i in 1:ncol(out)) {
        if (all(sapply(out[,i], length) == 1 & sapply(out[,i], class) %in% c("character", "numeric"))) {
          out[,i] <- unlist(out[,i])
        }
      }

      ### filter columns
      out <- dplyr::select(out, dplyr::any_of(columns))

      return(out)
    },

    #' @description
    #' Overview of the identified optima.
    optima = function (digits = 2) {
      optima <- round(self$summary("value"), digits = digits)
      optima <- as.data.frame(table(optima))
      colnames(optima) <- c("value", "frequency")
      optima[order(optima$frequency, decreasing = TRUE), ]
    },

    #' @description
    #' Visualization of optimization time.
    #' @param by
    #' A character vector of variables to group by.
    #' Can be \code{NULL} (default).
    #' @param nrow
    #' Passed to \code{\link[ggplot2]{facet_wrap}}.
    #' @importFrom ggplot2 ggplot aes scale_y_continuous geom_boxplot facet_wrap
    #' theme element_blank ylab
    plot = function (by = NULL, nrow = NULL) {
      self$summary() %>%
        ggplot2::ggplot(aes(x = "", y = .data$seconds)) +
        ggplot2::scale_y_continuous() +
        ggplot2::geom_boxplot() +
        {
          if (!is.null(by)) {
            ggplot2::facet_wrap(by, labeller = "label_both", nrow = nrow)
          }
        } +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        ) +
        ggplot2::ylab("optimization time in seconds")
    }

  ),
  private = list(

    ### optimization problem
    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .arguments = list(),
    .narguments = 0,
    .orig_arguments = list(),
    .true_parameter = NULL,
    .true_value = NULL,

    ### optimizer
    .optimizer = list(),
    .optimizer_label = character(0),

    ### records
    .records = list(),
    .show_minimum = TRUE,

    ### checks supplied value for target argument
    .check_target_arg = function (target_arg, arg_name) {
      stopifnot(is.character(arg_name), length(arg_name) == 1)
      if (!(is.numeric(target_arg) && length(target_arg) == private$.npar)) {
        ino_stop(
          glue::glue("Argument `{arg_name}` must be a numeric vector of length {private$.npar}.")
        )
      }
    },

    ### checks if specific argument exists
    .check_add_arg_exists = function (argument_name) {
      stopifnot(is.character(argument_name), length(argument_name) == 1)
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue("Argument `{argument_name}` does not exist for function `{private$.f_name}`."),
          glue::glue("Use `$set_argument(\"{argument_name}\" = ...)`.")
        )
      }
    },

    ### checks if argument is matrix or data.frame
    .check_add_arg_matrix_df = function (argument_name, verbose = TRUE) {
      private$.check_add_arg_exists(argument_name)
      argument <- private$.arguments[[argument_name]]
      if (!is.data.frame(argument) && !is.matrix(argument)) {
        ino_stop(
          glue::glue("Argument `{argument_name}` must be a `data.frame` or a `matrix`."),
          verbose = verbose
        )
      }
    },

    ### checks if all required arguments for function are specified
    .check_add_args_complete = function () {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        ### checks if `arg` has default value in `f`
        if (!all(nzchar(args_all[[arg]])) && is.name(args_all[[arg]])) {
          private$.check_add_arg_exists(arg)
        }
      }
    },

    ### returns numeric IDs of specified optimizer
    .get_optimizer_ids = function (which_optimizer, no_optimizer = "fatal") {
      stopifnot(no_optimizer %in% c("fatal", "ignored"))
      if (length(private$.optimizer) == 0) {
        if (no_optimizer == "fatal") {
          ino_stop(
            "No optimizer specified.",
            "Please use `$set_optimizer()` to specify an optimizer."
          )
        }
        if (no_optimizer == "ignored") {
          return(integer(0))
        }
      }
      if (identical(which_optimizer, "all")) {
        return(seq_along(private$.optimizer))
      } else if (is.character(which_optimizer)) {
        ids <- which(private$.optimizer_label %in% which_optimizer)
      } else if (is.numeric(which_optimizer)) {
        ids <- which(seq_along(private$.optimizer) %in% which_optimizer)
      } else {
        ino_stop(
          "Argument `which_optimizer` is misspecified."
        )
      }
      if (length(ids) == 0) {
        if (no_optimizer == "fatal") {
          ino_stop(
            "Please check argument `which_optimizer`, it fits to no specified optimizer."
          )
        }
        if (no_optimizer == "ignored") {
          return(integer(0))
        }
      }
      return(ids)
    },

    ### cheap function evaluation
    .evaluate = function(at) {
      at <- list(at)
      names(at) <- private$.f_target
      do.call(
        what = private$.f,
        args = c(at, private$.arguments)
      )
    },

    ### cheap function optimization
    .optimize = function(initial, optimizer_id) {
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
      )
    },

    ### save optimization results inside `Nop` object
    .save_results = function (results, optimizer_ids, label) {
      for (i in seq_along(results)) {
        for (j in seq_along(optimizer_ids)) {
          results[[i]][[j]][[".ino_optimizer"]] <- private$.optimizer_label[optimizer_ids[j]]
          results[[i]][[j]][[".ino_label"]] <- label
        }
      }
      private$.records <- append(private$.records, results)
      # TODO: add total time
    },

    ### save original arguments before standardization / reducing
    .save_orig_argument = function (orig_argument, argument_name) {
      if (is.null(private$.orig_arguments[[argument_name]])) {
        private$.orig_arguments[[argument_name]] <- orig_argument
      }
    },

    ### reset transformed argument to original argument
    .reset_orig_argument = function (argument_name, verbose = FALSE) {
      if (!is.null(private$.orig_arguments[[argument_name]])) {
        private$.arguments[[argument_name]] <- private$.orig_arguments[[argument_name]]
        private$.orig_arguments[[argument_name]] <- NULL
        ino_status(
          glue::glue("Reset `{argument_name}`."),
          verbose = verbose
        )
      }
    }

  ),
  active = list(

    #' @field f The \code{function} to be optimized.
    f = function (value) {
      if (missing(value)) {
        private$.f
      } else {
        ino_stop(
          "`$f` is read only."
        )
      }
    },

    #' @field npar The length of the first argument of \code{f}.
    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop(
          "`$npar` is read only."
        )
      }
    },

    #' @field arguments A \code{list} of specified additional arguments for \code{f}.
    arguments = function (value) {
      if (missing(value)) {
        private$.arguments
      } else {
        ino_stop(
          "`$arguments` is read only.",
          "To set an argument, use `$set_argument()`.",
          "To extract an argument by name, use `$get_argument()`.",
          "To remove an argument, use `$remove_argument()`."
        )
      }
    },

    #' @field true_parameter The true optimum \code{numeric} parameter vector of length \code{npar} (if available).
    true_parameter = function (value) {
      if (missing(value)) {
        private$.true_parameter
      } else {
        self$set_true_parameter(true_parameter = value, set_true_value = FALSE)
      }
    },

    #' @field true_value The true \code{numeric} optimum value of \code{f} (if available).
    true_value = function (value) {
      if (missing(value)) {
        private$.true_value
      } else {
        self$set_true_value(value)
      }
    },

    #' @field show_minimum A \code{logical}, set to \code{TRUE} (default) to
    #' show best minimum in \code{$best_value} and \code{$best_parameter}.
    show_minimum = function (value) {
      if (missing(value)) {
        private$.show_minimum
      } else {
        if (!isTRUE(value) && !isFALSE(value)) {
          ino_stop(
            "`show_minimum` must be `TRUE` or `FALSE`."
          )
        }
        private$.show_minimum <- value
      }
    },

    #' @field best_parameter The best found \code{numeric} parameter vector of length \code{npar} (if available).
    best_parameter = function (value) {
      if (missing(value)) {
        best_value <- self$best_value
        x <- self$summary(c("value", "parameter"))
        ind <- which(x$value == best_value)
        x$parameter[[ind]]
      } else {
        ino_stop(
          "`$best_parameter` is read only."
        )
      }
    },

    #' @field best_value The best found \code{numeric} value of \code{f} (if available).
    best_value = function (value) {
      if (missing(value)) {
        if (private$.show_minimum) {
          min(self$summary("value")$value)
        } else {
          max(self$summary("value")$value)
        }
      } else {
        ino_stop(
          "`$best_value` is read only."
        )
      }
    }

  )
)
