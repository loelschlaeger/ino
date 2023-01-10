#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.
#'
#' @param which_optimizer
#' Select specified numerical optimizer. Either:
#' - \code{"all"} for all specified optimizer (default)
#' - a \code{character} vector of specified optimizer labels
#' - a \code{numeric} vector of optimizer ids (see the output of \code{$print()})
#' @param ncores
#' An \code{integer}, the number of cores for parallel computation.
#' The default is \code{getOption("ino_ncores")}, which is set to \code{1}
#' when the package is loaded.
#' @param verbose
#' A \code{logical}, which indicates whether progress should be printed.
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) progress.
#' The default is \code{getOption("ino_verbose")}, which is set to \code{TRUE}
#' when the package is loaded.
#'
#' @details
#' # Initialize
#' TODO
#' ## Step 1: Create a new \code{Nop} object.
#'
#' ## Step 2: Add additional arguments.
#'
#' ## Step 2: Specify numerical optimizer.
#'
#' # Optimize
#' TODO
#'
#' # Evaluate
#' TODO
#'
#' @examples
#' ackley <- Nop$new(f = f_ackley, npar = 2)$
#'   set_optimizer(optimizer_nlm())$
#'   optimize()
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Create a new numerical optimization problem.
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
          "Please specify argument 'f'."
        )
      }
      if (!is.function(f)) {
        ino_stop(
          "Argument 'f' is not a function."
        )
      }
      if (is.null(formals(f))) {
        ino_stop(
          "The function 'f' should have at least one argument."
        )
      }
      if (missing(npar)) {
        ino_stop(
          "Please specify argument 'npar'."
        )
      }
      if (!(is.numeric(npar) && length(npar) == 1 && npar > 0 && npar %% 1 == 0)) {
        ino_stop(
          "Argument 'npar' must be a positive integer."
        )
      }
      private$.f <- f
      private$.f_name <- deparse(substitute(f))
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
        glue::glue(" Optimize over: {private$.f_target} (dimension {private$.npar})"), "\n",
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
      if (private$.noptimizer == 0) {
        cat(
          " No optimizer specified.\n"
        )
      } else {
        for (i in 1:private$.noptimizer) {
          cat(
            glue::glue(" {i}: {private$.optimizer_label[i]}"), "\n", sep = ""
          )
        }
      }
      cat(
        crayon::underline("Optimization results:"), "\n", sep = ""
      )
      if (private$.nrecords == 0) {
        cat(
          " No optimization records.\n"
        )
      } else {
        cat(
          glue::glue(" Optimization records: {private$.nrecords}"), "\n",
          glue::glue(" Found optimum at: {paste(private$.best_par, collapse = ' ')}"), "\n",
          glue::glue(" Found optimum value: {paste(private$.best_val, collapse = ' ')}"), "\n",
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
          "Please specify an argument."
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
            glue::glue("Argument '{argument_names[i]}' already exists."),
            glue::glue("Please use `$remove_argument('{argument_names[i]}')` first.")
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
    #' Remove additional arguments for \code{f}.
    #' @param argument_names
    #' A \code{character} (vector), the names of arguments to remove.
    remove_argument = function (argument_names) {
      if (missing(argument_names)) {
        ino_stop(
          "Please specify 'argument_names'."
        )
      }
      if (!is.character(argument_names)) {
        ino_stop(
          "Input 'argument_names' must be a character (vector)."
        )
      }
      for (i in seq_along(argument_names)) {
        if (!argument_names[i] %in% names(private$.arguments)) {
          ino_stop(
            glue::glue("Argument '{argument_names[i]}' does not exist.")
          )
        }
        arg_id <- which(names(private$.arguments) == argument_names[i])
        private$.arguments[arg_id] <- NULL
        private$.narguments <- private$.narguments - 1
      }
      invisible(self)
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
          "Argument 'true_value' must be a single numeric."
        )
      }
      private$.true_value <- true_value
      invisible(self)
    },

    #' @description
    #' Optionally set the true optimum parameter
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
          "Argument 'set_true_value' must be `TRUE` or `FALSE`."
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
    #' \code{\link[optimizeR]{set_optimizer}} function from the {optimizeR}
    #' package.
    #' @param label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label from
    #' \code{optimizer} is used.
    set_optimizer = function (optimizer, label = NULL) {
      if (missing(optimizer)) {
        ino_stop(
          "Please specify argument 'optimizer'."
        )
      }
      if (!inherits(optimizer, "optimizer")) {
        ino_stop(
          "Argument 'optimizer' must be an object of class 'optimizer'.",
          "Please use `optimizeR::set_optimizer()` to create such an object."
        )
      }
      if (is.null(label)) {
        label <- optimizer$opt_name
      }
      if (!(is.character(label) && length(label) == 1)) {
        ino_stop(
          "Argument 'label' must be a single character."
        )
      }
      if (label %in% private$.optimizer_label) {
        ino_stop(
          glue::glue("Label '{label}' already exists, please choose another one."),
          "Note that label for optimizer must be unique for identification."
        )
      }
      private$.noptimizer <- private$.noptimizer + 1
      private$.optimizer[[private$.noptimizer]] <- optimizer
      private$.optimizer_label <- c(private$.optimizer_label, label)
      invisible(self)
    },

    #' @description
    #' Remove numerical optimizer.
    remove_optimizer = function (which_optimizer = "all") {
      opt_ids <- private$.get_optimizer_ids(which_optimizer)
      private$.optimizer <- private$.optimizer[-opt_ids]
      private$.noptimizer <- private$.noptimizer - length(opt_ids)
      private$.optimizer_label <- private$.optimizer_label[-opt_ids]
      invisible(self)
    },

    #' @description
    #' Optimize the function.
    #' @param initial
    #' Specify the point where the optimizer should start. Either:
    #' - the character \code{"random"} (the default) for random initial values
    #'   drawn from a standard normal distribution
    #' - a \code{numeric} vector of length \code{$npar}, the starting point for
    #'   optimization
    #' - a \code{function} without any arguments that returns a \code{numeric}
    #'   vector of length \code{$npar}
    #' @param runs
    #' An \code{integer}, the number of optimization.
    #' By default, \code{runs = 1}.
    #' @param seed
    #' Set a seed. No seed by default.
    #' @param save_results
    #' A \code{logical}, whether to save the optimization results inside the
    #' object.
    #' By default, \code{save_results = TRUE}.
    #' That allows to analyze them later with the \code{$summary()} method.
    #' @param return_results
    #' A \code{logical}, whether to return the optimization results.
    #' By default, \code{return_results = FALSE}.
    #' @return
    #' The return value depends on the \code{return_results} argument:
    #' - If \code{return_results = TRUE}, the function returns a \code{list}
    #'   of length \code{runs} of optimization results for each run.
    #' - If \code{return_results = FALSE}, the object is returned invisibly.
    optimize = function (
      initial = "random", runs = 1, which_optimizer = "all", seed = NULL,
      save_results = TRUE, return_results = FALSE,
      ncores = getOption("ino_ncores"), verbose = getOption("ino_verbose")
    ) {
      ### make `initial` to function call `get_initial`
      get_initial <- if (identical(initial, "random")) {
        function() rnorm(private$.npar)
      } else if (is.numeric(initial) && length(initial) == private$.npar) {
        function() initial
      } else if (is.function(initial)) {
        try_initial <- try(initial(), silent = TRUE)
        if (!(is.numeric(initial) && length(initial) == private$.npar)) {
          ino_stop(
            glue::glue("The function `initial` should return a numeric vector of length {private$.npar}.")
          )
        }
        initial
      } else {
        ino_stop(
          "Input `initial` is missspecified."
        )
      }
      if (!(is.numeric(runs) && length(runs) == 1 && runs > 0 && runs %% 1 == 0)) {
        ino_stop(
          "Input `runs` must be a positive integer."
        )
      }
      if (!isTRUE(save_results) && !isFALSE(save_results)) {
        ino_stop(
          "Input `save_results` must be either TRUE or FALSE."
        )
      }
      if (!isTRUE(return_results) && !isFALSE(return_results)) {
        ino_stop(
          "Input `return_results` must be either TRUE or FALSE."
        )
      }
      if (!(is.numeric(ncores) && length(ncores) == 1 && ncores > 0 && ncores %% 1 == 0)) {
        ino_stop(
          "Input `ncores` must be a positive integer."
        )
      }
      if (!isTRUE(verbose) && !isFALSE(verbose)) {
        ino_stop(
          "Input `verbose` must be either TRUE or FALSE."
        )
      }
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (!is.null(seed)) {
        set.seed(seed)
      }
      ### set parallel
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
        run = 1:runs, .packages = c("ino")
      ) %par_seq% {
        initial <- get_initial()
        lapply(optimizer_ids, function (i) private$.optimize(initial, i))
      }
      if (save_results) {
        private$.save_results(results)
      }
      if (return_results) {
        return(results)
      } else {
        return(invisible(self))
      }
    },

    #' @description
    #' Test the configuration of numerical optimization problem.
    #' @param at
    #' A \code{numeric} of length \code{self$npar}, the point at which
    #' function (\code{$f}) and the specified optimizer are tested.
    #' Per default, \code{at = rnorm(self$npar)}.
    #' @param time_limit
    #' A \code{numeric}, the time limit in seconds for testing the function
    #' call and the optimization. If no error occurred after \code{time_limit}
    #' seconds, the test is considered to be successful.
    #' By default, \code{time_limit = 10}.
    test = function (
      at = rnorm(self$npar), which_optimizer = "all", time_limit = 10
      ) {

      ### test f
      out <- suppressWarnings(
        try(self$evaluate(at), silent = TRUE)
      )
      if (inherits(out, "try-error") || !is.numeric(out)) {
        ino_stop(
          "Test function call failed. I used the following inputs:",
          glue::glue("{private$f_target} = {paste(at, collapse = ' ')}"), # TODO other inputs
          "Result is not a numeric."
        )

      }

      ### test optimizer
      # TODO

      invisible(self)
    },

    #' @description
    #' TODO
    #' @param argument_name
    #' TODO
    #' @param by_column
    #' TODO
    #' @param center
    #' TODO
    #' @param scale
    #' TODO
    #' @param ignore
    #' TODO
    standardize = function (
      argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
      ignore = integer()
    ) {
      # TODO: cache argument, reset argument
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param argument_name
    #' TODO
    #' @param by_row
    #' TODO
    #' @param how
    #' A \code{character}, specifying ..., one of
    #' - \code{"first"}
    #' - \code{"last"}
    #' - \code{"random"}
    #' - \code{"similar"}
    #' - \code{"different"}
    #' @param proportion
    #' TODO
    #' @param ignore
    #' TODO
    subset = function (
      argument_name, by_row = TRUE, how = "first", proportion = 0.5,
      ignore = integer()
    ) {
      # TODO: cache argument, reset argument
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param digits
    #' TODO
    optima = function (digits = 2) {

    },

    #' @description
    #' TODO
    summary = function () {

    },

    #' @description
    #' TODO
    plot = function () {

    }

  ),
  private = list(

    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .arguments = list(),
    .narguments = 0,

    .true_parameter = NULL,
    .true_value = NULL,

    .optimizer = list(),
    .noptimizer = 0,
    .optimizer_label = character(0),

    .records = list(),
    .nrecords = 0,
    .best_parameter = NA_real_,
    .best_value = NA_real_,

    .check_target_arg = function (target_arg, arg_name) {
      stopifnot(is.character(arg_name), length(arg_name) == 1)
      if (!(is.numeric(target_arg) && length(target_arg) == private$.npar)) {
        ino_stop(
          glue::glue("Argument '{arg_name}' must be a numeric vector of length {private$.npar}.")
        )
      }
    },

    .check_add_args_complete = function () {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        if (!nzchar(args_all[[arg]]) & is.name(args_all[[arg]])) {
          if (!arg %in% names(private$.arguments)) {
            ino_stop(
              glue::glue("Argument '{arg}' must be specified for '{private$.f_name}'."),
              glue::glue("Use `$set_argument(\"{arg}\" = ...)`.")
            )
          }
        }
      }
    },

    .get_optimizer_ids = function (which_optimizer) {
      if (private$.noptimizer == 0) {
        ino_stop(
          "No optimizer specified."
        )
      }
      if (identical(which_optimizer, "all")) {
        return(1:private$.noptimizer)
      } else if (is.character(which_optimizer)) {
        ids <- which(private$.optimizer_label %in% which_optimizer)
      } else if (is.numeric(which_optimizer)) {
        ids <- which(1:private$.noptimizer %in% which_optimizer)
      } else {
        ino_stop(
          "Argument 'which_optimizer' is misspecified."
        )
      }
      if (length(ids) == 0) {
        ino_stop(
          "Please check argument 'which_optimizer', it fits to no optimizer."
        )
      }
      return(ids)
    },

    .evaluate = function (at) {
      at <- list(at)
      names(at) <- private$.f_target
      do.call(
        what = private$.f,
        args = c(at, private$.arguments)
      )
    },

    .optimize = function (initial, optimizer_id) {
      do.call(
        what = optimizeR::apply_optimizer,
        args = list(
          "optimizer" = private$.optimizer[[optimizer_id]],
          "f" = private$.f,
          "p" = initial
        )
      )
      # TODO: here needs to go check if optimization is to be continued with full data
    },

    .save_results = function (results) {
      stop("not yet implemented.")
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

    #' @field npar The length of the first argument of \code{$f}.
    npar = function (value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop(
          "'$npar' is read only."
        )
      }
    },

    #' @field arguments A \code{list} of specified additional arguments for \code{f}.
    arguments = function (value) {
      if (missing(value)) {
        private$.arguments
      } else {
        ino_stop(
          "'$arguments' is read only.",
          "To set an argument, please use `$set_argument()`.",
          "To remove an argument, please use `$remove_argument()`."
        )
      }
    },

    #' @field true_parameter The true optimum parameter vector of length \code{$npar} (if available).
    true_parameter = function (value) {
      if (missing(value)) {
        private$.true_parameter
      } else {
        self$set_true_parameter(true_parameter = value, set_true_value = FALSE)
      }
    },

    #' @field true_value The true optimum value of \code{$f} (if available).
    true_value = function (value) {
      if (missing(value)) {
        private$.true_value
      } else {
        self$set_true_value(value)
      }
    }

  )
)
