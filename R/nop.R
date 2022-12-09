#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a numerical optimization problem.
#'
#' @param which_optimizer
#' Either:
#' - \code{"all"} for all specified optimizer (default)
#' - a \code{character} vector of specified optimizer labels
#' - a \code{numeric} vector of optimizer ids (see the output of \code{$print()})
#' @param argument_name
#' A \code{character}, the argument name.
#' @param which_argument_combination
#' Only relevant if any argument to \code{f} was specified with multiple values,
#' i.e., if any \code{$set_argument(..., multiple_values = TRUE)}.
#' In that case, either:
#' - \code{"all"} for all argument combinations (default)
#' - a \code{numeric} vector of combination ids (see \code{$argument_combinations})
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
#' ackley <- Nop$new(f = f_ackley, npar = 2)
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
    #' Use \code{$add_par()} to add additional parameter for \code{f}.
    #' @param npar
    #' An \code{integer}, the length of the first argument of \code{f} (the
    #' argument over which \code{f} is optimized).
    #' @return
    #' A new \code{Nop} object.
    initialize = function (f, npar) {
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
    },

    #' @description
    #' Print method for numerical optimization problem.
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
        cat(" Additional arguments:\n")
        arg_names <- names(private$.arguments)
        for (i in seq_len(private$.narguments)) {
          cat(" -", arg_names[i], "\n")
        }
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
      if (private$.optimizer_n == 0) {
        cat(
          " No optimizer specified.\n"
        )
      } else {
        for (i in 1:private$.optimizer_n) {
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
    #' Optionally set an additional argument for \code{f}.
    #' @param argument_value
    #' Any \code{R} object, the value of argument \code{argument_name} for \code{f}.
    #' @param multiple_values
    #' TODO
    set_argument = function(argument_name, argument_value, multiple_values = FALSE) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify 'argument_name'."
        )
      }
      if (!(is.character(argument_name) && length(argument_name) == 1)) {
        ino_stop(
          "Input 'argument_name' must be a single character."
        )
      }
      if (argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue("Argument '{argument_name}' already exists.")
        )
      }
      if (!(isTRUE(multiple_arguments) || isFALSE(multiple_arguments))) {
        ino_stop(
          "Argument 'multiple_arguments' must be TRUE or FALSE."
        )
      }
      private$.arguments[[argument_name]] <- argument_value
      private$.narguments <- private$.narguments + 1
      if (multiple_arguments) {
        private$.multiple_arguments <- c(private$.multiple_arguments, multiple_arguments)
      }
      invisible(self)
    },

    #' @description
    #' Optionally remove additional arguments for \code{f}.
    remove_argument = function(argument_name) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify 'argument_name'."
        )
      }
      if (!(is.character(argument_name) && length(argument_name) == 1)) {
        ino_stop(
          "Input 'argument_name' must be a single character."
        )
      }
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue("Argument '{argument_name}' does not exist.")
        )
      }
      arg_id <- which(names(private$.arguments) == argument_name)
      private$.arguments[arg_id] <- NULL
      private$.narguments <- private$.narguments - 1
    },

    #' @description
    #' Optionally set the true optimum value.
    #' @param true_value
    #' A \code{numeric}, the value of \code{f} at its optimum.
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
    #' Optionally set the true optimum parameter.
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
          "Argument 'set_true_value' must be TRUE or FALSE."
        )
      }
      private$.true_parameter <- true_parameter
      if (set_true_value) {
        private$.true_value <- private$.evaluate(at = true_parameter)
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
    #' By default \code{NULL}, in which case the default label from
    #' \code{optimizer} is used.
    set_optimizer = function (optimizer, label = NULL) {
      if (missing(optimizer)) {
        ino_stop(
          "Please specify argument 'optimizer'."
        )
      }
      if (!inherits(optimizer, "optimizer")) {
        ino_stop(
          "Argument 'optimizer' must be an object of class 'optimizer'."
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
          glue::glue("Label '{label}' already exists, please choose another one.")
        )
      }
      private$.optimizer_n <- private$.optimizer_n + 1
      private$.optimizer[[private$.optimizer_n]] <- optimizer
      private$.optimizer_label <- c(private$.optimizer_label, label)
      invisible(self)
    },

    #' @description
    #' Remove numerical optimizer.
    remove_optimizer = function (which_optimizer = "all") {
      ids <- private$.get_optimizer_ids(which_optimizer)
      private$.optimizer <- private$.optimizer[-ids]
      private$.optimizer_n <- private$.optimizer_n - length(ids)
      private$.optimizer_label <- private$.optimizer_label[-ids]
      invisible(self)
    },

    #' @description
    #' Print details of numerical optimizer.
    optimizer_details = function (which_optimizer = "all") {
      ids <- private$.get_optimizer_ids(which_optimizer)
      for (id in ids) {
        cat(paste0(id, ":"), private$.optimizer_label[id], "\n")
        str(private$.optimizer[id])
        cat("\n")
      }
      invisible(self)
    },

    #' @description
    #' Evaluate the function.
    #' @param at
    #' A \code{numeric} vector of length \code{$npar}.
    #' @return
    #' TODO
    evaluate = function(at, which_argument_combination = "all") {
      private$.check_target_arg(at, arg_name = "at")
      argument_combination <- private$.argument_combination(which_argument_combination)
      private$.evaluate(at, argument_combination)
    },

    #' @description
    #' Optimize the function.
    #' @param initial
    #' Either:
    #' - the character \code{"random"} (the default) for random initial values
    #'   drawn from a standard normal distribution
    #' - a \code{numeric} vector of length \code{$npar}, the starting point for
    #'   optimization
    #' - a \code{function} without any arguments that returns a \code{numeric}
    #' @param runs
    #' An \code{integer}, the number of optimization.
    #' By default, \code{runs = 1}.
    #' @param which_optimizer
    #' Either:
    #' - \code{"all"} for all specified optimizer (default)
    #' - a \code{character} vector of specified optimizer labels
    #' - a \code{numeric} vector of optimizer ids (see the output of \code{$print()})
    #' @param seed
    #' Set a seed. No seed by default.
    #' @return
    #' TODO
    optimize = function(
      initial = rnorm(self$npar), runs = 1, which_optimizer = "all", seed = NULL
    ) {
      ### make `initial` to function call `get_initial`
      get_initial <- if (identical(initial, "random")) {
        function() rnorm(private$.npar)
      } else if (is.numeric(initial) && length(initial) == private$.npar) {
        function() initial
      } else if (is.function(initial)) {
        try_initial <- try(initial(), silent = TRUE)
        if (!(is.numeric(initial) && length(initial) == private$.npar)) {
          ino_stop()
        }
        initial
      } else {
        ino_stop()
      }
      if (!(is.numeric(runs) && length(runs) == 1 && runs > 0 && runs %% 1 == 0)) {
        ino_stop()
      }
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (!is.null(seed)) {
        set.seed(seed)
      }
      results <- list()
      for (i in seq_len(private$.optimizer_n)) {
        optimizer <- private$.optimizer[[i]]
        results[[i]] <- private$.optimize(initial, optimizer)
      }
      return(results)
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
    #' By default, \code{time_limit = 3}.
    test = function (at = rnorm(self$npar), time_limit = 3) {

      ### test f
      out <- suppressWarnings(try(self$evaluate(at), silent = TRUE))
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
    standardize = function(argument_name, by_column = TRUE, center = TRUE, scale = TRUE, ignore = integer()) {

      invisible(self)
    },

    #' @description
    #' TODO
    subset = function(argument_name, by_row = TRUE, how = "first", proportion = 0.5, ignore = integer()) {

      invisible(self)
    },

    #' @description
    #' TODO
    optima = function (digits = 2) {

    },

    summary = function () {

    },

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
    .multiple_arguments = character(),
    .argument_combinations = data.frame(),

    .true_parameter = NULL,
    .true_value = NULL,

    .optimizer = list(),
    .optimizer_n = 0,
    .optimizer_label = character(0),

    .nrecords = 0,
    .best_parameter = NA_real_,
    .best_value = NA_real_,

    .evaluate = function(at, which_argument_combination) {
      if (private$.narguments == 0) {
        do.call(
          what = private$.f,
          args = list(at)
        )
      } else {
        do.call(
          what = private$.f,
          args = c(list(at), add_args)
        )
      }
    },

    .optimize = function(initial, optimizer_id) {
      do.call(
        what = optimizeR::apply_optimizer,
        args = list(
          "optimizer" = private$.optimizer[[optimizer_id]],
          "f" = private$.f,
          "p" = initial
        )
      )
    },

    .check_target_arg = function (target_arg, arg_name) {
      if (!(is.numeric(target_arg) && length(target_arg) == private$.npar)) {
        ino_stop(
          glue::glue("Argument '{arg_name}' must be a numeric vector of length {private$.npar}.")
        )
      }
    },

    .get_optimizer_ids = function (which_optimizer) {
      if (private$.optimizer_n == 0) {
        ino_stop(
          "No optimizer specified."
        )
      }
      if (identical(which_optimizer, "all")) {
        return(1:private$.optimizer_n)
      } else if (is.character(which_optimizer)) {
        ids <- which(private$.optimizer_label %in% which_optimizer)
      } else if (is.numeric(which_optimizer)) {
        ids <- which(1:private$.optimizer_n %in% which_optimizer)
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
    }

  ),
  active = list(

    #' @field f The \code{function} to be optimized.
    f = function(value) {
      if (missing(value)) {
        private$.f
      } else {
        ino_stop(
          "`$f` is read only."
        )
      }
    },

    #' @field npar The length of the first argument of \code{$f}.
    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop(
          "'$npar' is read only."
        )
      }
    },

    #' @field true_parameter The true optimum parameter vector of length \code{$npar} (if available).
    true_parameter = function(value) {
      if (missing(value)) {
        private$.true_parameter
      } else {
        self$set_true_parameter(true_parameter = value, set_true_value = FALSE)
      }
    },

    #' @field true_value The true optimum value of \code{$f} (if available).
    true_value = function(value) {
      if (missing(value)) {
        private$.true_value
      } else {
        self$set_true_value(value)
      }
    },

    #' @field arguments
    arguments = function(value) {
      if (missing(value)) {
        private$.arguments
      } else {
        ino_stop(
          "'$arguments' is read only.",
          "To set an argument, please use '$set_argument()'.",
          "To remove an argument, please use '$remove_argument()'."
        )
      }
    }

    #' @field argument_combinations
    argument_combinations = function(value) {
      if (missing(value)) {
        private$.argument_combinations
      } else {
        ino_stop(
          "'$argument_combinations' is read only."
        )
      }
    }
  )
)
