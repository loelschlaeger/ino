#' Nop Object (R6 Class)
#'
#' @description
#' A \code{Nop} object defines a *n*umerical *o*ptimization *p*roblem.
#'
#' @param which_optimizer
#' Select specified numerical optimizer. Either:
#' - \code{"all"} for all specified optimizer (default)
#' - a \code{character} vector of specified optimizer labels
#' - a \code{numeric} vector of optimizer IDs (see the output of \code{$print()})
#' @param verbose
#' A \code{logical}, which indicates whether progress should be printed.
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) progress.
#' The default is \code{getOption("ino_progress")}, which is set to \code{TRUE}
#' when the package is loaded.
#' @param ncores
#' An \code{integer}, the number of cores for parallel computation.
#' The default is \code{getOption("ino_ncores")}, which is set to \code{1}
#' when the package is loaded.
#' @param seed
#' Set a seed. No seed by default.
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
    #' Print method for numerical optimization problem.
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
    set_argument = function(...) {
      arguments <- list(...)
      if (length(arguments) == 0) {
        ino_stop(
          "Please specify an argument for 'f'."
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
          "Please specify 'argument_name'."
        )
      }
      if (!is.character(argument_name)) {
        ino_stop(
          "Input 'argument_name' must be a character."
        )
      }
      private$.check_add_arg_exists(argument_name)
      private$.arguments[[argument_name]]
    },

    #' @description
    #' Remove additional arguments for \code{f}.
    #' @param argument_name
    #' A \code{character}, the argument to remove.
    remove_argument = function (argument_name) {
      if (missing(argument_name)) {
        ino_stop(
          "Please specify 'argument_name'."
        )
      }
      if (!is.character(argument_name)) {
        ino_stop(
          "Input 'argument_name' must be a character."
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
    #' Evaluate the function.
    #' @param at
    #' A \code{numeric} vector of length \code{npar}.
    #' @return
    #' A \code{numeric} value.
    evaluate = function(at) {
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
    #' \code{\link[optimizeR]{set_optimizer}} function from the \{optimizeR\}
    #' package.
    #' @param label
    #' A \code{character}, a unique label for the optimizer.
    #' By default \code{label = NULL}, in which case the default label saved
    #' inside \code{optimizer} is used.
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
    #' Specify the initial point where the optimizer should start. Either:
    #' - the character \code{"random"} (the default) for random initial values
    #'   drawn from a standard normal distribution
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
    #' TODO
    #' @param simplify
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
      ncores = getOption("ino_ncores"), verbose = getOption("ino_progress"),
      simplify = TRUE
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

      ### other input checks
      if (!(is.numeric(runs) && length(runs) == 1 && runs > 0 && runs %% 1 == 0)) {
        ino_stop(
          "Input `runs` must be a positive integer."
        )
      }

      ### optimization
      optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
      if (!is.null(seed)) {
        set.seed(seed)
      }
      results <- list()
      if (ncores > 1 && runs > ncores) {
        cluster <- parallel::makeCluster(ncores)
        doSNOW::registerDoSNOW(cluster)
        on.exit(parallel::stopCluster(cluster))
        `%par_seq%` <- foreach::`%dopar%`
      } else {
        `%par_seq%` <- foreach::`%do%`
      }
      results <- foreach::foreach(
        run = 1:runs, .packages = c("ino", "R6"), .export = "Nop"
      ) %par_seq% {
        # TODO: bug when ncores > 1
        initial <- get_initial()
        lapply(optimizer_ids, private$.optimize, initial = initial)
      }

      ### process results
      if (save_results) {
        # TODO: save results
      }
      if (return_results) {
        # TODO: simplify
        return(results)
      } else {
        return(invisible(self))
      }
    },

    #' @description
    #' Test the configuration of numerical optimization problem.
    #' @param at
    #' A \code{numeric} of length \code{npar}, the point at which the
    #' function \code{f} and the specified optimizer are tested.
    #' Per default, \code{at = rnorm(self$npar)}, i.e., random values.
    #' @param time_limit
    #' A \code{numeric}, the time limit in seconds for testing the function
    #' call and the optimization.
    #' If no error occurred after \code{time_limit} seconds, the test is
    #' considered to be successful.
    #' By default, \code{time_limit = 10}.
    test = function (at = rnorm(self$npar), which_optimizer = "all", time_limit = 10) {

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
      ignore = integer()
    ) {

      ### input checks
      private$.check_add_arg_matrix_df(argument_name)
      if (!isTRUE(by_column) && !isFALSE(by_column)) {
        ino_stop(
          "Argument 'by_column' must be `TRUE` or `FALSE`."
        )
      }
      if (length(ignore) > 0) {
        if (!all(is.numeric(ignore) & ignore > 0 & ignore %% 1 == 0)) {
          ino_stop(
            "Argument 'ignore' must be a vector of indices."
          )
        }
      }

      ### standardizing
      argument <- self$get_argument(argument_name)
      orig_argument <- argument
      attr_argument <- attributes(argument)
      if (!by_column) argument <- t(argument)
      if (length(ignore) > 0) argument <- argument[, -ignore, drop = FALSE]
      argument <- scale(argument, center = center, scale = scale)
      if (is.data.frame(orig_argument)) argument <- as.data.frame(argument)
      if (length(ignore) > 0) argument <- replace(orig_argument, -ignore, argument)
      if (!by_column) argument <- t(argument)
      attributes(argument) <- attr_argument
      self$set_argument(argument) # TODO: overwrite

      ### temporally save original argument
      private$.save_orig_arg(orig_argument, argument_name) # TODO
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
      ignore = integer(), seed = NULL
    ) {

      ### input checks
      private$.check_add_arg_matrix_df(argument_name)
      if (!isTRUE(by_row) && !isFALSE(by_row)) {
        ino_stop(
          "Argument 'by_row' must be `TRUE` or `FALSE`."
        )
      }
      if (!all(is.numeric(ignore) & ignore > 0 & ignore %% 1 == 0)) {
        ino_stop(
          "Argument 'ignore' must be a vector of indices."
        )
      }
      if (!(is.character(how) && length(how) == 1) &&
          how %in% c("random", "first", "last", "similar", "unsimilar")) {
        ino_stop(
          "Argument 'how' must be one of `random`, `first`, `last`, `similar` or `unsimilar`."
        )
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
        arg_val_ign <- arg_val
        if (!is.null(ind_ign)) {
          arg_val_ign <- arg_val_ign[, -ind_ign, drop = FALSE]
        }
        kmeans_out <- stats::kmeans(argument, centers = centers)
        nc <- ceiling(arg_val_subset_length / centers)
        subset_ind <- c()
        for (i in 1:centers) {
          subset_ind_i <- which(kmeans_out$cluster == i)
          subset_ind <- c(subset_ind, sample(
            x = subset_ind_i,
            size = min(nc, length(subset_ind_i))
          ))
        }
        subset_ind <- sort(subset_ind)
      }
      if (!by_row) argument <- t(argument)

      ### temporally save original argument
      private$.save_orig_arg(orig_argument, argument_name) # TODO
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

    ### function that checks supplied value for target argument
    .check_target_arg = function (target_arg, arg_name) {
      stopifnot(is.character(arg_name), length(arg_name) == 1)
      if (!(is.numeric(target_arg) && length(target_arg) == private$.npar)) {
        ino_stop(
          glue::glue("Argument '{arg_name}' must be a numeric vector of length {private$.npar}.")
        )
      }
    },

    ### function that checks if argument exists
    .check_add_arg_exists = function (argument_name) {
      stopifnot(is.character(argument_name), length(argument_name) == 1)
      if (!argument_name %in% names(private$.arguments)) {
        ino_stop(
          glue::glue("Argument '{argument_name}' does not exist for function '{private$.f_name}'."),
          glue::glue("Use `$set_argument(\"{argument_name}\" = ...)`.")
        )
      }
    },

    ### function that checks if argument is matrix or data.frame
    .check_add_arg_matrix_df = function (argument_name) {
      private$.check_add_arg_exists(argument_name)
      argument <- private$.arguments[[argument_name]]
      if (!is.data.frame(argument) && !is.matrix(argument)) {
        ino_stop(
          glue::glue("Argument '{argument_name}' must be a `data.frame` or a `matrix`."),
        )
      }
    },

    ### function that checks if all required arguments for function are specified
    .check_add_args_complete = function () {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        if (!nzchar(args_all[[arg]]) & is.name(args_all[[arg]])) {
          private$.check_add_arg_exists(arg)
        }
      }
    },

    ### function that returns numeric IDs of specified optimizer
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
        args = list(
          "optimizer" = private$.optimizer[[optimizer_id]],
          "f" = private$.f,
          "p" = initial
        )
      )
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

    #' @field npar The length of the first argument of \code{f}.
    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop(
          "'$npar' is read only."
        )
      }
    },

    #' @field arguments A \code{list} of specified additional arguments for \code{f}.
    arguments = function(value) {
      if (missing(value)) {
        private$.arguments
      } else {
        ino_stop(
          "'$arguments' is read only.",
          "To set an argument, use `$set_argument()`.",
          "To extract an argument by name, use `$get_argument()`.",
          "To remove an argument, use `$remove_argument()`."
        )
      }
    },

    #' @field true_parameter The true optimum parameter vector of length \code{npar} (if available).
    true_parameter = function(value) {
      if (missing(value)) {
        private$.true_parameter
      } else {
        self$set_true_parameter(true_parameter = value, set_true_value = FALSE)
      }
    },

    #' @field true_value The true optimum value of \code{f} (if available).
    true_value = function(value) {
      if (missing(value)) {
        private$.true_value
      } else {
        self$set_true_value(value)
      }
    }

  )
)
