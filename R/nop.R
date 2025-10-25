#' Nop Object
#'
#' @description
#' A `Nop` object defines a numerical optimization problem.
#'
#' @param f \[`function`\]\cr
#' A `function` to be optimized (the so-called objective function).
#'
#' It is expected that
#'
#' 1. `f` has at least one `numeric` argument,
#' 2. the return value of `f` is of the structure `numeric(1)`.
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) that get optimized (the so-called target arguments).
#'
#' All target arguments must be `numeric`.
#'
#' Can be `NULL` (default), then the first function argument is selected.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target argument, i.e., the length(s) of the
#' argument(s) specified via `target`.
#'
#' @param argument_name \[`character(1)`\]\cr
#' The name of a fixed argument for the objective function.
#'
#' @param runs \[`integer(1)`\]\cr
#' The number of optimization runs.
#'
#' @param which_direction \[`character()`\]\cr
#' Selects the direction of optimization. One or both of:
#'
#' - `"min"` for minimization,
#' - `"max"` for maximization.
#'
#' @param which_optimizer \[`character()` | `integer()`\]\cr
#' Selects numerical optimizers. Either:
#'
#' - `"all"` for all specified optimizers,
#' - specific optimizer labels,
#' - specified optimizer ids as defined in the `print()` output.
#'
#' @param only_original \[`logical(1)\]\cr
#' Include only optima obtained on the original problem?
#'
#' @param digits \[`integer(1)\]\cr
#' The number of decimal places.
#'
#' @details
#' # Getting started
#'
#' ## Step 1: Create a `Nop` object
#' Call `object <- Nop$new(f, target, npar, ...)` where
#' - `f` is the objective function,
#' - `target` are the names of the target arguments,
#' - `npar` specifies the lengths of the target arguments,
#' - and `...` are additional arguments for `f`.
#'
#' You can now evaluate the objective function via the `$evaluate()` method.
#'
#' ## Step 2: Specify numerical optimizers
#' Call `object$set_optimizer(<optimizer object>)`, where
#' `<optimizer object>` is an object of class `optimizer`, which can
#' be created via the `{optimizeR}` package (please refer to
#' [the package homepage](https://loelschlaeger.de/optimizeR/) for details).
#'
#' For example,
#' - `optimizeR::Optimizer$new(which = "stats::nlm")` defines the
#'   \code{\link[stats]{nlm}} optimizer,
#' - `optimizeR::Optimizer$new(which = "stats::optim")` defines the
#'   \code{\link[stats]{optim}} optimizer.
#'
#' ## Step 3: Select initial values
#' Call initialization methods to define starting values for the
#' optimization, for example:
#' - `object$initialize_fixed()` for fixed initial values,
#' - `object$initialize_random()` for random initial values,
#' - `object$initialize_continue()` for initial values based on parameter
#'   estimates from previous optimization runs.
#'
#' ## Step 4: Optimization
#' Call `object$optimize()` for the optimization.
#'
#' ## Step 5: Analyze the results
#' - `$results` returns a `tibble` of the optimization results,
#' - `$optima()` lists all identified optima,
#' - `$minimum` and `$maximum` return the best minimizer and maximizer
#'
#' # Progress during optimization
#' Displaying progress during multiple optimization runs via the
#' `{progressr}` package is supported. To get started, run
#' \preformatted{progressr::handlers(global = TRUE)}
#' and see \code{\link[progressr]{handlers}} for details.
#'
#' # Parallel optimization
#' Parallel computation of multiple optimization runs via the `{future}`
#' package is supported. To get started, run one of
#' \preformatted{future::plan(future::multisession)}
#' and see \code{\link[future]{plan}} for details.
#'
#' @examples
#' ### define objective function, optimizer and initial values
#' Nop_ackley <- Nop$new(f = TestFunctions::TF_ackley, npar = 2)$
#'   set_optimizer(optimizeR::Optimizer$new(which = "stats::nlm"))$
#'   initialize_random(runs = 20)
#'
#' ### plot function surface and initial values
#' Nop_ackley |> ggplot2::autoplot()
#'
#' ### minimize objective function
#' Nop_ackley$optimize(which_direction = "min")
#'
#' ### show optima
#' Nop_ackley$optima(digits = 0)
#'
#' ### show best value and parameter across all minimizations
#' Nop_ackley$minimum
#'
#' @export

Nop <- R6::R6Class(

  classname = "Nop",

  public = list(

    #' @description
    #' Creates a new `Nop` object.
    #'
    #' The output has an associated \code{\link[ggplot2]{autoplot}} method.
    #'
    #' @param gradient \[`function` | `NULL`\]\cr
    #' Optionally a `function` that returns the gradient of `f`.
    #'
    #' The function call of `gradient` must be identical to `f`.
    #'
    #' Ignored for optimizers that do not support user-supplied gradient.
    #'
    #' @param hessian \[`function` | `NULL`\]\cr
    #' Optionally a `function` that returns the Hessian of `f`.
    #'
    #' The function call of `hessian` must be identical to `f`.
    #'
    #' Ignored for optimizers that do not support user-supplied Hessian.
    #'
    #' @param ...
    #' Optionally additional function arguments passed to `f` (and `gradient`
    #' and `hessian`, if specified) that are fixed during the optimization.

    initialize = function(
      f, target = NULL, npar, gradient = NULL, hessian = NULL, ...
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(f),
        var_name = "f"
      )
      oeli::input_check_response(
        check = oeli::check_missing(npar),
        var_name = "npar"
      )

      ### build Objective object
      private$.objective <- optimizeR::Objective$new(
        f = f, target = target, npar = npar, ...
      )
      if (!is.null(gradient)) {
        oeli::input_check_response(
          check = checkmate::check_function(gradient),
          var_name = "gradient"
        )
        private$.objective$set_gradient(gradient = gradient, .verbose = FALSE)
      }
      if (!is.null(hessian)) {
        oeli::input_check_response(
          check = checkmate::check_function(hessian),
          var_name = "hessian"
        )
        private$.objective$set_hessian(hessian = hessian, .verbose = FALSE)
      }

      ### build Storage object
      private$.results <- oeli::Storage$new()

    },

    #' @description
    #' Manages fixed arguments for the objective function.
    #'
    #' @param action \[`character(1)`\]\cr
    #' One of:
    #' - `"set"` to set an argument,
    #' - `"get"` to extract an argument value,
    #' - `"remove"` to remove an argument,
    #' - `"reset"` to reset an argument to its original value,
    #' - `"modify"` to modify an argument value.
    #'
    #' Note that `"set"` overrides an argument value, while `"modify"` preserves
    #' the original value, which can be recovered via `"reset"`.
    #'
    #' @param ...
    #' Additional parameters depending on `action`:
    #' - named arguments if `action = "set"` or `"modify"`,
    #' - a single argument name if `action = "get"`, `"remove"`, or `"reset"`.

    fixed_argument = function(action, ...) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(action),
        var_name = "action"
      )
      action <- oeli::match_arg(
        action, choices = c("set", "get", "remove", "reset", "modify")
      )
      args <- list(...)

      ### different actions
      if (action == "set") {
        oeli::input_check_response(
          check = checkmate::check_list(args, min.len = 1, names = "unique"),
          var_name = "..."
        )
        for (i in seq_along(args)) {
          do.call(
            private$.objective$set_argument,
            c(args[i], list(".overwrite" = TRUE, ".verbose" = self$verbose))
          )
        }
      }
      if (action == "get") {
        arg_name <- unlist(args)
        oeli::input_check_response(
          check = checkmate::check_string(arg_name),
          var_name = "..."
        )
        if (arg_name %in% private$.objective$fixed_arguments) {
          return(
            private$.objective$get_argument(arg_name, .verbose = FALSE)
          )
        } else {
          cli::cli_abort("Argument {.val {arg_name}} not available.")
        }
      }
      if (action == "remove") {
        arg_name <- unlist(args)
        oeli::input_check_response(
          check = checkmate::check_string(arg_name),
          var_name = "..."
        )
        private$.objective$remove_argument(arg_name, .verbose = FALSE)
        if (arg_name %in% names(private$.original_arguments)) {
          arg_id <- which(names(private$.original_arguments) == arg_name)
          private$.original_arguments[arg_id] <- NULL
        }
        private$.print_status("Removed argument {.var {arg_name}}.")
      }
      if (action == "modify") {
        oeli::input_check_response(
          check = checkmate::check_list(args, min.len = 1, names = "unique"),
          var_name = "..."
        )
        for (i in seq_along(args)) {
          arg_name <- names(args)[i]
          current_argument <- self$fixed_argument("get", arg_name)
          if (!is.null(current_argument)) {
            if (is.null(private$.original_arguments[[arg_name]])) {
              private$.original_arguments[[arg_name]] <- current_argument
            }
            do.call(
              private$.objective$set_argument,
              c(args[i], list(".overwrite" = TRUE, ".verbose" = FALSE))
            )
            private$.print_status("Modified argument {.var {arg_name}}.")
          } else {
            cli::cli_warn("Argument {.var {arg_name}} not available.")
          }
        }
      }
      if (action == "reset") {
        arg_name <- unlist(args)
        oeli::input_check_response(
          check = checkmate::check_string(arg_name),
          var_name = "..."
        )
        if (!is.null(private$.original_arguments[[arg_name]])) {
          original_argument <- private$.original_arguments[[arg_name]]
          arg <- list("set", original_argument)
          names(arg) <- c("action", arg_name)
          suppressMessages(do.call(self$fixed_argument, arg))
          private$.original_arguments[[arg_name]] <- NULL
          private$.print_status("Reset argument {.var {arg_name}}.")
        } else {
          cli::cli_warn("Argument {.var {arg_name}} cannot be reset.")
        }
      }
      invisible(self)

    },

    #' @description
    #' Reduces a fixed argument for the objective function.
    #'
    #' @param proportion,how,centers,byrow,ignore
    #' Passed on to \code{\link[portion]{portion}}.

    reduce_argument = function(
      argument_name,
      proportion = 0.5,
      how = "random",
      centers = 2L,
      byrow = TRUE,
      ignore = integer()
    ) {
      oeli::input_check_response(
        check = oeli::check_missing(argument_name),
        var_name = "argument_name"
      )
      original_argument <- self$fixed_argument("get", argument_name)
      reduced_argument <- portion::portion(
        x = original_argument,
        proportion = proportion,
        how = how,
        centers = centers,
        byrow = byrow,
        ignore = ignore
      )
      arg <- list("modify", reduced_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
      private$.print_status("Reduced argument {.val {argument_name}}.")
      invisible(self)
    },

    #' @description
    #' Standardizes a fixed argument for the objective function.
    #'
    #' @param center,scale,byrow,ignore,jointly
    #' Passed on to \code{\link[normalize]{normalize}}.

    standardize_argument = function(
      argument_name,
      center = TRUE,
      scale = TRUE,
      byrow = FALSE,
      ignore = integer(),
      jointly = list()
    ) {
      oeli::input_check_response(
        check = oeli::check_missing(argument_name),
        var_name = "argument_name"
      )
      original_argument <- self$fixed_argument("get", argument_name)
      standardized_argument <- normalize::normalize(
        x = original_argument,
        center = center,
        scale = scale,
        byrow = byrow,
        ignore = ignore,
        jointly = jointly,
      )
      arg <- list("modify", standardized_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
      private$.print_status("Standardized argument {.val {argument_name}}.")
      invisible(self)
    },

    #' @description
    #' Prints details of the `Nop` object.
    #'
    #' @param ...
    #' Currently not used.

    print = function(...) {

      ### info on optimization problem
      cli::cli_h2("Optimization problem")
      target <- private$.objective$.__enclos_env__$private$.target
      cli::cli_bullets(c(
        "*" = "Objective: {private$.objective$objective_name}",
        "*" = "Target: {target} (length {self$npar})"
      ))

      ### info on additional arguments
      argument_names <- private$.objective$fixed_arguments
      if (length(argument_names) > 0) {
        modified <- argument_names %in% names(private$.original_arguments)
        names(argument_names) <- ifelse(modified, "!", "*")
        cli::cli_h2("Fixed arguments")
        cli::cli_bullets(argument_names)
      }

      ### info on optimizer functions
      optimizer <- private$.optimizer
      if (length(optimizer) > 0) {
        cli::cli_h2("Optimizer functions")
        cli::cli_ol(names(optimizer))
      }

      ### info on initial values
      initial_values <- private$.initial_values
      if (length(initial_values) > 0) {
        cli::cli_h2("Initial values")
        initial_types <- table(private$.initial_type)
        cli::cli_bullets(
          structure(
            paste0(initial_types, "x ", names(initial_types)),
            names = rep("*", length(initial_types))
          )
        )
      }

      ### info on optimization results
      results <- self$results
      if (nrow(results) > 0) {
        cli::cli_h2("Optimization results")
        occ <- oeli::occurrence_info(
          x = results |> dplyr::select(
            error, .optimization_label, .optimizer_label, .direction, .original
          ),
          relative = TRUE, named = TRUE
        )
        cli::cli_bullets(
          structure(
            c(paste("Total:", nrow(results)), occ),
            names = rep("*", 6)
          )
        )
      }
      invisible(self)
    },

    #' @description
    #' Evaluates the objective function.
    #'
    #' @param at \[`numeric()`\]\cr
    #' The values for the target argument(s), written in a single vector.
    #'
    #' Must be of length `sum(self$npar)`.
    #'
    #' @param .gradient_as_attribute,.hessian_as_attribute \[`logical(1)`\]\cr
    #' Add gradient and / or Hessian value as attributes?
    #'
    #' If gradient and / or Hessian function is not specified, numerical
    #' approximation is used.

    evaluate = function(
      at = rep(0, sum(self$npar)),
      .gradient_as_attribute = FALSE, .hessian_as_attribute = FALSE
    ) {
      private$.objective$evaluate(
        .at = at,
        .gradient_as_attribute = .gradient_as_attribute,
        .hessian_as_attribute = .hessian_as_attribute,
        .gradient_numeric = !private$.objective$gradient_specified,
        .hessian_numeric = !private$.objective$hessian_specified
      )
    },

    #' @description
    #' Specifies a numerical optimizer.
    #'
    #' @param optimizer \[`Optimizer`\]\cr
    #' An `Optimizer` object, which can be created via
    #' \code{\link[optimizeR]{Optimizer}}.
    #'
    #' @param optimizer_label \[`character(1)`\]\cr
    #' A (unique) label for the optimizer.

    set_optimizer = function(optimizer, optimizer_label = optimizer$label) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(optimizer),
        var_name = "optimizer"
      )
      oeli::input_check_response(
        check = checkmate::check_class(optimizer, "Optimizer"),
        var_name = "optimizer"
      )
      oeli::input_check_response(
        check = checkmate::check_string(optimizer_label),
        var_name = "optimizer_label"
      )
      oeli::input_check_response(
        check = checkmate::check_names(
          optimizer_label, disjunct.from = names(private$.optimizer)
        ),
        var_name = "optimizer_label"
      )

      ### set optimizer
      private$.optimizer[[optimizer_label]] <- optimizer
      private$.print_status("Set optimizer {.val {optimizer_label}}.")
      invisible(self)
    },

    #' @description
    #' Defines fixed initial values for the optimization.
    #'
    #' @param at \[`integer(self$sum(npar))` | `list()`\]\cr
    #' The fixed initial parameter vector.
    #'
    #' It can also be a `list` of such vectors.

    initialize_fixed = function(at) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(at),
        var_name = "at"
      )

      ### set initial values
      if (!checkmate::test_list(at)) at <- list(at)
      self$initialize_custom(at, type = "fixed")

    },

    #' @description
    #' Defines random initial values for the optimization.
    #'
    #' @param sampler \[`function`\]\cr
    #' A `function` without any arguments that returns a `numeric`
    #' vector of length `sum(self$npar)`.

    initialize_random = function(
      runs = 1L, sampler = function() stats::rnorm(sum(self$npar))
    ) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_function(sampler, nargs = 0),
        var_name = "sampler"
      )
      oeli::input_check_response(
        check = checkmate::check_count(runs, positive = TRUE),
        var_name = "runs"
      )

      ### set initial values
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
    #' Defines a grid of initial values for the optimization.
    #'
    #' @param lower,upper \[`numeric(1)` | `numeric(self$sum(npar))`\]\cr
    #' Lower and upper grid bounds for each parameter dimension.
    #'
    #' @param breaks \[`integer(1)` | `integer(self$sum(npar))`\]\cr
    #' The number of breaks for each parameter dimension.
    #'
    #' @param jitter
    #' Add noise to the grid points for a random grid layout?
    #'
    #' @param ...
    #' Optional parameters passed to \code{\link[base]{jitter}}.

    initialize_grid = function(
      lower = 0, upper = 1, breaks = 3, jitter = FALSE, ...
    ) {

      ### input checks
      if (checkmate::test_number(lower)) {
        lower <- rep(lower, sum(self$npar))
      }
      if (checkmate::test_number(upper)) {
        upper <- rep(upper, sum(self$npar))
      }
      if (checkmate::test_number(breaks)) {
        breaks <- rep(breaks, sum(self$npar))
      }
      oeli::input_check_response(
        check = checkmate::check_numeric(
          lower, any.missing = FALSE, len = sum(self$npar)
        ),
        var_name = "lower"
      )
      oeli::input_check_response(
        check = checkmate::check_numeric(
          upper, any.missing = FALSE, len = sum(self$npar)
        ),
        var_name = "upper"
      )
      if (!all(lower <= upper)) {
        cli::cli_abort(
          "Lower bounds must be smaller than upper bounds.", call = NULL
        )
      }
      oeli::input_check_response(
        check = checkmate::check_integerish(
          breaks, any.missing = FALSE, lower = 1, len = sum(self$npar)
        ),
        var_name = "breaks"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(jitter),
        var_name = "jitter"
      )

      ### set initial values
      grid_points <- mapply(
        seq, from = lower, to = upper, len = breaks, SIMPLIFY = FALSE
      )
      at <- lapply(as.list(asplit(expand.grid(grid_points), 1)), as.numeric)
      if (jitter) at <- lapply(at, jitter, ...)
      self$initialize_custom(as.list(at), type = "grid")

    },

    #' @description
    #' Defines custom initial values for the optimization.
    #'
    #' @param at \[`list()`\]\cr
    #' A `list` of initial parameter vectors.
    #'
    #' @param seconds \[`numeric(length(at))`\]\cr
    #' The number of seconds it took to obtain each initial value in `at`,
    #' which is added to the overall optimization time.
    #'
    #' @param type \[`character(1)`\]\cr
    #' The type of the initial values.

    initialize_custom = function(
      at, seconds = rep(0, length(at)), type = "custom"
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(at),
        var_name = "at"
      )
      runs <- length(at)
      oeli::input_check_response(
        check = checkmate::check_numeric(seconds, lower = 0, len = runs),
        var_name = "seconds"
      )
      oeli::input_check_response(
        check = checkmate::check_string(type),
        var_name = "type"
      )
      lapply(at, private$.check_target, verbose = FALSE)

      ### set initial values
      private$.initial_values <- c(private$.initial_values, at)
      private$.initial_type <- c(private$.initial_type, rep(type, runs))
      private$.initial_seconds <- c(private$.initial_seconds, seconds)
      private$.print_status("Added {runs} {type} initial parameter value{?s}.")
      invisible(self)
    },

    #' @description
    #' Defines initial values based on results from previous optimizations.
    #'
    #' @param optimization_label \[`character(1)`\]\cr
    #' Label of optimization runs from which to select.

    initialize_continue = function(optimization_label) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_string(optimization_label),
        var_name = "optimization_label"
      )

      ### extract parameters
      if (nrow(self$results) == 0) {
        private$.print_status("No results available.")
        return(invisible(self))
      }
      results <- self$results |>
        dplyr::filter(
          .optimization_label == optimization_label, !error, !is.na(seconds)
        )

      ### set initial values
      if (nrow(results) == 0) {
        cli::cli_warn(
          "No (succesful) optimizations for label {.val {optimization_label}}.",
          call = NULL
        )
      } else {
        self$initialize_custom(
          at = results |> dplyr::pull(parameter),
          seconds = results |> dplyr::pull(seconds),
          type = "continued"
        )
      }
      invisible(self)
    },

    #' @description
    #' Filters initial values from the defined initial values.
    #'
    #' @param condition \[`character(1)`\]\cr
    #' Defines the condition on which the initial values are filtered, one of:
    #'
    #' - `"gradient_negative` for points where the gradient is negative,
    #' - `"gradient_positive` for points where the gradient is negative,
    #' - `"hessian_negative"` for points where the Hessian is negative definite,
    #' - `"hessian_positive"` for points where the Hessian is positive definite.

    initialize_filter = function(condition) {

      ### input checks
      if (length(private$.initial_values) == 0) {
        cli::cli_warn("No initial values defined by user yet.", call = NULL)
        return(invisible(self))
      }
      condition <- oeli::match_arg(
        condition, c("gradient_negative", "gradient_positive",
                     "hessian_negative", "hessian_positive")
      )

      ### filter for initial values based on condition
      values <- if (startsWith(condition, "gradient")) {
        lapply(
          private$.initial_values, self$evaluate, .gradient_as_attribute = TRUE
        ) |> lapply(attr, "gradient")
      } else if (startsWith(condition, "hessian")) {
        lapply(
          private$.initial_values, self$evaluate, .hessian_as_attribute = TRUE
        ) |> lapply(attr, "hessian") |>
          lapply(function(x) eigen(x, symmetric = TRUE, only.values = TRUE)$values)
      }
      filter <- if (endsWith(condition, "negative")) {
        sapply(values, function(x) all(x < 0))
      } else {
        sapply(values, function(x) all(x > 0))
      }

      ### select initial values
      n <- length(private$.initial_values[filter])
      private$.initial_values <- private$.initial_values[filter]
      private$.initial_seconds <- private$.initial_seconds[filter]
      private$.initial_type <- private$.initial_type[filter]
      private$.print_status(
        "Reduced to {n} initialization{?s} via condition {.val {condition}}."
      )
      invisible(self)

    },

    #' @description
    #' Selects promising initial values from the defined initial values.
    #'
    #' @param proportion \[`numeric(1)`\]\cr
    #' The proportion of selected from the defined initial values.
    #'
    #' @param condition \[`character(1)`\]\cr
    #' Defines the condition on which the initial values are selected, one of:
    #'
    #' - `"value_small"` for points where the function value is smallest,
    #' - `"value_large"` for points where the function value is largest,
    #' - `"gradient_small"` for points where the gradient norm is smallest,
    #' - `"gradient_large"` for points where the gradient norm is largest,
    #' - `"condition_small"` for points where the Hessian condition is smallest,
    #' - `"condition_large"` for points where the Hessian condition is largest.

    initialize_promising = function(proportion, condition) {

      ### input checks
      if (length(private$.initial_values) == 0) {
        cli::cli_warn("No initial values defined by user yet.", call = NULL)
        return(invisible(self))
      }
      oeli::input_check_response(
        check = checkmate::check_number(proportion, lower = 0, upper = 1)
      )
      condition <- oeli::match_arg(
        condition, c("value_small", "value_large", "gradient_small",
                     "gradient_large", "condition_small", "condition_large")
      )

      ### compute ranking of initial values based on condition
      values <- if (startsWith(condition, "value")) {
        vapply(
          private$.initial_values, self$evaluate, FUN.VALUE = numeric(1)
        )
      } else if (startsWith(condition, "gradient")) {
        lapply(
          private$.initial_values, self$evaluate, .gradient_as_attribute = TRUE
        ) |> sapply(function(x) sqrt(sum(attr(x, "gradient")^2)))
      } else if (startsWith(condition, "condition")) {
        lapply(
          private$.initial_values, self$evaluate, .hessian_as_attribute = TRUE
        ) |> sapply(function(x) {
          ev <- eigen(x, symmetric = TRUE, only.values = TRUE)$values
          evf <- abs(ev) |> na.omit()
          if (length(evf) == 0) {
            Inf
          } else {
            max(ev, na.rm = TRUE) / min(ev, na.rm = TRUE)
          }
        })
      }
      decreasing <- condition == endsWith(condition, "large")
      ranking <- order(values, decreasing = decreasing)

      ### select initial values
      n <- ceiling(length(private$.initial_values) * proportion)
      indices <- ranking[seq_len(n)]
      private$.initial_values <- private$.initial_values[indices]
      private$.initial_seconds <- private$.initial_seconds[indices]
      private$.initial_type <- private$.initial_type[indices]
      private$.print_status(
        "Reduced to {n} initialization{?s} via condition {.val {condition}}."
      )
      invisible(self)
    },

    #' @description
    #' Transforms the currently defined initial values.
    #'
    #' @param transformer \[`function()`\]\cr
    #' A `function` that receives and returns a `numeric()` of length
    #' `sum(self$npar)`.

    initialize_transform = function(transformer = function(x) x) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_function(transformer, args = "x", nargs = 1),
        var_name = "transformer"
      )

      ### transform initial values
      runs <- length(private$.initial_values)
      if (runs == 0) {
        private$.print_status("No initial values defined yet.")
      } else {
        private$.initial_values <- lapply(private$.initial_values, transformer)
        private$.print_status("Transformed {runs} initial parameter value{?s}.")
      }
      invisible(self)
    },

    #' @description
    #' Resets the currently defined initial values.

    initialize_reset = function() {
      runs <- length(private$.initial_values)
      if (runs == 0) {
        private$.print_status("No initial values defined yet.")
      } else {
        private$.initial_values <- list()
        private$.initial_type <- numeric()
        private$.initial_seconds <- numeric()
        private$.print_status("Reset {runs} initial parameter value{?s}.")
      }
      invisible(self)
    },

    #' @description
    #' Optimizes the target function.
    #'
    #' @details
    #' Supports:
    #'
    #' - Parallel computation of multiple optimization runs via `{future}`
    #' - Progress messages via `{progressr}`
    #'
    #' @param lower,upper \[`numeric()` | `NULL`\]\cr
    #' Optionally lower and upper parameter bounds.
    #'
    #' Ignored for optimizers that do not support parameter bounds.
    #'
    #' @param optimization_label \[`character(1)`\]\cr
    #' A label for the optimization to distinguish optimization runs.
    #'
    #' Setting a label is useful when using the `$initialize_continue()` method.
    #'
    #' @param reset_initial_afterwards \[`logical(1)`\]\cr
    #' Reset the initial values after the optimization?
    #'
    #' @param hide_warnings \[`logical(1)`\]\cr
    #' Hide any warnings during optimization?
    #'
    #' @param seconds \[`numeric(1)`\]\cr
    #' A time limit in seconds.
    #'
    #' Optimization is interrupted prematurely if `seconds` is exceeded.
    #'
    #' Note the limitations documented in \code{\link[base]{setTimeLimit}}.

    optimize = function(
      optimization_label = self$fresh_label,
      which_optimizer = "all", which_direction = "min",
      lower = NULL, upper = NULL,
      seconds = Inf, hide_warnings = TRUE, reset_initial_afterwards = TRUE
    ) {

      ### input checks
      private$.check_arguments_complete()
      oeli::input_check_response(
        check = checkmate::check_string(optimization_label),
        var_name = "optimization_label"
      )
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE
      )
      if (length(optimizer_ids) == 0) {
        cli::cli_warn("No optimizer selected.", call = NULL)
        return(invisible(self))
      }
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = TRUE
      )
      oeli::input_check_response(
        check = oeli::check_numeric_vector(lower, any.missing = FALSE, null.ok = TRUE),
        var_name = "lower"
      )
      if (!is.null(lower)) {
        if (length(lower) == 1) {
          lower <- rep(lower, self$npar)
        }
        oeli::input_check_response(
          check = oeli::check_numeric_vector(lower, len = self$npar),
          var_name = "lower"
        )
      }
      oeli::input_check_response(
        check = oeli::check_numeric_vector(upper, any.missing = FALSE, null.ok = TRUE),
        var_name = "upper"
      )
      if (!is.null(upper)) {
        if (length(upper) == 1) {
          upper <- rep(upper, self$npar)
        }
        oeli::input_check_response(
          check = oeli::check_numeric_vector(upper, len = self$npar),
          var_name = "upper"
        )
      }
      oeli::input_check_response(
        check = checkmate::check_number(seconds, lower = 0),
        var_name = "seconds"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(hide_warnings),
        var_name = "hide_warnings"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(reset_initial_afterwards),
        var_name = "reset_initial_afterwards"
      )

      ### initial values available?
      if (length(private$.initial_values) == 0) {
        self$initialize_random()
        cli::cli_warn(
          "No initial values defined by user, random initial values are used.",
          call = NULL
        )
      }

      ### build grid of optimization combinations
      combinations <- expand.grid(
        initial = private$.initial_values,
        optimizer_id = optimizer_ids,
        which_direction = which_direction
      )

      ### optimize
      private$.print_status("Start optimization {.val {optimization_label}}.")
      if (requireNamespace("progressr", quietly = TRUE)) {
        progress_step <- progressr::progressor(steps = nrow(combinations))
      } else {
        progress_step <- function() NULL
      }
      results <- future.apply::future_apply(
        combinations, MARGIN = 1, function(x) {
          out <- private$.optimize(
            initial = x$initial,
            optimizer_id = x$optimizer_id,
            direction = x$which_direction,
            lower = lower,
            upper = upper,
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
      if (reset_initial_afterwards) self$initialize_reset()
      invisible(self)
    },

    #' @description
    #' Lists all identified optima.
    #'
    #' The output has an associated \code{\link[ggplot2]{autoplot}} method.
    #'
    #' @param group_by \[`character(1)\]\cr
    #' Selects how the output is grouped. Either:
    #' - `NULL` to not group,
    #' - `"optimization"` to group by optimization label,
    #' - `"optimizer"`` to group by optimizer label.
    #'
    #' @param sort_by_value \[`logical(1)\]\cr
    #' Sort by value? Else, sort by frequency.

    optima = function(
      which_direction = "min", only_original = TRUE,
      group_by = NULL, sort_by_value = FALSE,
      digits = getOption("digits", default = 7)
    ) {

      ### input checks
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = FALSE
      )
      oeli::input_check_response(
        check = checkmate::check_flag(only_original),
        var_name = "only_original"
      )
      group_by <- private$.check_group_by(group_by)
      oeli::input_check_response(
        check = checkmate::check_flag(sort_by_value),
        var_name = "sort_by_value"
      )
      oeli::input_check_response(
        check = checkmate::check_int(digits),
        var_name = "digits"
      )

      ### extract optima
      results <- self$results |> dplyr::filter(.direction == which_direction)
      if (only_original) results <- results |> dplyr::filter(.original == TRUE)
      results <- results |> dplyr::mutate(value = round(value, digits = digits))

      if (is.null(group_by)) {
        results <- results |> dplyr::count(value)
        results <- if (sort_by_value && which_direction == "min") {
          results |> dplyr::arrange(value)
        } else if (sort_by_value && which_direction == "max") {
          results |> dplyr::arrange(dplyr::desc(value))
        } else {
          results |> dplyr::arrange(dplyr::desc(n))
        }
      } else {
        results <- if (group_by == "optimization") {
          results |> dplyr::select(value, .optimization_label) |>
            split(results$.optimization_label)
        } else {
          results |> dplyr::select(value, .optimizer_label) |>
            split(results$.optimizer_label)
        }

        results <- if (sort_by_value && which_direction == "min") {
          lapply(results, function(x) {
            dplyr::count(x, value) |> dplyr::arrange(value)
          })
        } else if (sort_by_value && which_direction == "max") {
          lapply(results, function(x) {
            dplyr::count(x, value) |> dplyr::arrange(dplyr::desc(value))
          })
        } else {
          lapply(results, function(x) {
            dplyr::count(x, value) |> dplyr::arrange(dplyr::desc(n))
          })
        }
      }
      structure(
        results, class = c(
          "Nop_optima", if (!is.null(group_by)) "group_by", class(results)
        )
      )

    },

    #' @description
    #' Compute deviations with respect to a reference parameter.
    #'
    #' The output has an associated \code{\link[ggplot2]{autoplot}} method.
    #'
    #' @param reference \[`numeric()`\]\cr
    #' The reference vector of length `sum(self$npar)`.
    #'
    #' @param which_element \[`character(1)\]\cr
    #' Either
    #' - `"initial"` for deviations with respect to the initial values, or
    #' - `"parameter"` for deviations with respect to the estimated parameters.
    #'
    #' @param parameter_labels \[`character()`\]\cr
    #' Labels for the parameters of length `sum(self$npar)`.

    deviation = function(
      reference = rep(0, sum(self$npar)), which_element = "initial",
      which_direction = "min", which_optimizer = "all", only_original = TRUE,
      parameter_labels = paste0("x", seq_len(sum(self$npar)))
    ) {

      ### input checks
      private$.check_target(at = reference)
      which_element <- oeli::match_arg(which_element, c("initial", "parameter"))
      which_direction <- private$.check_which_direction(which_direction)
      which_optimizer <- private$.check_which_optimizer(which_optimizer, to_id = FALSE)
      oeli::input_check_response(
        check = checkmate::check_flag(only_original),
        var_name = "only_original"
      )
      oeli::input_check_response(
        check = checkmate::check_character(
          parameter_labels, unique = TRUE, any.missing = FALSE, len = sum(self$npar)
        )
      )

      ### compute deviation
      results <- self$results |> dplyr::filter(.direction == which_direction)
      if (only_original) results <- results |> dplyr::filter(.original == TRUE)
      out <- if (nrow(results) == 0) {
        c(lapply(seq_len(sum(self$npar)), function(x) numeric(0)), list(character(0))) |>
          setNames(c(parameter_labels, ".optimization_label")) |>
          dplyr::as_tibble()
      } else {
        results |>
          dplyr::select(.optimization_label, dplyr::all_of(which_element)) |>
          dplyr::mutate(
            diff = lapply(results[[which_element]], function(x) x - reference)
          ) |>
          tidyr::unnest_wider(diff, names_sep = "_") |>
          dplyr::rename_with(~ parameter_labels, dplyr::starts_with("diff_")) |>
          dplyr::select(dplyr::all_of(c(parameter_labels, ".optimization_label")))

      }
      structure(out, class = c("Nop_deviation", class(out)))
    }

  ),

  active = list(

    #' @field initial_values \[`list()`, read-only\]\cr
    #' The currently defined initial values.
    #'
    #' Use the `initialize_*()` methods to add, transform, and reset values.

    initial_values = function(value) {
      if (missing(value)) {
        private$.initial_values
      } else {
        cli::cli_abort(
          paste(
            "Field {.var $initial_values} is read-only.",
            "Use the {.fun initialize_*} methods to add, transform, and reset."
          ),
          call = NULL
        )
      }
    },

    #' @field results \[`tibble`, read-only\]\cr
    #' Optimization results with identifiers:
    #'
    #' - `".optimization_label"` (identifies the optimization run)
    #' - `".optimizer_label"` (identifies the optimizer)
    #' - `".direction"` (identifies the optimization direction)
    #' - `".original"` (identifies results obtained on the original problem)
    #'
    #' The output has an associated \code{\link[ggplot2]{autoplot}} method.

    results = function(value) {
      if (missing(value)) {

        ### add identifiers
        res <- private$.results$get("all")
        add_identifier <- c(
          ".optimization_label", ".optimizer_label", ".direction", ".original"
        )
        if (length(res) == 0) {
          return(
            setNames(
              rep(list(character(0)), length(add_identifier)), add_identifier
            ) |> dplyr::as_tibble()
          )
        }
        res <- private$.results$get("all") |>
          lapply(function(result) {
            identifier <- attributes(result)[add_identifier]
            result <- append(result, identifier)
          })

        ### build tibble
        all_names <- unique(unlist(lapply(res, names), use.names = FALSE))
        results <- lapply(res, function(x) {
          values <- lapply(all_names, function(nm) {
            if (!is.null(x[[nm]])) {
              val <- x[[nm]]
            } else {
              val <- NA
            }
            if (is.null(val) || length(val) != 1 || is.matrix(val)) list(val) else val
          })
          names(values) <- all_names
          dplyr::as_tibble(values)
        }) |> dplyr::bind_rows()
        structure(results, class = c("Nop_results", class(results)))

      } else {
        cli::cli_abort(
          "Field {.var $results} is read-only.",
          call = NULL
        )
      }
    },

    #' @field minimum \[`list(2)`, read-only\]\cr
    #' Best value and parameter across all (original) minimizations.

    minimum = function(value) {
      if (missing(value)) {
        self$results |>
          dplyr::filter(.direction == "min", .original == TRUE) |>
          {\(results) if (nrow(results) == 0) {
            private$.print_status("No results available.")
            return(list(value = NA_real_, parameter = numeric()))
          } else {
            results |>
              dplyr::slice_min(order_by = value, n = 1) |>
              dplyr::select(value, parameter) |>
              (\(df) list(value = df$value, parameter = df$parameter[[1]]))()
          }}()
      } else {
        cli::cli_abort(
          "Field {.var $minimum} is read-only.",
          call = NULL
        )
      }
    },

    #' @field maximum \[`list(2)`, read-only\]\cr
    #' Best value and parameter across all (original) maximizations.

    maximum = function(value) {
      if (missing(value)) {
        self$results |>
          dplyr::filter(.direction == "max", .original == TRUE) |>
          {\(results) if (nrow(results) == 0) {
            private$.print_status("No results available.")
            return(list(value = NA_real_, parameter = numeric()))
          } else {
            results |>
              dplyr::slice_max(order_by = value, n = 1) |>
              dplyr::select(value, parameter) |>
              (\(df) list(value = df$value, parameter = df$parameter[[1]]))()
          }}()
      } else {
        cli::cli_abort(
          "Field {.var $maximum} is read-only.",
          call = NULL
        )
      }
    },

    #' @field npar \[`integer()`, read-only\]\cr
    #' The length of each target argument.

    npar = function(value) {
      if (missing(value)) {
        private$.objective$npar
      } else {
        cli::cli_abort(
          "Field {.var $npar} is read-only.",
          call = NULL
        )
      }
    },

    #' @field verbose \[`logical(1)`\]\cr
    #' Print progress and details?

    verbose = function(value) {
      if (missing(value)) {
        private$.verbose
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(value),
          var_name = "verbose"
        )
        private$.verbose <- value
      }
    },

    #' @field fresh_label \[`character(1)`, read-only\]\cr
    #' An optimization label that has not been used yet.

    fresh_label = function(value) {
      if (missing(value)) {
        default_label <- "unlabeled"
        n <- 1
        while (TRUE) {
          label <- paste0(default_label, "_", n)
          if (!label %in% private$.optimization_labels) {
            return(as.character(label))
          } else {
            n <- n + 1
          }
        }
      } else {
        cli::cli_abort(
          "Field {.var $fresh_label} is read-only.",
          call = NULL
        )
      }
    }

  ),

  private = list(

    .verbose = getOption("verbose", default = FALSE),
    .objective = NULL,
    .original_arguments = list(),
    .optimizer = list(),
    .initial_values = list(),
    .initial_type = character(),
    .initial_seconds = numeric(),
    .optimization_labels = character(),
    .results = NULL,

    .print_status = function(msg, verbose = self$verbose) {
      checkmate::assert_string(msg)
      checkmate::assert_flag(verbose)
      if (verbose) {
        cli::cli_alert_info(msg, .envir = parent.frame())
      }
    },

    .check_target = function(at, verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_target(
        .at = at, .verbose = verbose
      )
    },

    .check_arguments_complete = function(verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_arguments_complete(
        .verbose = verbose
      )
    },

    .check_group_by = function(group_by, verbose = self$verbose) {
      oeli::input_check_response(
        check = checkmate::check_choice(
          group_by, choices = c("optimization", "optimizer"),
          null.ok = TRUE
        ),
        var_name = "group_by"
      )
      if (!is.null(group_by)) {
        private$.print_status(
          "Selected grouping by {.val {group_by}}.", verbose = verbose
        )
      }
      return(group_by)
    },

    .check_which_optimizer = function(
      which_optimizer, to_id, verbose = self$verbose
    ) {
      checkmate::assert_flag(to_id)
      checkmate::assert_flag(verbose)
      optimizer_ids <- seq_along(private$.optimizer)
      if (length(optimizer_ids) == 0) {
        cli::cli_abort(paste(
          "No optimizer specified yet.",
          "Please use {.fun $set_optimizer} to specify an optimizer."
        ), call = NULL)
        return(integer(0))
      }
      if (checkmate::test_character(which_optimizer)) {
        if ("all" %in% which_optimizer) {
          if (length(unique(which_optimizer)) > 1) {
            cli::cli_abort(
              "Filter {.var which_optimizer = {.val all}} cannot be paired with
              other optimizer filters.",
              call = NULL
            )
          }
          private$.print_status(
            "Selected all specified optimizers.", verbose = verbose
          )
          if (to_id) {
            return(seq_along(private$.optimizer))
          } else {
            return("all")
          }
        } else {
          optimizer_ids <- which(names(private$.optimizer) %in% which_optimizer)
          optimizer_labels <- names(private$.optimizer)[optimizer_ids]
          if (length(optimizer_ids) > 0) {
            private$.print_status(
              "Selected optimizer {optimizer_labels}.", verbose = verbose
            )
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
        if (length(optimizer_ids) > 0) {
          private$.print_status(
            "Selected optimizer {optimizer_labels}.", verbose = verbose
          )
        }
        if (to_id) {
          return(optimizer_ids)
        } else {
          return(optimizer_labels)
        }
      } else {
        cli::cli_abort(
          "Argument {.var which_optimizer} is misspecified.",
          call = NULL
        )
      }
    },

    .check_which_direction = function(
      which_direction, both_allowed = FALSE, verbose = self$verbose
    ) {
      checkmate::assert_flag(both_allowed)
      checkmate::assert_flag(verbose)
      which_direction <- oeli::match_arg(
        which_direction, choices = c("min", "max"), several.ok = both_allowed
      )
      if (identical(which_direction, "min")) {
        private$.print_status("Selected minimization.", verbose = verbose)
        return("min")
      } else if (identical(which_direction, "max")) {
        private$.print_status("Selected maximization.", verbose = verbose)
        return("max")
      } else {
        private$.print_status(
          "Selected minimization and maximization.", verbose = verbose
        )
        return(c("min", "max"))
      }
    },

    .optimize = function(
      initial, optimizer_id, direction, lower, upper, seconds, hide_warnings
    ) {
      optimizer <- private$.optimizer[[optimizer_id]]
      optimizer$seconds <- seconds
      optimizer$hide_warnings <- hide_warnings
      result <- optimizer$.__enclos_env__$private$.optimize(
        objective = private$.objective,
        initial = initial,
        lower = if (!is.null(lower)) lower else NA,
        upper = if (!is.null(upper)) upper else NA,
        additional_arguments = list(),
        direction = direction
      )
      structure(
        result,
        ".optimizer_id" = optimizer_id,
        ".direction" = direction
      )
    },

    .save_results = function(
      results, optimization_label, verbose = self$verbose
    ) {
      private$.print_status(
        msg = "Saving {length(results)} optimization result{?s}.",
        verbose = verbose
      )
      is_original <- length(private$.original_arguments) == 0
      for (i in seq_along(results)) {
        result <- results[[i]]
        parts <- names(result)

        ### add optimization values
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

          ### additional seconds for initialization
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

        ### add identifiers as attributes
        attr(result, ".optimization_label") <- optimization_label
        optimizer_id <- attr(result, ".optimizer_id")
        optimizer_label <- names(private$.optimizer)[optimizer_id]
        attr(result, ".optimizer_label") <- optimizer_label
        direction <- attr(result, ".direction")
        attr(result, ".original") <- is_original
        direction_identifier <- if (direction == "max") {
          c("direction:max", "!direction:min")
        } else {
          c("direction:min", "!direction:max")
        }

        ### save in Storage object
        identifier <- c(
          paste0("optimization_label:", optimization_label),
          paste0("optimizer_id:", optimizer_id),
          paste0("optimizer_label:", optimizer_label),
          paste0(ifelse(is_original, "", "!"), "original"),
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
    }

  )
)
