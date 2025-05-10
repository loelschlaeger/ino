#' Nop Object
#'
#' @description
#' A `Nop` object defines a numerical optimization problem.
#'
#' @param f \[`function`\]\cr
#' A \code{function} to be optimized (the so-called objective function).
#'
#' It is expected that \code{f} has at least one \code{numeric} argument.
#'
#' Further, it is expected that the return value of \code{f} is of the
#' structure \code{numeric(1)}, i.e. a single \code{numeric} value.
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) that get optimized (the so-called target arguments).
#'
#' All target arguments must be \code{numeric}.
#'
#' Can be \code{NULL} (default), then the first function argument is selected.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target argument, i.e., the length(s) of the
#' \code{numeric} \code{vector} argument(s) specified by \code{target}.
#'
#' @param runs \[`integer(1)`\]\cr
#' The number of optimization runs.
#'
#' @param which_direction \[`character()`\]\cr
#' Selects the direction of optimization. One or both of:
#'
#' - \code{"min"} for minimization,
#' - \code{"max"} for maximization.
#'
#' @param which_optimizer \[`character()` | `integer()`\]\cr
#' Selects numerical optimizers. Either:
#'
#' - `"all"` for all specified optimizers,
#' - specific optimizer labels,
#' - specified optimizer ids as defined in the \code{print()} output.
#'
#' @details
#' # Getting started
#'
#' ## Step 1: Create a \code{Nop} object
#' Call \code{object <- Nop$new(f, target, npar, ...)} where
#' - \code{f} is the objective function,
#' - \code{target} are the names of the target arguments,
#' - \code{npar} specifies the lengths of the target arguments,
#' - and \code{...} are additional arguments for \code{f}.
#'
#' You can try to evaluate the objective function via the `$evaluate()` method.
#'
#' ## Step 2: Specify numerical optimizers
#' Call \code{object$set_optimizer(<optimizer object>)}, where
#' \code{<optimizer object>} is an object of class \code{optimizer}. Such
#' objects can be created via the \code{{optimizeR}} package, please refer to
#' [the package homepage](https://loelschlaeger.de/optimizeR/) for details.
#'
#' For example,
#' - \code{optimizeR::Optimizer$new(which = "stats::nlm")} defines the
#'   \code{\link[stats]{nlm}} optimizer,
#' - \code{optimizeR::Optimizer$new(which = "stats::optim")} defines the
#'   \code{\link[stats]{optim}} optimizer.
#'
#' ## Step 3: Select initial values
#' Call initialization methods to define starting values for the
#' optimization (the different initialization strategies are illustrated in the
#' package vignettes), for example:
#' - \code{object$initialize_fixed()} for fixed initial values,
#' - \code{object$initialize_random()} for random initial values,
#' - \code{object$initialize_continue()} for initial values based on parameter
#'   estimates from previous optimization runs.
#'
#' ## Step 4: Optimization
#' Call \code{object$optimize()} for the optimization.
#'
#' ## Step 5: Analyze the results
#' - `$results` returns a `tibble` of all optimization results,
#' - `$minimum` and `$maximum` return the best minimizer and maximizer,
#' - `$optima()` lists the identified optima
#' - `$deviation()` provides parameter deviations to a specified reference.
#'
#' # Progress bar during optimization
#' Displaying progress during multiple optimization runs via the
#' \code{{progressr}} package is supported. To get started, run
#' \preformatted{
#' progressr::handlers(global = TRUE)
#' }
#' and see \code{\link[progressr]{handlers}} for details.
#'
#' # Parallel optimization
#' Parallel computation of multiple optimization runs via the \code{{future}}
#' package is supported. To get started, run one of
#' \preformatted{
#' future::plan(future::sequential)
#' future::plan(future::multisession)
#' future::plan(future::multicore)
#' }
#' and see \code{\link[future]{plan}} for details.
#'
#' @examples
#' Nop_ackley <- Nop$new(f = TestFunctions::TF_ackley, npar = 2)$
#'   set_optimizer(optimizeR::Optimizer$new(which = "stats::nlm"))$
#'   initialize_random(
#'     sampler = function() rnorm(2, mean = 0, sd = 3), runs = 100
#'   )$
#'   optimize(which_direction = "min")
#'
#' Nop_ackley$optima()
#' Nop_ackley$minimum
#'
#' @export

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Creates a new `Nop` object.
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
      # TODO

      ### info on optimizer functions
      cli::cli_h2("Optimizer functions")
      optimizer <- private$.optimizer
      if (length(optimizer) == 0) {
        cat(cli::style_italic("No optimizer specified.\n\n"))
      } else {
        cli::cli_ol(names(optimizer))
      }

      ### info on initial values
      cli::cli_h2("Initial values")
      initial_values <- private$.initial_values
      if (length(initial_values) == 0) {
        cat(cli::style_italic("No initial values specified.\n\n"))
      } else {
        initial_types <- table(private$.initial_type)
        cli::cli_bullets(
          structure(
            paste0(initial_types, "x ", names(initial_types)),
            names = rep("*", length(initial_types))
          )
        )
      }

      ### info on optimization results
      # TODO: number runs, proportion comparable, different optimizers, etc.

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
    #' Add gradient / Hessian value as attributes? Only if specified.

    evaluate = function(
      at = stats::rnorm(sum(self$npar)),
      .gradient_as_attribute = FALSE, .hessian_as_attribute = FALSE
    ) {
      private$.objective$evaluate(
        .at = at,
        .gradient_as_attribute = .gradient_as_attribute,
        .hessian_as_attribute = .hessian_as_attribute
      )
    },

    #' @description
    #' Specifies a numerical optimizer.
    #'
    #' @param optimizer \[`Optimizer`\]\cr
    #' An \code{Optimizer} object, which can be created via
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
    #' A `function` without any arguments that returns a \code{numeric}
    #' vector of length \code{sum(self$npar)}.

    initialize_random = function(
      sampler = function() stats::rnorm(sum(self$npar)), runs = 1L
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
      grid_points <- mapply(seq, from = lower, to = upper, len = breaks, SIMPLIFY = FALSE)
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
      lapply(at, private$.check_target)

      ### set initial values
      private$.initial_values <- c(private$.initial_values, at)
      private$.initial_type <- c(private$.initial_type, rep(type, runs))
      private$.initial_seconds <- c(private$.initial_seconds, seconds)
      private$.print_status("Added {runs} {type} initial parameter value{?s}.")
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
    #' @param reset_initial_afterwards \[`logical(1)`\]\cr
    #' Reset the initial values after the optimization?
    #'
    #' @param hide_warnings \[`logical(1)`\]\cr
    #' Hide any warning messages during optimization?
    #'
    #' @param seconds \[`numeric(1)`\]\cr
    #' A time limit in seconds.
    #'
    #' Optimization is interrupted prematurely if \code{seconds} is exceeded.
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
        cli::cli_warn(paste(
          "No initial values defined by user, random initial values are used.",
          "Call {.fun $initialize_*} first to customize the initialization."
        ), call = NULL)
      }

      ### build grid of optimization combinations
      combinations <- expand.grid(
        initial = private$.initial_values,
        optimizer_id = optimizer_ids,
        which_direction = which_direction
      )

      ### optimize
      private$.print_status("Start optimization {.val {optimization_label}}.")
      progress_step <- progressr::progressor(steps = nrow(combinations))
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

    # TODO: autoplot method

    optima = function() {
      # TODO
    },

    # TODO: autoplot method

    deviation = function() {
      # TODO
    }

  ),
  active = list(

    #' @field results \[`tibble`, read-only\]\cr
    #' Optimization results with identifiers:
    #'
    #' - `".optimization_label"` (identifies the optimization run)
    #' - `".optimizer_label"` (identifies the optimizer)
    #' - `".direction"` (identifies the optimization direction)
    #' - `".original"` (identifies results obtained on the original problem)

    # TODO: autoplot method

    results = function(value) {
      if (missing(value)) {

        ### add identifiers
        res <- private$.results$get("all")
        add_identifier <- c(
          ".optimization_label", ".optimizer_label", ".direction", ".original"
        )
        if (length(res) == 0) {
          return(
            add_identifier |>
              purrr::map_dfc(setNames, object = list(character()))
          )
        }
        res <- private$.results$get("all") |>
          lapply(function(result) {
            identifier <- attributes(result)[add_identifier]
            result <- append(result, identifier)
          })

        ### build tibble
        all_names <- unique(purrr::flatten_chr(purrr::map(res, names)))
        purrr::map(res, function(x) {
          purrr::map(all_names, function(nm) {
            val <- x[[nm]] %||% NA
            if (is.null(val) || length(val) != 1 || is.matrix(val)) list(val) else val
          }) |>
            rlang::set_names(all_names) |> tibble::as_tibble()
        }) |> dplyr::bind_rows()

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
          filter(.direction == "min", .original == TRUE) |>
          {\(results) if (nrow(results) == 0) {
            cli::cli_warn("No results available.", call = NULL)
            return(list(value = NA_real_, parameter = numeric()))
          } else {
            results |>
              slice_min(order_by = value, n = 1) |>
              select(value, parameter) |>
              (\(df) list(value = df$value, parameter = df$parameter[[1]]))()
          }}()
      } else {
        cli::cli_abort(
          "Field {.var $maximum} is read-only.",
          call = NULL
        )
      }
    },

    #' @field maximum \[`list(2)`, read-only\]\cr
    #' Best value and parameter across all (original) maximizations.

    maximum = function(value) {
      if (missing(value)) {
        self$results |>
          filter(.direction == "max", .original == TRUE) |>
          {\(results) if (nrow(results) == 0) {
            cli::cli_warn("No results available.", call = NULL)
            return(list(value = NA_real_, parameter = numeric()))
          } else {
            results |>
              slice_max(order_by = value, n = 1) |>
              select(value, parameter) |>
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
          label <- glue::glue("{default_label}_{n}")
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

    ### global options
    .verbose = getOption("verbose", default = FALSE),

    ### optimization problem
    .objective = NULL,
    .optimizer = list(),

    ### initial values
    .initial_values = list(),
    .initial_type = character(),
    .initial_seconds = numeric(),

    ### optimization results
    .optimization_labels = character(),
    .results = NULL,

    ### print status
    .print_status = function(msg, verbose = self$verbose) {
      checkmate::assert_string(msg)
      checkmate::assert_flag(verbose)
      if (verbose) {
        cli::cli_alert_info(msg, .envir = parent.frame())
      }
    },

    ### checks
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

    ### optimization
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

    ### save optimization results
    .save_results = function(
      results, optimization_label, verbose = self$verbose
    ) {
      private$.print_status(
        msg = "Saving {length(results)} optimization result{?s}.",
        verbose = verbose
      )
      original <- length(private$.original_arguments) == 0
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
        attr(result, ".original") <- original
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
          paste0(ifelse(original, "", "!"), "original"),
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
