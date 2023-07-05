#' Runs Object (R6 Class)
#'
#' @description
#' A \code{Runs} object stores results of numerical optimization runs. It is
#' typically contained inside a \code{\link{Nop}} object.
#'
#' @details
#' A \code{Runs} object can be created via \code{Runs$new()}.
#'
#' @param which_optimizer
#' Select specified numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels,
#' - a \code{numeric} (vector) of optimizer ids.
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
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' The default is \code{2}.
#'
#' @return
#' A \code{Runs} object, which is an R6 class that stores results of numerical
#' optimization runs, see the details.

Runs <- R6::R6Class(
  classname = "Runs",
  public = list(

    #'
    prepare_results = function(
      results = list(), optimizer_label = character(), simplify = logical()
    ) {

    },

    save_results = function(
      results = list(), optimizer_label = character(),
      optimization_label = character(), comparable = logical()
    ) {

    },

    #' @description
    #' Saves a new optimization result.
    #' @param result
    #' A \code{list} with outputs of a numerical optimization, typically
    #' containing
    #' - \code{"value"}, the function value at the found optimum,
    #' - \code{"parameter"}, the parameter at which the optimum value is obtained,
    #' - \code{"seconds"}, the optimization time in seconds,
    #' - \code{"initial"}, the initial parameter,
    #' - \code{"error"}, indicating whether an error occurred,
    #' - \code{"error_message"}, the error message (if any),
    #' and other elements that are optimizer-specific.
    #' @param optimizer_label
    #' A \code{character}, a label for the optimizer.
    #' @param optimization_label
    #' A \code{character}, a label for the optimization.
    #' @param comparable
    #' Either \code{TRUE} if the optimization result was obtained
    #' for the original optimization problem without any transformations,
    #' or \code{FALSE} else.
    #' @return
    #' Invisibly the \code{Results} object.
    new_result = function(
      result, optimizer_label, optimization_label, comparable
    ) {
      if (missing(result)) {
        ino_stop("Please specify {.var result}.")
      }
      if (!is.list(result)) {
        ino_stop("Argument {.var result} is not a {.cls list}.")
      }
      if (missing(optimizer_label)) {
        ino_stop("Please specify {.var optimizer_label}.")
      }
      is_name(optimizer_label)
      if (missing(optimization_label)) {
        ino_stop("Please specify {.var optimization_label}.")
      }
      is_name(optimization_label)
      if (missing(comparable)) {
        ino_stop("Please specify {.var comparable}.")
      }
      is_TRUE_FALSE(comparable)
      result_names <- names(result)
      if ("value" %in% result_names) {
        is_number(result[["value"]])
      } else {
        result[["value"]] <- NA_real_
      }
      if ("parameter" %in% result_names) {
        is_number_vector(result[["parameter"]])
      } else {
        result[["parameter"]] <- NA_real_
      }
      if ("seconds" %in% result_names) {
        is_time(result[["seconds"]])
      } else {
        result[["seconds"]] <- NA_real_
      }
      if ("initial" %in% result_names) {
        is_number_vector(result[["initial"]])
      } else {
        result[["initial"]] <- NA_real_
      }
      if ("error" %in% result_names) {
        is_TRUE_FALSE(result[["error"]])
      } else {
        result[["error"]] <- FALSE
      }
      if ("error_message" %in% result_names) {
        is_name(result[["error_message"]])
      } else {
        result[["error_message"]] <- ""
      }
      result[["optimization_label"]] <- optimization_label
      result[["optimizer_label"]] <- optimizer_label
      result[["comparable"]] <- comparable
      result_id <- self$number_results() + 1
      private$.results[[result_id]] <- result
      private$.add_optimization_label_reference(
        optimization_label = optimization_label, result_id = result_id
      )
      private$.add_optimizer_label_reference(
        optimizer_label = optimizer_label, result_id = result_id
      )
      private$.add_optimization_label(optimization_label)
      private$.add_failed_runs_reference(
        error = result[["error"]], result_id = result_id
      )
      private$.add_comparable_runs_reference(
        comparable = result[["comparable"]], result_id = result_id
      )
    },

    #' @description
    #' Prints details of the stored numerical optimization results.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Results} object.
    #' @importFrom crayon underline
    #' @importFrom cli style_italic
    #' @importFrom glue glue
    print = function(digits = getOption("ino_digits", default = 2), ...) {
      cat(crayon::underline("Optimization runs:\n"))
      if (self$number_results() == 0) {
        cat(cli::style_italic("No results saved yet.\n"))
      } else {
        cat(glue::glue(
          "- Total runs: {self$number_results()}",
          "- Comparable runs: {self$number_results(only_comparable = TRUE)}",
          "- Failed runs: {self$number_runs(which_run = 'failed')}",
          .sep = "\n"
        ), "\n")
      }
    },

    #' @description
    #' TODO
    summary = function() {

    },

    #' @description
    #' TODO
    plot = function() {

    },

    #' @description
    #' Deletes optimization results.
    #' @return
    #' Invisibly the \code{Runs} object.
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
    #' Returns names of available elements per optimizer.
    #' @return
    #' A \code{list}.
    elements_available = function(which_optimizer = "all") {
      optimizer_ids <- private$.get_optimizer_ids(
        which_optimizer = which_optimizer
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
    #' Returns the number of saved optimization results
    #' @return
    #' An \code{integer}.
    number_results = function(
      which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {



      results <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_element = "all", only_comparable = only_comparable,
        simplify = FALSE
      )
      sum(sapply(results, length) > 0)
    }

  ),

  active = list(

    #' @field optimization_labels All unique optimization labels.
    optimization_labels = function(value) {
      if (missing(value)) {
        private$.optimization_labels
      } else {
        ino_stop("{.var $optimization_labels} is read only.")
      }
    }

  ),

  private = list(

    .results = list(),

    .comparable_runs_references = integer(),
    .add_comparable_runs_reference = function(comparable, result_id) {
      if (comparable) private$.comparable_runs_references <- c(
        private$.comparable_runs_references, result_id
      )
    },

    .failed_runs_references = integer(),
    .add_failed_runs_reference = function(error, result_id) {
      if (error) private$.failed_runs_references <- c(
        private$.failed_runs_references, result_id
      )
    },

    .optimization_labels = character(),
    .add_optimization_label = function(optimization_label) {
      private$.optimization_labels <- unique(
        c(private$.optimization_labels, optimization_label)
      )
    },

    .optimization_label_references = list(),
    .add_optimization_label_reference = function(optimization_label, result_id) {
      if (is.null(private$.optimization_label_references[[optimization_label]])) {
        private$.optimization_label_references[[optimization_label]] <- result_id
      } else {
        private$.optimizer_label_references[[optimization_label]] <- c(
          private$.optimizer_label_references[[optimization_label]], result_id
        )
      }
    },

    .optimizer_label_references = list(),
    .add_optimizer_label_reference = function(optimizer_label, result_id) {
      if (is.null(private$.optimizer_label_references[[optimizer_label]])) {
        private$.optimizer_label_references[[optimizer_label]] <- result_id
      } else {
        private$.optimizer_label_references[[optimizer_label]] <- c(
          private$.optimizer_label_references[[optimizer_label]], result_id
        )
      }
    }

    # TODO

    # ### get ids of optimization runs
    # .get_run_ids = function(which_run) {
    #   if (length(private$.results) == 0) {
    #     ino_warn(
    #       "No optimization results saved yet.",
    #       "Please call {.var $optimize(save_results = TRUE)}."
    #     )
    #     return(integer(0))
    #   }
    #   if (is.character(which_run)) {
    #     if (identical(which_run, "all")) {
    #       ids <- seq_along(private$.results)
    #     } else if (identical(which_run, "last")) {
    #       # TODO
    #     } else {
    #       ids <- which(sapply(lapply(private$.results, `[[`, 1), `[[`, "label") %in% which_run)
    #     }
    #   } else if (is.numeric(which_run)) {
    #     ids <- which(seq_along(private$.results) %in% which_run)
    #   } else {
    #     ids <- integer(0)
    #   }
    #   if (length(ids) == 0) {
    #     ino_warn(
    #       "Please check argument {.var which_run}.",
    #       "Your input selects no saved result."
    #     )
    #     return(integer(0))
    #   }
    #   return(ids)
    # },


  )
)
