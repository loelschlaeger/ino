#' Records Object
#'
#' @description
#' A \code{Records} object stores results of numerical optimizations runs.
#' This object is contained inside a \code{\link{Nop}} object.
#'
#' @details
#' Results of numerical optimization runs can be provided in three formats:
#' 1. As a \code{list} that contains the results of a single optimization with
#'    a single optimizer. This is indicated via \code{results_depth = 1}.
#'    Such a \code{list} typically contains the elements
#'    - \code{"value"}, the function value at the optimum,
#'    - \code{"parameter"}, the parameters at the optimum,
#'    - \code{"seconds"}, the optimization time in seconds,
#'    - \code{"initial"}, the initial parameter,
#'    - \code{"error"}, indicating whether an error occurred,
#'    - \code{"error_message"}, the error message (if any),
#'    and optionally other elements that are optimizer-specific.
#' 2. As a \code{list} that contains the results of a single optimization run,
#'    where each element is a \code{list} of results from one optimizer.
#'    This is indicated via \code{results_depth = 2}.
#' 3. As a \code{list} that contains the results of multiple optimization runs,
#'    where each element is a \code{list} of a single optimization run.
#'    This is indicated via \code{results_depth = 3}.
#'
#' @param results
#' A \code{list} of optimization results, see the details.
#' @param results_depth
#' Either \code{1}, \code{2}, or \code{3}, indicating the format of the
#' input \code{results}, see the details.
#' @param optimizer_label
#' A \code{character}, the optimizer label(s).
#' @param optimization_label
#' A \code{character}, a label for the optimization.
#' @param comparable
#' Either \code{TRUE} if the results are obtained for the original optimization
#' problem without any transformations, or \code{FALSE} else.
#' @param which_optimizer
#' Select specified numerical optimizers. Either:
#' - \code{"all"} for all specified optimizers,
#' - a \code{character} (vector) of specified optimizer labels.
#' @param which_run
#' Select saved results of optimization runs. Either:
#' - \code{"all"} for all results,
#' - \code{"last"}, the last saved results,
#' - \code{"failed"}, the results from all failed optimization runs,
#' - a \code{character} (vector) of optimization labels,
#' - a \code{numeric} (vector) of run ids.
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
#'   - \code{"error_message"}, the error message (if any),
#'   - \code{"comparable"}, indicating whether the results are obtained for the
#'     original optimization problem without any transformations.
#' - a \code{character} (vector) with names of specific elements (see
#'   \code{$elements_available()} for the names of all available elements).
#' @param only_comparable
#' Either \code{TRUE} to show only comparable results (i.e., results obtained
#' for the original optimization problem without any transformations),
#' or \code{FALSE} to include all optimization results.
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' @param simplify
#' If \code{simplify = TRUE}, the nested list output of optimization results is
#' flattened if possible.
#' @param ...
#' Currently not used.
#'
#' @return
#' A \code{Records} object, which is an R6 class that stores results of
#' numerical optimization runs.

Records <- R6::R6Class(
  classname = "Records",
  public = list(

    #' @description
    #' Creates a new \code{Records} object.
    #' @return
    #' A new \code{Records} object.
    initialize = function() {
      private$.results <- list()
    },

    #' @description
    #' Saves optimization results.
    #' @return
    #' Invisibly the \code{Records} object.
    save = function(
      results, results_depth, optimizer_label, optimization_label, comparable
    ) {
      if (missing(results)) {
        ino_stop("Please specify {.var results}.")
      }
      if (!is.list(results)) {
        ino_stop("Argument {.var results} must be a {.cls list}.")
      }
      if (length(results) == 0) {
        ino_warn("No results to save.")
        return(invisible(self))
      }
      if (!(is_count(results_depth) && results_depth %in% 1:3)) {
        ino_stop("Argument {.var results_depth} must be 1, 2, or 3.")
      }
      if (results_depth == 1) {
        no_optimizer <- 1
        results <- list(list(results))
      }
      if (results_depth == 2) {
        if (!all(sapply(results, is.list))) {
          ino_stop("Argument {.var results} must contain {.cls list} elements.")
        }
        no_optimizer <- length(results)
        results <- list(results)
      }
      if (results_depth == 3) {
        no_optimizer <- unique(sapply(results, length))
        if (length(no_optimizer) > 1) {
          ino_stop("Each {.cls list} of {.var results} must be of same length.")
        }
      }
      if (missing(optimizer_label)) {
        ino_stop("Please specify {.var optimizer_label}.")
      }
      is_name_vector(optimizer_label, allow_na = FALSE)
      if (length(optimizer_label) != no_optimizer) {
        ino_stop(
          glue::glue(
            "Argument {.var optimizer_label} must be of length <no_optimizer>.",
            .open = "<", .close = ">"
          )
        )
      }
      if (missing(optimization_label)) {
        ino_stop("Please specify {.var optimization_label}.")
      }
      is_name(optimization_label, allow_na = FALSE)
      if (missing(comparable)) {
        ino_stop("Please specify {.var comparable}.")
      }
      is_TRUE_FALSE(comparable, allow_na = FALSE)
      private$.last_runs_ids <- integer()
      for (i in seq_along(results)) {
        run <- results[[i]]
        run_id <- length(private$.run_ids) + 1
        if (!is.list(run)) {
          ino_stop("Element {.var results[[{i}]]} must be a {.cls list}.")
        }
        for (j in seq_along(run)) {
          optimization <- run[[j]]
          if (!is.list(optimization)) {
            ino_stop(
              "Element {.var results[[{i}]][[{j}]]} must be a {.cls list}."
            )
          }
          parts <- names(optimization)
          if ("value" %in% parts) {
            is_number(optimization[["value"]], allow_na = TRUE)
          } else {
            optimization[["value"]] <- NA_real_
          }
          if ("parameter" %in% parts) {
            is_number_vector(optimization[["parameter"]], allow_na = TRUE)
          } else {
            optimization[["parameter"]] <- NA_real_
          }
          if ("seconds" %in% parts) {
            is_time(optimization[["seconds"]], allow_na = TRUE)
          } else {
            optimization[["seconds"]] <- NA_real_
          }
          if ("initial" %in% parts) {
            is_number_vector(optimization[["initial"]], allow_na = TRUE)
          } else {
            optimization[["initial"]] <- NA_real_
          }
          if ("error" %in% parts) {
            is_TRUE_FALSE(optimization[["error"]], allow_na = TRUE)
          } else {
            optimization[["error"]] <- FALSE
          }
          if ("error_message" %in% parts) {
            is_name(optimization[["error_message"]], allow_na = TRUE)
          } else {
            optimization[["error_message"]] <- NA_character_
          }
          optimization[["optimization_label"]] <- optimization_label
          optimization[["optimizer_label"]] <- optimizer_label[j]
          optimization[["comparable"]] <- comparable
          attr(optimization, "run") <- run_id
          attr(optimization, "optimizer") <- optimizer_label[j]
          result_id <- suppressWarnings(self$number()) + 1
          private$.results[[result_id]] <- optimization
          private$.set_ids(
            result_id = result_id,
            run_id = run_id,
            optimization_label = optimization_label,
            optimizer_label = optimizer_label[j],
            error = optimization[["error"]],
            comparable = comparable
          )
        }
      }
      return(invisible(self))
    },

    #' @description
    #' Extract optimization results.
    #' @return
    #' A \code{list} of selected optimization results.
    get = function(
      which_run = "all", which_optimizer = "all", which_element = "all",
      only_comparable = FALSE, by_run = TRUE, by_optimizer = TRUE,
      simplify = TRUE
    ) {
      which_run <- private$.check_which_run(which_run)
      which_optimizer <- private$.check_which_optimizer(which_optimizer)
      which_element <- private$.check_which_element(
        which_element = which_element, which_optimizer = which_optimizer
      )
      is_TRUE_FALSE(by_run, allow_na = FALSE)
      is_TRUE_FALSE(by_optimizer, allow_na = FALSE)
      is_TRUE_FALSE(simplify, allow_na = FALSE)

      ids <- private$.get_ids(
        which_run = which_run, which_optimizer = which_optimizer
      )
      if (length(ids) == 0) {
        return(list())
      }
      return(private$.results[ids])

    },

    #' @description
    #' Prints details of the stored numerical optimization results.
    #' @return
    #' Invisibly the \code{Results} object.
    #' @importFrom crayon underline
    #' @importFrom cli style_italic
    #' @importFrom glue glue
    print = function(digits = getOption("digits", default = 7), ...) {
      cat(crayon::underline("Optimization records:\n"))
      if (suppressWarnings(self$number()) == 0) {
        cat(cli::style_italic("No results saved yet.\n"))
      } else {
        suppressWarnings({
          nruns <- length(private$.run_ids)
          noptimizer <- length(self$optimizer_labels)
          noptimizations <- self$number()
          ncomparable <- self$number(only_comparable = TRUE)
          nfailed <- self$number(which_run = 'failed')
        })
        cat(glue::glue(
          "- Number runs: {nruns} with {noptimizer} optimizer(s)",
          "- Number optimizations: {noptimizations}",
          "- Comparable optimizations: {ncomparable}",
          "- Failed optimizations: {nfailed}",
          .sep = "\n"
        ), "\n")
      }
    },

    #' @description
    #' Deletes optimization results.
    #' @return
    #' Invisibly the \code{Records} object.
    clear = function(which_run, which_optimizer, which_element) {
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
    #' Returns names of available elements by optimizer in the results.
    #' @return
    #' A \code{list}.
    elements = function(
      which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {
      which_optimizer <- private$.check_which_optimizer(
        which_optimizer = which_optimizer
      )


      ids <- private$.get_ids(
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      )
      if (length(ids) == 0) return(list())



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
    #' Returns the number of saved optimization results.
    #' @return
    #' An \code{integer}.
    number = function(
      which_run = "all", which_optimizer = "all", only_comparable = FALSE
    ) {
      length(
        private$.get_ids(
          which_run = which_run, which_optimizer = which_optimizer,
          only_comparable = only_comparable
        )
      )
    }

  ),

  active = list(

    #' @field optimization_labels Unique stored optimization labels.
    optimization_labels = function(value) {
      if (missing(value)) {
        names(private$.optimization_label_ids)
      } else {
        ino_stop("{.var $optimization_labels} is read only.")
      }
    },

    #' @field optimizer_labels Unique stored optimizer labels.
    optimizer_labels = function(value) {
      if (missing(value)) {
        names(private$.optimizer_label_ids)
      } else {
        ino_stop("{.var $optimizer_labels} is read only.")
      }
    }

  ),

  private = list(

    ### each element corresponds to a single optimization
    .results = list(),

    ### check selections
    .check_which_run = function(which_run) {

    },
    .check_which_optimizer = function(which_optimizer) {
      is_name_vector(which_optimizer, allow_na = FALSE)
      if (identical(which_optimizer, "all")) {
        which_optimizer <- self$optimizer_labels
      }

      return(which_optimizer)
    },
    .check_which_element = function(which_element, which_optimizer) {
      stopifnot(sapply(optimizer_ids, is_count))
      if (length(protected_elements) > 0) sapply(protected_elements, is_name)
      all_elements <- unique(unlist(
        self$elements(which_optimizer = optimizer_ids)
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

    ### ids for specific optimization runs
    .last_runs_ids = integer(),
    .run_ids = list(),
    .optimization_label_ids = list(),
    .comparable_runs_ids = integer(),
    .failed_runs_ids = integer(),
    .optimizer_label_ids = list(),
    .set_ids = function(
      result_id, run_id, optimization_label, optimizer_label, error, comparable
    ) {
      private$.last_runs_ids <- c(private$.last_runs_ids, result_id)
      if (length(private$.run_ids) < run_id) {
        private$.run_ids[[run_id]] <- result_id
      } else {
        private$.run_ids[[run_id]] <- c(private$.run_ids[[run_id]], result_id)
      }
      if (is.null(private$.optimization_label_ids[[optimization_label]])) {
        private$.optimization_label_ids[[optimization_label]] <- result_id
      } else {
        private$.optimization_label_ids[[optimization_label]] <- c(
          private$.optimization_label_ids[[optimization_label]], result_id
        )
      }
      if (comparable) private$.comparable_runs_ids <- c(
        private$.comparable_runs_ids, result_id
      )
      if (error) private$.failed_runs_ids <- c(
        private$.failed_runs_ids, result_id
      )
      if (is.null(private$.optimizer_label_ids[[optimizer_label]])) {
        private$.optimizer_label_ids[[optimizer_label]] <- result_id
      } else {
        private$.optimizer_label_ids[[optimizer_label]] <- c(
          private$.optimizer_label_ids[[optimizer_label]], result_id
        )
      }
    },
    .get_ids = function(which_run, which_optimizer, only_comparable) {
      if (length(private$.results) == 0) {
        ino_warn("No optimization results saved yet.")
        return(integer())
      }
      is_name_vector(which_optimizer, allow_na = FALSE)
      is_TRUE_FALSE(only_comparable, allow_na = FALSE)
      ids <- if (is_name(which_run, allow_na = FALSE, error = FALSE)) {
        if (identical(which_run, "all")) {
          seq_along(private$.results)
        } else if (identical(which_run, "last")) {
          private$.last_runs_ids
        } else if (identical(which_run, "failed")) {
          private$.failed_runs_ids
        } else if (which_run %in% self$optimization_labels) {
          private$.optimization_label_ids[[which_run]]
        } else {
          integer()
        }
      } else if (is_index_vector(which_run, error = FALSE)) {
        which(seq_along(private$.results) %in% which_run)
      } else {
        ino_stop("Argument {.var which_run} is misspecified.")
      }
      if (!identical(which_optimizer, "all")) {
        ids <- if (which_optimizer %in% names(private$.optimizer_label_ids)) {
          intersect(ids, private$.optimizer_label_ids[[which_optimizer]])
        } else {
          integer()
        }
      }
      if (only_comparable) {
        ids <- intersect(ids, private$.comparable_runs_ids)
      }
      if (length(ids) == 0) {
        ino_warn("Your input selects no saved result.")
        return(integer())
      }
      return(ids)
    }
  )
)
