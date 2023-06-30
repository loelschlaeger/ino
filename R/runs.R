#' Runs Object (R6 Class)
#'
#' @description
#' A \code{Runs} object stores results of numerical optimization runs. It is
#' typically contained inside a \code{\link{Nop}} object.
#'
#' @details
#' TODO
#'
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

    #' @description
    #' Creates a new \code{Runs} object.
    #' @return
    #' A new \code{Runs} object.
    initialize = function() {

    },

    new_result = function(
      run_id, optimizer_id,
    ) {

    },

    #' @description
    #' Saves a new optimization result.
    #' @param result
    #' TODO
    #' @param optimizer_ids
    #' TODO
    #' @param label
    #' TODO
    #' @param comparable
    #' TODO
    #' @return
    #' Invisibly the \code{Results} object.
    new_result = function(
      result = list(), optimizer_ids = numeric(), label = character(),
      comparable = logical()
    ) {

      # comparable <- length(private$.original_arguments) == 0
      # optimizer <- private$.optimizer
      # optimizer_labels <- names(optimizer)
      # for (i in seq_along(optimizer_ids)) {
      #   run[[i]][["run"]] <- run_id
      #   run[[i]][["optimizer"]] <- optimizer_labels[optimizer_ids[i]]
      #   run[[i]][["comparable"]] <- comparable
      # }
      # full_run <- replicate(length(optimizer), list())
      # for (i in seq_along(optimizer_ids)) {
      #   full_run[[optimizer_ids[i]]] <- run[[i]]
      # }
      # private$.results[[run_id]] <- full_run

    },

    #' @description
    #' Prints details of the stored numerical optimization results.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Results} object.
    print = function(digits = getOption("ino_digits", default = 2), ...) {
      cat(crayon::underline("Optimization results:\n"))
      # suppressWarnings({
      #   results <- x$results()
      #   nruns <- x$number_runs()
      #   nruns_comparable <- x$number_runs(only_comparable = TRUE)
      # })
      # if (nruns == 0) {
      #   cat(cli::style_italic("No results saved yet.\n"))
      # } else if (nruns_comparable == 0) {
      #   cat(glue::glue(
      #     "- Total runs (comparable): {nruns} (0)"
      #   ))
      # } else {
      #   best_parameter <- round(x$best_parameter(), digits = digits)
      #   best_value <- round(x$best_value(), digits = digits)
      #   cat(glue::glue(
      #     "- Total runs (comparable): {nruns} ({nruns_comparable})",
      #     "- Best parameter: {paste(best_parameter, collapse = ' ')}",
      #     "- Best value: {best_value}",
      #     .sep = "\n"
      #   ), "\n")
      # }
    },

    summary = function() {

    },

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

    .ids_last = numeric(),

    .runs_references = list(),
    .optimizer_references = list(),

    .optimization_labels = character(),


    .add_optimization_label = function(new_label) {
      is_name(new_label)
      if (new_label %in% private$.optimization_labels) {
        ino_stop("Label {.val new_label} already exists.")
      }
      private$.optimization_labels <- c(private$.optimization_labels, new_label)
    }

  )
)
