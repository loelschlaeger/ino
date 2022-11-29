#' Nop Object (R6 Class)
#'
#' @description
#' A `Nop` object defines a numerical optimization problem.
#'
#' @details
#' # Initialize
#' TODO
#' ## Step 1
#' TODO
#' ## Step 2
#' TODO
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
    #' TODO
    #' @param npar
    #' TODO
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
      if (!is.null(private$.true_par)) {
        cat(
          glue::glue(" True optimum at: {paste(private$.true_par, collapse = ' ')}"),
          "\n", sep = ""
        )
      }
      if (!is.null(private$.true_val)) {
        cat(
          glue::glue(" True optimum value: {paste(private$.true_val, collapse = ' ')}"),
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
      invisible(self)
    },

    #' @description
    #' Evaluate the function.
    #' @param at
    #' TODO
    evaluate = function(at) {
      if (!(is.numeric(at) && length(at) == private$.npar)) {
        ino_stop(
          glue::glue("Argument 'at' must be a numeric vector of length {private$.npar}.")
        )
      }
      private$.evaluate(at)
    },

    #' @description
    #' Optimize the function.
    #' @param initial
    #' TODO
    #' @param runs
    #' TODO
    #' @param which_optimizer
    #' Can be:
    #' - \code{"all"}
    optimize = function(
      initial = rnorm(self$npar), runs = 1, which_optimizer = "all"
    ) {
      results <- list()
      for (i in seq_len(private$.optimizer_n)) {
        optimizer <- private$.optimizer[[i]]
        results[[i]] <- private$.optimize(initial, optimizer)
      }
      return(results)
    },

    #' @description
    #' Optionally set the true optimum parameter.
    #' @param true_par
    #' Either \code{NULL} (default) or the point where \code{f} obtains its
    #' optimum (i.e., a numeric vector of length \code{npar}).
    #' @param set_true_val
    #' Set to \code{TRUE} to use \code{true_par} to compute the true
    #' optimum value of \code{f} and to \code{FALSE} (default) if not.
    set_true_par = function (true_par = NULL, set_true_val = FALSE) {
      if (!(isTRUE(set_true_val) || isFALSE(set_true_val))) {
        ino_stop(
          "Argument 'set_true_val' must be TRUE or FALSE."
        )
      }
      if (is.null(true_par)) {
        private$.true_par <- NULL
      } else {
        if (!(is.numeric(true_par) && length(true_par) == private$.npar)) {
          ino_stop(
            glue::glue("Argument 'true_par' must be a numeric vector of length {private$.npar}.")
          )
        }
        private$.true_par <- true_par
        if (set_true_val) {
          private$.true_val <- private$.evaluate(at = true_par)
        }
      }
      invisible(self)
    },

    #' @description
    #' Optionally set the true optimum value.
    #' @param true_val
    #' Either \code{NULL} (default) or the value of \code{f} at its optimum.
    set_true_val = function (true_val = NULL) {
      if (is.null(true_val)) {
        private$.true_val <- NULL
      } else {
        if (!(is.numeric(true_val) && length(true_val) == 1)) {
          ino_stop(
            glue::glue("Argument 'true_val' must be a single numeric.")
          )
        }
        private$.true_val <- true_val
      }
      invisible(self)
    },

    #' @description
    #' Set a numerical optimizer.
    #' @param optimizer
    #' An object of class \code{optimizer} which can be created with the
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
          glue::glue("Label {label} already exists.")
        )
      }
      private$.optimizer[[private$.optimizer_n + 1]] <- optimizer
      private$.optimizer_n <- private$.optimizer_n + 1
      private$.optimizer_label <- c(private$.optimizer_label, label)
      invisible(self)
    },

    #' @description
    #' Remove a numerical optimizer.
    #' @param id
    #' An \code{integer} (vector), the id(s) of optimizer to remove.
    remove_optimizer = function (id) {
      # TODO
    },

    #' @description
    #' Test configuration of numerical optimization problem.
    #' @param at
    #' TODO
    #' @param time_limit
    #' TODO
    test = function (at = rnorm(self$npar), time_limit = 3) {

      invisible(self)
    }
  ),
  private = list(

    .f = NULL,
    .f_name = NULL,
    .f_target = NULL,
    .npar = NULL,
    .true_par = NULL,
    .true_val = NULL,

    .optimizer = list(),
    .optimizer_n = 0,
    .optimizer_label = character(0),

    .evaluate = function(at) {
      do.call(
        what = private$.f,
        args = list(at)
      )
    },

    .optimize = function(initial, optimizer_id) {
      do.call(
        what = optimizeR::optimizeR,
        args = list(
          "optimizer" = private$.optimizer[[optimizer_id]],
          "f" = private$.f,
          "p" = initial
        )
      )
    }


  ),
  active = list(

    f = function(value) {
      if (missing(value)) {
        private$.f
      } else {
        ino_stop(
          "`$f` is read only."
        )
      }
    },

    npar = function(value) {
      if (missing(value)) {
        private$.npar
      } else {
        ino_stop(
          "`$npar` is read only."
        )
      }
    },

    true_par = function(value) {
      if (missing(value)) {
        private$.true_par
      } else {
        # TODO
      }
    },

    true_val = function(value) {
      if (missing(value)) {
        private$.true_val
      } else {
        # TODO
      }
    }

  )
)
