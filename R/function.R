Function <- R6::R6Class(

  classname = "Function",

  public = list(

    initialize = function(f, target, npar) {

    },

    set_argument = function() {

    },

    get_argument = function() {

    },

    remove_argument = function() {

    },

    modify_argument = function() {

    },

    reset_argument = function() {

    },

    activate_argument = function() {

    },

    evaluate = function() {

    }

  ),

  active = list(

    info_arguments = function() {

      ### which arguments are required?
      ### which arguments are currently activated?

    }

  ),

  private = list(

    .f = NULL,
    .target = NULL,
    .arguments = list(),
    .arguments_original = list(),

    .check_target = function() {
      if (is.null(arg_name)) {
        arg_name <- deparse(substitute(target_arg))
      } else {
        checkmate::assert_string(arg_name)
      }
      if (!is.vector(target_arg) || !is.numeric(target_arg)) {
        cli::cli_abort(
          "Input {.var {arg_name}} must be a {.cls numeric}.",
          call = NULL
        )
      }
      if (length(target_arg) != self$npar) {
        cli::cli_abort(
          "Input {.var {arg_name}} must be of length {self$npar}.",
          call = NULL
        )
      }
      TRUE
    },

    .check_argument_specified = function(label) {
      checkmate::assert_string(label)
      if (!name %in% names(private$.arguments)) {
        cli::cli_abort(
          "Function argument {.var {name}} is not specified,
          Call {.var $set_argument({.val {name}} = ...)} first.",
          call = NULL
        )
      }
      TRUE
    },

    .check_arguments_complete = function() {
      args_all <- formals(private$.f)
      args_all[private$.f_target] <- NULL
      for (arg in names(args_all)) {
        if (!all(nzchar(args_all[[arg]])) && is.name(args_all[[arg]])) {
          private$.check_additional_argument_exists(arg)
        }
      }
      TRUE
    }

  )

)
