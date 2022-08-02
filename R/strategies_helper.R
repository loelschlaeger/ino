#' @noRd
#' @keywords
#' internal

ino_check_inputs <- function(...) {
  inputs <- list(...)
  arg <- at <- by_col <- by_row <- center <- how <- prop <- runs <- NULL
  sampler <- x <- NULL
  within(inputs, {
    n <- names(inputs)
    if ("x" %in% n) {
      if (!inherits(x, "ino")) {
        ino_stop("'x' must be of class 'ino'.")
      }
    }
    if ("runs" %in% n) {
      if (!length(runs) == 1 && is_number(runs)) {
        ino_stop("'runs' must be an integer.")
      }
    }
    if ("sampler" %in% n) {
      if (!is.function(sampler)) {
        ino_stop("'sampler' must be a function.")
      }
    }
    if ("at" %in% n) {
      if (!is.numeric(at)) {
        ino_stop("'at' must be a numeric vector.")
      }
    }
    if ("arg" %in% n) {
      if (!is.character(arg)) {
        ino_stop("'arg' must be a character.")
      }
    }
    if (all(c("arg", "x") %in% n)) {
      if (!arg %in% names(x$f$add)) {
        ino_stop(paste0(
          "'arg' = '", arg, "' does not seem to be an argument of '",
          x$f$name, "'."
        ))
      }
      if (arg %in% x$f$mpvs &&
          !all(sapply(x$f$add[[arg]], inherits, c("matrix", "data.frame"))) ||
          !arg %in% x$f$mpvs &&
          !inherits(x$f$add[[arg]], c("matrix", "data.frame"))) {
        ino_stop(paste0(
          "The argument 'arg' = '", arg, "' does not seem to be of class ",
          "'matrix' or 'data.frame'."
        ))
      }
    }
    if ("how" %in% n) {
      if (!how %in% c("random", "first", "kmeans")) {
        ino_stop("'how' must be one of 'random', 'first', or 'kmeans'.")
      }
    }
    if ("prop" %in% n) {
      if (!(is.numeric(prop) && all(prop <= 1) && all(prop >= 0))) {
        ino_stop("(Each element of) 'prop' must be between 0 and 1.")
      }
    }
    if ("by_col" %in% n) {
      if (!(is.logical(by_col) || length(by_col) == 1)) {
        ino_stop("'by_col' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("by_row" %in% n) {
      if (!(is.logical(by_row) || length(by_row) == 1)) {
        ino_stop("'by_row' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("center" %in% n) {
      if (!(is.logical(center) || length(center) == 1)) {
        ino_stop("'center' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("scale" %in% n) {
      if (!(is.logical(scale) || length(scale) == 1)) {
        ino_stop("'scale' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) > x$f$npar) {
        ino_stop("'at' has more entries than the function has parameters.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) < x$f$npar) {
        ino_stop("'at' has less entries than the function has parameters.")
      }
    }
  })

  return(invisible(NULL))
}

#' @noRd
#' @keywords
#' internal

subset_arg <- function(x, arg, how, prop, by_row, col_ign, kmeans_arg) {

  ### check inputs
  ino_check_inputs(
    "x" = x, "arg" = arg, "how" = how, "prop" = prop, "by_row" = by_row,
    "col_ign" = col_ign, "kmeans_arg" = kmeans_arg
  )

  ### function for subsetting
  do_subset_arg <- function(arg_val) {
    if (!by_row) arg_val <- t(arg_val)
    arg_val_length <- nrow(arg_val)
    arg_val_subset_length <- ceiling(arg_val_length * prop)
    if (how == "random") {
      subset_ind <- sort(sample.int(arg_val_length, arg_val_subset_length))
    } else if (how == "first") {
      subset_ind <- 1:arg_val_subset_length
    } else if (how == "kmeans") {
      arg_val_ign <- arg_val
      if (!is.null(col_ign)) {
        arg_val_ign <- arg_val_ign[, -col_ign, drop = FALSE]
      }
      kmeans_out <- do.call(
        what = stats::kmeans,
        args = c(list("x" = arg_val_ign), kmeans_arg)
      )
      nc <- ceiling(arg_val_subset_length / kmeans_arg[["centers"]])
      subset_ind <- c()
      for (i in 1:kmeans_arg[["centers"]]) {
        subset_ind_i <- which(kmeans_out$cluster == i)
        subset_ind <- c(subset_ind, sample(
          x = subset_ind_i,
          size = min(nc, length(subset_ind_i))
        ))
      }
      subset_ind <- sort(subset_ind)
    }
    arg_val_subset <- arg_val[subset_ind, , drop = FALSE]
    if (!by_row) arg_val_subset <- t(arg_val_subset)
    return(arg_val_subset)
  }

  ### perform subsetting
  if (arg %in% x$f$mpvs) {
    x$f$add[[arg]] <- lapply(x$f$add[[arg]], do_subset_arg)
  } else {
    x$f$add[[arg]] <- do_subset_arg(x$f$add[[arg]])
  }

  ### return updated ino object
  return(x)
}

#' @noRd
#' @keywords
#' internal

standardize_arg <- function(x, arg, by_col, center, scale, col_ign) {

  ### check inputs
  ino_check_inputs(
    "x" = x, "arg" = arg, "by_col" = by_col, "center" = center, "scale" = scale,
    "col_ign" = col_ign
  )

  ### function for standardizing
  do_standardize_arg <- function(arg_val) {
    if (!by_col) arg_val <- t(arg_val)
    for (i in 1:ncol(arg_val)) {
      if (i %in% col_ign) next()
      arg_val[, i] <- scale(arg_val[, i], center = center, scale = scale)
    }
    if (!by_col) arg_val <- t(arg_val)
    return(arg_val)
  }

  ### perform subsetting
  if (arg %in% x$f$mpvs) {
    x$f$add[[arg]] <- lapply(x$f$add[[arg]], do_standardize_arg)
  } else {
    x$f$add[[arg]] <- do_standardize_arg(x$f$add[[arg]])
  }

  ### return updated ino object
  return(x)
}

#' Save results of optimization run
#'
#' @description
#' This helper function saves the results of an optimization run into the
#' submitted \code{ino} object.
#'
#' @details
#' The results are saved at \code{x$runs}, which is a list of two elements:
#' \itemize{
#'   \item \code{table} is a \code{data.frame} with the optimization runs as
#'         rows and optimization results as columns.
#'         Per default, the following columns are created:
#'         \itemize{
#'           \item \code{.strategy}, the name of the initialization strategy,
#'           \item \code{.optimizer}, the name of the optimizer,
#'           \item \code{.time}, the optimization time,
#'           \item \code{.optimum}, the function value at the optimum.
#'         }
#'   \item \code{pars} is a \code{list}, where the following values are saved
#'         for each optimization run:
#'         \itemize{
#'           \item \code{.init}, the initial value,
#'           \item \code{.estimate}, the optimal parameter values,
#'           \item and any non-single valued output of the optimizer that was
#'                 specified via \code{crit} in \code{\link{set_optimizer}}.
#'         }
#' }
#'
#' @param x
#' An object of class \code{ino}.
#' @param strategy
#' A character, the name of the initialization strategy.
#' @param pars
#' A list of parameter values for the optimization run.
#' @param result
#' The output of \code{\link{do.call_timed}}.
#' @param opt_name
#' The name of the optimizer.
#'
#' @return
#' The updated object \code{x} (invisibly).
#'
#' @keywords
#' internal

save_result <- function(x, strategy, pars, result, opt_name) {

  ### determine number of new optimization result
  nopt <- nrow(x[["runs"]][["table"]]) + 1

  ### save optimization results
  x[["runs"]][["table"]][nopt, ".strategy"] <- strategy
  x[["runs"]][["table"]][nopt, ".time"] <- result$time
  v <- x$opt[[opt_name]]$base_arg_names[3]
  x[["runs"]][["table"]][nopt, ".optimum"] <- result$res[[v]]
  if (length(x$opt) > 1) {
    x[["runs"]][["table"]][nopt, ".optimizer"] <- opt_name
  }
  for (i in seq_along(attr(pars, "par_name"))) {
    if (attr(pars, "par_name")[i] %in% x$f$mpvs) {
      x[["runs"]][["table"]][nopt, attr(pars, "par_name")[i]] <-
        attr(pars, "par_id")[i]
    }
  }
  x[["runs"]][["pars"]][[nopt]] <- list()
  x[["runs"]][["pars"]][[nopt]][[".init"]] <- pars[[x$f$target_arg]]
  z <- x$opt[[opt_name]]$base_arg_names[4]
  x[["runs"]][["pars"]][[nopt]][[".estimate"]] <- result$res[[z]]
  opt_crit <- x$opt[[opt_name]]$crit
  crit_val <- result$res[opt_crit]
  for (i in seq_along(opt_crit)) {
    if (is.numeric(crit_val[[i]]) && length(crit_val[[i]]) == 1) {
      x[["runs"]][["table"]][nopt, opt_crit[i]] <- crit_val[[i]]
    } else {
      x[["runs"]][["pars"]][[nopt]][[opt_crit[i]]] <- crit_val[[i]]
    }
  }

  ### return (invisibly) updated ino object
  return(invisible(x))
}

#' @noRd
#' @keywords
#' internal

strategy_call <- function(call) {
  call$ncores <- 1
  class(call) <- c("strategy_call", class(call))
  return(call)
}

#' @exportS3Method
#' @noRd
#' @keywords
#' internal

print.strategy_call <- function(x, ...) {
  cat("<strategy call>")
}

#' Check sampler
#'
#' @description
#' This function ...
#'
#' @param sampler
#' The sampler for random initial values. Can be any function that
#' \itemize{
#'   \item has as first argument an integer, say \code{npar},
#'   \item and returns a numeric vector of length \code{npar}.
#' }
#' Per default, \code{sampler = stats::rnorm}, i.e. independent draws from a
#' standard normal distribution as initial value.

check_sampler <- function() {
  npar <- NULL
  sampler_first_arg <- alist(npar)
  names(sampler_first_arg) <- names(formals(sampler))[1]
  sampler_add_args <- list(...)
  sampler_args <- c(sampler_first_arg, sampler_add_args)
  sampler_init <- function(npar) do.call(what = sampler, args = sampler_args)
  try_sampler <- try_silent(sampler_init(x$f$npar))
  if (inherits(try_sampler, "fail")) {
    stop("'sampler' failed with error message:\n", try_sampler, call. = FALSE)
  }
  if (!is.numeric(try_sampler) || length(try_sampler) != x$f$npar) {
    stop("'sampler' must return a numeric vector of length 'x$f$npar'.\n",
         "Instead, 'sampler' returned:\n",
         paste(capture.output(str(try_sampler)), collapse = "\n"),
         call. = FALSE
    )
  }
}
