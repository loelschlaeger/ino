#' Fixed initialization
#'
#' @description
#' This function implemented the fixed initialization strategy.
#'
#' @details
#' See the vignette "Initialization strategies" for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param at
#' A list containing the (fixed) initial values.
#'
#' @return NULL
#' @export
#'
#' @examples
#' #fixed_initialization()
#'
#' @keywords
#' strategy

fixed_initialization <- function(x, at) {

  ### check inputs
  if (class(x) != "ino") {
    stop("'x' must be of class 'ino'.")
  }
  if(!is.list(at)){
    stop("'at' must be a list.")
  }
  if(any(sapply(at,length) > x$f$npar)){
    stop("Some element in 'at' has more entries than the function has parameters.")
  }
  if(any(sapply(at,length) < x$f$npar)){
    stop("Some element in 'at' has less entries than the function has parameters.")
  }

  grid_for_optim <- create_grid(x, at)

  # check if multiple argument specifications were provided
  if ("argument_value" %in% colnames(grid_for_optim)) {
    # name of argument for which multiple values are given
    des_argument <- names(which(lapply(x$f$add, length) > 1))
  }

  ### loop over all parameter combinations provided
  for(i in 1:nrow(grid_for_optim)) {
    main_args <- list(f = x$f$f, p = grid_for_optim$at[[i]])
    # set data argument for optimiser, if a data set exists
    if (is.list(x$data)){
      data_arg <- list(data = x$data[[grid_for_optim$data_idx[i]]])
    }
    else {
      data_arg <- c()
    }

    # set further argument specifications (if there exist any)
    if ("argument_value" %in% colnames(grid_for_optim)) {
      x$f$add[[des_argument]] <- grid_for_optim$argument_value[i]
    }

    out <- do.call_timed(
      what = x$optimizer$fun,
      args = c(main_args, data_arg, x$f$add)
    )
    x <- save_optimization_results(x, strategy = "fixed", res = out$res,
                                   time = out$time)
  }

  ### return ino
  return(x)
}

#' Random initialization
#'
#' This function implemented the random initialization strategy.
#'
#' @details
#' See the vignette ... for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param runs
#' The number of random initialization runs.
#' @param sampler
#' If unspecified, draws independent initial values from a standard normal
#' distribution. Else can be a function that returns a (random) numeric vector
#' of length \code{npar}.
#'
#' @return
#' The \code{ino} object.
#'
#' @export
#'
#' @examples
#' #random_initialization()
#'
#' @importFrom stats rnorm
#'
#' @keywords
#' strategy

random_initialization <- function(x, runs = 1, sampler = NULL) {

  ### check inputs
  if (class(x) != "ino") {
    stop("'x' must be of class 'ino'.")
  }
  if (!length(runs) == 1 && is_number(runs)) {
    stop("'runs' must be a numeric value.")
  }
  if (is.null(sampler)) {
    sampler <- function() stats::rnorm(x$f$npar)
  } else {
    try_sampler <- try_silent(sampler())
    if (class(try_sampler) == "fail") {
      stop("'sampler' must be a function that returns a numeric vector.")
    }
    if (!(is.numeric(try_sampler) && length(try_sampler) == x$f$npar)) {
      stop("'sampler' must return a numeric vector of length 'npar'.")
    }
  }

  # create list with one entry corresponding to one set of starting values
  at <- replicate(runs, sampler(), simplify = FALSE)
  # create grid
  grid_for_optim <- create_grid(x, at)

  # check if multiple argument specifications were provided
  if ("argument_value" %in% colnames(grid_for_optim)) {
    # name of argument for which multiple values are given
    des_argument <- names(which(lapply(x$f$add, length) > 1))
  }

  ### loop over all parameter combinations provided
  for(i in 1:nrow(grid_for_optim)){
    main_args <- list(f = x$f$f, p = grid_for_optim$at[[i]])
    # set data argument for optimiser, if a data set exists
    if(is.list(x$data)){
      data_arg <- list(data = x$data[[grid_for_optim$data_idx[i]]])
    }
    else{
      data_arg <- c()
    }

    # set further argument specifications (if there exist any)
    if ("argument_value" %in% colnames(grid_for_optim)) {
      x$f$add[[des_argument]] <- grid_for_optim$argument_value[i]
    }

    out <- do.call_timed(
      what = x$optimizer$fun,
      args = c(main_args, data_arg, x$f$add)
    )

    ### save results in ino
    x <- save_optimization_results(x,
                                   strategy = "random",
                                   res = out$res,
                                   time = out$time)
  }

  ### loop over runs
  # for(run in 1:runs){
  #
  #   ### random initialization
  #   p <- sampler()
  #   main_args <- list(f = x$f$f, p = p)
  #   out <- do.call_timed(
  #     what = x$optimizer$fun,
  #     args = c(main_args, x$f$add)
  #   )
  #
  #   ### save results in ino
  #   x <- save_optimization_results(x,
  #                                  strategy = "random",
  #                                  res = out$res,
  #                                  time = out$time)
  # }

  ### return ino
  return(x)
}

#' Saving of optimization results.
#'
#' @description
#' This function saves optimization results in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param strategy
#' A character, the label for the strategy.
#' @param res
#' A list object, the optimization output.
#' @param time
#' An object of class \code{difftime}, the optimization time.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

save_optimization_results <- function(x, strategy, res, time) {
  new_results <- list("strategy" = strategy, "res" = res, "time" = time)
  if (identical(x[["optimizations"]], NA)) {
    ### first optimization
    x[["optimizations"]] <- list()
    x[["optimizations"]][[1]] <- new_results
  } else {
    ### not first optimization
    x[["optimizations"]] <- c(x[["optimizations"]], list(new_results))
  }
  return(x)
}
