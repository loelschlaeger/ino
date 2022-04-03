#' Data subset initialization
#'
#' @description
#' This function is an implementation of the data subset initialization
#' strategy. See the vignette on initialization strategies for more details.
#'
#' @details
#' Missing.
#'
#' @param x
#' An object of class \code{ino}.
#' @param how
#' A character, specifying how to select the data subset. Can be one of
#' \code{"random"} (default), \code{"first"}, and \code{"kmeans"}, see the
#' details.
#' @param prop
#' A numeric between 0 and 1, specifying the proportion of the data subset.
#' @param data_arg
#' A character, the name of the data argument.
#' @param by_row
#' A boolean,
#' @return
#' The updated input \code{x}.
#'
#' @export
#'
#' @examples
#' #data_subset_initialization()
#'
#' @keywords
#' strategy

data_subset_initialization <- function(x, how = "random", prop = 0.5,
                                       data_arg = "data", by_row = TRUE) {


}

#' Fixed initialization
#'
#' @description
#' This function is an implementation of the fixed initialization
#' strategy. See the vignette on initialization strategies for more details.
#'
#' @details
#' Missing.
#'
#' @param x
#' An object of class \code{ino}.
#' @param at
#' A list containing the (fixed) initial values.
#'
#' @return
#' The updated input \code{x}.
#'
#' @export
#'
#' @examples
#' #fixed_initialization()
#'
#' @keywords
#' strategy

fixed_initialization <- function(x, at) {

  # ### check inputs
  # if (class(x) != "ino") {
  #   stop("'x' must be of class 'ino'.")
  # }
  # if(!is.list(at)){
  #   stop("'at' must be a list.")
  # }
  # if(any(sapply(at,length) > x$f$npar)){
  #   stop("Some element in 'at' has more entries than the function has parameters.")
  # }
  # if(any(sapply(at,length) < x$f$npar)){
  #   stop("Some element in 'at' has less entries than the function has parameters.")
  # }
  #
  # grid_for_optim <- create_grid(x, at)
  #
  # # check if multiple argument specifications were provided
  # if ("argument_value" %in% colnames(grid_for_optim)) {
  #   # name of argument for which multiple values are given
  #   des_argument <- names(which(lapply(x$f$add, length) > 1))
  # }
  #
  # ### loop over all parameter combinations provided
  # for(i in 1:nrow(grid_for_optim)) {
  #   main_args <- list(f = x$f$f, p = grid_for_optim$at[[i]])
  #   # set data argument for optimiser, if a data set exists
  #   if (is.list(x$data)){
  #     data_arg <- list(data = x$data[[grid_for_optim$data_idx[i]]])
  #   }
  #   else {
  #     data_arg <- c()
  #   }
  #
  #   # set further argument specifications (if there exist any)
  #   if ("argument_value" %in% colnames(grid_for_optim)) {
  #     x$f$add[[des_argument]] <- grid_for_optim$argument_value[i]
  #   }
  #
  #   out <- do.call_timed(
  #     what = x$optimizer$fun,
  #     args = c(main_args, data_arg, x$f$add)
  #   )
  #   x <- save_optimization_results(x, strategy = "fixed", res = out$res,
  #                                  time = out$time)
  # }

  ### return ino
  return(x)
}

#' Random initialization
#'
#' @description
#' This function is an implementation of the random initialization
#' strategy. See the vignette on initialization strategies for more details.
#'
#' @details
#' Missing.
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
#' The updated input \code{x}.
#'
#' @export
#'
#' @examples
#' #random_initialization()
#'
#' @keywords
#' strategy

random_initialization <- function(x, runs = 1, sampler = stats::rnorm) {

  # ### check inputs
  # if (class(x) != "ino") {
  #   stop("'x' must be of class 'ino'.")
  # }
  # if (!length(runs) == 1 && is_number(runs)) {
  #   stop("'runs' must be a numeric value.")
  # }
  # if (is.null(sampler)) {
  #   sampler <- function() stats::rnorm(x$f$npar)
  # } else {
  #   try_sampler <- try_silent(sampler())
  #   if (class(try_sampler) == "fail") {
  #     stop("'sampler' must be a function that returns a numeric vector.")
  #   }
  #   if (!(is.numeric(try_sampler) && length(try_sampler) == x$f$npar)) {
  #     stop("'sampler' must return a numeric vector of length 'npar'.")
  #   }
  # }
  #
  # # create list with one entry corresponding to one set of starting values
  # at <- replicate(runs, sampler(), simplify = FALSE)
  # # create grid
  # grid_for_optim <- create_grid(x, at)
  #
  # # check if multiple argument specifications were provided
  # if ("argument_value" %in% colnames(grid_for_optim)) {
  #   # name of argument for which multiple values are given
  #   des_argument <- names(which(lapply(x$f$add, length) > 1))
  # }
  #
  # ### loop over all parameter combinations provided
  # for(i in 1:nrow(grid_for_optim)){
  #   main_args <- list(f = x$f$f, p = grid_for_optim$at[[i]])
  #   # set data argument for optimiser, if a data set exists
  #   if(is.list(x$data)){
  #     data_arg <- list(data = x$data[[grid_for_optim$data_idx[i]]])
  #   }
  #   else{
  #     data_arg <- c()
  #   }
  #
  #   # set further argument specifications (if there exist any)
  #   if ("argument_value" %in% colnames(grid_for_optim)) {
  #     x$f$add[[des_argument]] <- grid_for_optim$argument_value[i]
  #   }
  #
  #   out <- do.call_timed(
  #     what = x$optimizer$fun,
  #     args = c(main_args, data_arg, x$f$add)
  #   )
  #
  #   ### save results in ino
  #   x <- save_optimization_results(x,
  #                                  strategy = "random",
  #                                  res = out$res,
  #                                  time = out$time)
  # }

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
