#' Data subset initialization
#'
#' @description
#' This function is an implementation of the data subset initialization
#' strategy.
#'
#' @details
#' See the vignette on initialization strategies for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param how
#' A character, specifying how to select the data subset. Can be one of
#' \code{"random"} (default), \code{"first"}, and \code{"kmeans"}, see the
#' details.
#' @param prop
#' A numeric between 0 and 1, specifying the proportion of the data subset.
#' Can also be a vector...
#' @param data_arg
#' A character, the name of the data argument. The data argument must be ...
#' @param by_row
#' A boolean, ...
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
#' This function is an implementation of the fixed initialization strategy.
#'
#' @details
#' See the vignette on initialization strategies for more details.
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

  ### check inputs
  if (!inherits(x, "ino")) {
    stop("'x' must be of class 'ino'.", call. = FALSE)
  }
  if(!is.list(at)){
    stop("'at' must be a list.", call. = FALSE)
  }
  if(any(sapply(at,length) > x$f$npar)){
    stop("Some element in 'at' has more entries than the function has parameters.",
         call. = FALSE)
  }
  if(any(sapply(at,length) < x$f$npar)){
    stop("Some element in 'at' has less entries than the function has parameters.",
         call. = FALSE)
  }

  ### create parameter grid
  grid <- grid_ino(x)

  ### loop over parameter sets
  for(p in 1:length(grid)) {
    ### extract current parameter set
    pars <- grid[[p]]
    ### loop over 'at'
    for(r in seq_along(at)) {
      ### draw random initial value
      init <- at[[1]]
      ### save initial value in parameter set
      pars[[x$f$target_arg]] <- init
      ### loop over optimizer
      for(o in 1:length(x$opt)) {
        ### extract current optimizer
        opt <- x$opt[[o]]
        ### base arguments of the optimizer
        base_args <- list(x$f$f, pars[[x$f$target_arg]])
        names(base_args) <- opt$base_arg_names
        f_args <- pars
        f_args[[x$f$target_arg]] <- NULL
        result <- try_silent(
          do.call_timed(what = opt$f, args = c(base_args, f_args, opt$args))
        )
        if(inherits(result, "fail")) {
          warning("Optimization failed with message", result)
        }
        ### save new result
        x <- result_ino(x = x, strategy = "fixed", pars = pars,
                        result = result, opt_name = names(x$opt)[o])
      }
    }
  }

  ### return ino object
  return(x)
}

#' Random initialization
#'
#' @description
#' This function is an implementation of the random initialization
#' strategy.
#'
#' @details
#' See the vignette on initialization strategies for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param runs
#' The number of random initialization runs, the default is \code{1}.
#' @param sampler
#' The sampler for random initial values. Can be any function that
#' \itemize{
#'   \item has as first argument an integer, say \code{npar},
#'   \item and returns a numeric vector of length \code{npar}.
#' }
#' Per default, \code{sampler = stats::rnorm}, i.e. independent draws from a
#' standard normal distribution as initial value.
#' @param ...
#' Additional argument to \code{sampler} (optional).
#'
#' @return
#' The updated input \code{x}.
#'
#' @export
#'
#' @importFrom utils capture.output
#'
#' @examples
#' #random_initialization()
#'
#' @keywords
#' strategy

random_initialization <- function(x, runs = 1, sampler = stats::rnorm, ...) {

  ### check inputs
  if (!inherits(x, "ino")) {
    stop("'x' must be of class 'ino'.")
  }
  if (!length(runs) == 1 && is_number(runs)) {
    stop("'runs' must be an integer.")
  }

  ### check sampler
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
    stop("Expected that 'sampler' returns a numeric vector of length 'x$f$npar'.\n",
         "Instead, 'sampler' returned:\n",
         paste(capture.output(str(try_sampler)), collapse = "\n"), call. = FALSE)
  }

  ### create parameter grid
  grid <- grid_ino(x)

  ### loop over parameter sets
  for(p in 1:length(grid)) {
    ### extract current parameter set
    pars <- grid[[p]]
    ### loop over runs
    for(r in 1:runs) {
      ### draw random initial value
      init <- sampler_init(x$f$npar)
      ### save initial value in parameter set
      pars[[x$f$target_arg]] <- init
      ### loop over optimizer
      for(o in 1:length(x$opt)) {
        ### extract current optimizer
        opt <- x$opt[[o]]
        ### base arguments of the optimizer
        base_args <- list(x$f$f, pars[[x$f$target_arg]])
        names(base_args) <- opt$base_arg_names
        f_args <- pars
        f_args[[x$f$target_arg]] <- NULL
        result <- try_silent(
          do.call_timed(what = opt$f, args = c(base_args, f_args, opt$args))
        )
        if(inherits(result, "fail")) {
          warning("Optimization failed with message", result)
        }
        ### save new result
        x <- result_ino(x = x, strategy = "random", pars = pars,
                        result = result, opt_name = names(x$opt)[o])
      }
    }
  }

  ### return ino object
  return(x)
}
