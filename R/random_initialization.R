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
#' of length `npar`.
#'
#' @return NULL
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

  ### loop over runs
  for(run in 1:runs){

    ### random initialization
    p <- sampler()
    main_args <- list(f = x$f$f, p = p)
    out <- do.call_timed(
      what = x$optimizer$fun,
      args = c(main_args, x$f$add)
    )

    ### save results in ino
    x <- save_optimization_results(x, strategy = "random", res = out$res,
                                   time = out$time)
  }

  ### return ino
  return(x)
}
