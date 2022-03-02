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

  ### loop over all parameter combinations provided
  for(p in at){
    main_args <- list(f = x$f$f, p = p)
    out <- do.call_timed(
      what = x$optimizer$fun,
      args = c(main_args, x$f$add)
    )
    x <- save_optimization_results(x, strategy = "fixed", res = out$res,
                                   time = out$time)
  }

  ### return ino
  return(x)
}
