#' Fixed initialization
#'
#' This function implemented the fixed initialization strategy.
#'
#' @details
#' See the vignette ... for more details.
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
  if(!(is.list(at))){
    stop("'at' must be a list.")
  }
  if(length(at) > x$f$npar){
    stop("'at' has more entries than the function has parameters.")
  }
  if(length(at) < x$f$npar){
    stop("'at' has less entries than the function has parameters.")
  }

  ### loop over all parameter combinations provided
  for(run in 1:length(at[[1]])){

    ### fixed initialization
    p <- unname(unlist(lapply(at, `[[`, 1)))
    main_args <- list(f = x$f$f, p = p)
    out <- do.call_timed(
      what = x$optimizer$fun,
      args = c(main_args, x$f$add)
    )

    ### save results in ino
    x <- save_optimization_results(x, strategy = "fixed", res = out$res,
                                   time = out$time)
  }

  ### return ino
  return(x)
}
