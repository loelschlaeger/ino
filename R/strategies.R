#' Subset initialization
#'
#' @description
#' This function is an implementation of the subset initialization strategy.
#'
#' @details
#' See the vignette on initialization strategies for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param arg
#' A character, the name of the argument to be subsetted. The argument must be
#' of class \code{matrix} or \code{data.frame}.
#' @param how
#' A character, specifying how to select the subset. Can be one of
#' \code{"random"} (default), \code{"first"}, and \code{"kmeans"}, see the
#' details.
#' @param prop
#' A numeric between 0 and 1, specifying the proportion of the subset.
#' Can also be a vector of proportions.
#' @param row
#' A boolean, set to \code{TRUE} to subset by row, set to \code{FALSE} to subset
#' by column.
#' @param ncores
#' This function is parallelized, set the number of cores here.
#'
#' @return
#' The updated input \code{x}.
#'
#' @export
#'
#' @examples
#' #subset_initialization()
#'
#' @keywords
#' strategy

subset_initialization <- function(x, arg = "data", how = "random", prop = 0.5,
                                  row = TRUE, ncores = parallel::detectCores()-1) {

  ### initial message
  ino_status("subset initialization")

  ### check inputs
  if (!inherits(x, "ino")) {
    stop("'x' must be of class 'ino'.", call. = FALSE)
  }
  if(!is.character(arg)){
    stop("'arg' must be a character.", call. = FALSE)
  }
  if(!arg %in% names(x$f$add)) {
    stop(paste0("'arg' = '", arg, "' does not seem to be an argument of '",
                x$f$name, "'."), call. = FALSE)
  }
  if(arg %in% x$f$mpvs && !all(sapply(x$f$add[[arg]], inherits, c("matrix","data.frame"))) ||
     arg %in% !x$f$mpvs && !inherits(x$f$add[[arg]], c("matrix","data.frame"))) {
      stop(paste0("The argument 'arg' = '", arg, "' does not seem to be of class ",
                  "'matrix' or 'data.frame'."), call. = FALSE)
  }
  if(!how %in% c("random", "first", "kmeans")) {
    stop("'how' must be one of 'random', 'first', or 'kmeans'.", call. = FALSE)
  }
  if(how == "kmeans"){
    # add check if columns are numeric
    # add argument that specifies the columns for clustering
  }
  if(!(is.numeric(prop) && all(prop <= 1) && all(prop >= 0))) {
    stop("(Each element of) 'prop' must be between 0 and 1.", call. = FALSE)
  }
  if(!(is.logical(row) || length(logical) == 1)) {
    stop("'row' must be either 'TRUE' or 'FALSE'.")
  }

  ### create parameter grid
  grid <- grid_ino(x)

  ### initialize progress bar
  pb <- ino_pb(title = "", total = length(grid))

  ### initialize parallel cluster
  cluster <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cluster)
  opts <- list(progress = function(n) ino_pp(pb))

  ### loop over parameter sets
  p <- NULL
  loop_res <- foreach::foreach(p = 1:length(grid), .packages = "ino",
                               .options.snow = opts) %dopar% {

   ### extract current parameter set
   pars <- grid[[p]]

   ### initialize list for results for loop p
   loop_res_p <- list()

   ### loop over 'how' and 'prop'
   for(h in how) for(pr in prop) {

     ### determine initial value
     pars_arg_full <- pars[[arg]]
     pars_arg_full_length <- ifelse(row, nrow(pars_arg_full), ncol(pars_arg_full))
     pars_arg_subset_length <- ceiling(pars_arg_full_length * pr)
     if(h == "random"){


       init <- 0




     } else if (h == "first") {
       init <- 0
     } else if (h == "kmeans") {
       init <- 0
     } else {
       stop()
     }



     init <- at[[r]]

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
         warning("Optimization failed with message", result, immediate. = TRUE)
       }

       ### save new result
       loop_res_p[[length(loop_res_p) + 1]] <- list(
         "pars" = pars, "result" = result, "opt_name" = names(x$opt)[o]
       )
     }
   }

   ### return results of loop p
   loop_res_p
  }

  ### terminate cluster
  parallel::stopCluster(cluster)

  ### save optimization results
  ino_status("saving results")
  loop_res <- unlist(loop_res, recursive = FALSE)
  for(res in seq_along(loop_res)){
    x <- do.call(what = result_ino,
                 args = append(list("x" = x, "strategy" = "fixed"),
                               loop_res[[res]]))
  }

  ### return ino object
  ino_status("done")
  return(x)

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
#' @param ncores
#' This function is parallelized, set the number of cores here.
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

fixed_initialization <- function(x, at, ncores = parallel::detectCores()-1) {

  ### initial message
  ino_status("fixed initialization")

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

  ### initialize progress bar
  pb <- ino_pb(title = "", total = length(grid))

  ### initialize parallel cluster
  cluster <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cluster)
  opts <- list(progress = function(n) ino_pp(pb))

  ### loop over parameter sets
  p <- NULL
  loop_res <- foreach::foreach(p = 1:length(grid), .packages = "ino",
                               .options.snow = opts) %dopar% {

    ### extract current parameter set
    pars <- grid[[p]]

    ### initialize list for results for loop p
    loop_res_p <- list()

    ### loop over 'at'
    for(r in seq_along(at)) {

      ### draw random initial value
      init <- at[[r]]

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
          warning("Optimization failed with message", result, immediate. = TRUE)
        }

        ### save new result
        loop_res_p[[length(loop_res_p) + 1]] <- list(
          "pars" = pars, "result" = result, "opt_name" = names(x$opt)[o]
        )
      }
    }

    ### return results of loop p
    loop_res_p
  }

  ### terminate cluster
  parallel::stopCluster(cluster)

  ### save optimization results
  ino_status("saving results")
  loop_res <- unlist(loop_res, recursive = FALSE)
  for(res in seq_along(loop_res)){
    x <- do.call(what = result_ino,
                 args = append(list("x" = x, "strategy" = "fixed"),
                               loop_res[[res]]))
  }

  ### return ino object
  ino_status("done")
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
#' @param ncores
#' This function is parallized, set the number of cores here.
#' @param ...
#' Additional argument to \code{sampler} (optional).
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
#'
#' @importFrom utils capture.output
#' @importFrom foreach %dopar%

random_initialization <- function(x, runs = 1, sampler = stats::rnorm,
                                  ncores = parallel::detectCores()-1, ...) {

  ### initial message
  ino_status("random initialization")

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

  ### initialize progress bar
  pb <- ino_pb(title = "", total = length(grid))

  ### initialize parallel cluster
  cluster <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cluster)
  opts <- list(progress = function(n) ino_pp(pb))

  ### loop over parameter sets
  p <- NULL
  loop_res <- foreach::foreach(p = 1:length(grid), .packages = "ino",
                               .options.snow = opts) %dopar% {

    ### extract current parameter set
    pars <- grid[[p]]

    ### initialize list for results for loop p
    loop_res_p <- list()

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

        ### optimize
        result <- try_silent(
          do.call_timed(
            what = opt$f,
            args = c(base_args, f_args, opt$args)
            )
        )
        if(inherits(result, "fail")) {
          warning("Optimization failed with message", result, immediate. = TRUE)
        }

        ### save new result
        loop_res_p[[length(loop_res_p) + 1]] <- list(
          "pars" = pars, "result" = result, "opt_name" = names(x$opt)[o]
        )
      }
    }

    ### return results of loop p
    loop_res_p
  }

  ### terminate cluster
  parallel::stopCluster(cluster)

  ### save optimization results
  ino_status("saving results")
  loop_res <- unlist(loop_res, recursive = FALSE)
  for(res in seq_along(loop_res)){
    x <- do.call(what = result_ino,
                 args = append(list("x" = x, "strategy" = "random"),
                               loop_res[[res]]))
  }

  ### return ino object
  ino_status("done")
  return(x)
}
