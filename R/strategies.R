#' Random initialization
#'
#' @description
#' This function is an implementation of the random initialization strategy.
#'
#' @param x
#' An object of class \code{ino}.
#' @param runs
#' An integer, the number of random initializations.
#' The default is \code{1}.
#' @param sampler
#' A function without any arguments which returns a numeric vector of length
#' \code{npar(x)} with (random) initial values.
#' Per default, \code{sampler = function() stats::rnorm(npar(x))}, i.e. random
#' initial values from a standard normal distribution.
#' @param ncores
#' The number of cores for parallel computation over parameters and optimizers.
#' The default is \code{getOption("ino_ncores")}, which is set to \code{1}
#' when the package is loaded.
#' @param verbose
#' A boolean, which indicates whether progress should be printed.
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) progress.
#' The default is \code{getOption("ino_progress")}, which is set to \code{TRUE}
#' when the package is loaded.
#' @param label
#' A character, the label for the initialization strategy.
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' strategy
#'
#' @importFrom progress progress_bar
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar% %do%
#'
#' @seealso
#' [npar()] to extract the number \code{npar} from an \code{ino} object.

random_initialization <- function(
    x, runs = 1L, sampler = function() stats::rnorm(npar(x)),
    ncores = getOption("ino_ncores"), verbose = getOption("ino_progress"),
    return_result = getOption("ino_return_result"), label = "random"
) {
  if (missing(x)) {
    return(strategy_call(match.call(expand.dots = TRUE)))
  }
  check_inputs(
    x = x, runs = runs, sampler = sampler, ncores = ncores, verbose = verbose,
    return_result = return_result, label = label
  )
  ino_status("Random initial values", verbose = verbose)
  inits <- replicate(runs, sampler(), simplify = FALSE)
  pb <- progress::progress_bar$new(
    format = "Run :current/:total | :eta", total = runs, clear = FALSE
  )
  opts <- structure(
    list(function(n) {
      if (verbose) if (pb$.__enclos_env__$private$total > 1) pb$tick()
    }),
    names = "progress"
  )
  parallel <- FALSE
  if (ncores > 1 && runs > ncores) {
    parallel <- TRUE
    cluster <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cluster))
    doSNOW::registerDoSNOW(cluster)
    ncores <- 1
    `%par_seq%` <- foreach::`%dopar%`
    parallel <- TRUE
  }
  `%par_seq%` <- ifelse(parallel, foreach::`%dopar%`, foreach::`%do%`)
  results <- foreach::foreach(
    run = 1:runs, .packages = c("ino"), .options.snow = opts
  ) %par_seq% {
    if(!parallel) pb$tick()
    optimize(x = x, init = inits[[run]], ncores = ncores, verbose = verbose)
  }
  if (return_result) {
    return(results)
  } else {
    for(run in 1:runs) {
      x <- save_result(
        x = x, result = results[[run]], strategy = label, init = inits[[run]]
      )
    }
    return(x)
  }
}

#' Fixed initialization
#'
#' @description
#' This function is an implementation of the fixed initialization strategy.
#'
#' @param at
#' A numeric vector of length \code{npar(x)} with the (fixed) initial values.
#' @inheritParams random_initialization
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' strategy
#'
#' @seealso
#' [npar()] to extract the number \code{npar} from an \code{ino} object.

fixed_initialization <- function(
    x, at, ncores = getOption("ino_ncores"),
    verbose = getOption("ino_progress"),
    return_result = getOption("ino_return_result"), label = "fixed"
) {
  if (missing(x)) {
    return(strategy_call(match.call(expand.dots = TRUE)))
  }
  check_inputs(
    x = x, at = at, ncores = ncores, verbose = verbose,
    return_result = return_result, label = label
  )
  ino_status("Fixed initial values", verbose = verbose)
  result <- optimize(x = x, init = at, ncores = ncores, verbose = verbose)
  x <- save_result(x = x, result = result, strategy = label, init = at)
  if (return_result) {
    return(result)
  } else {
    return(x)
  }
}

#' Standardize initialization
#'
#' @description
#' This function is an implementation of the standardize initialization
#' strategy.
#'
#' @param arg
#' A character, the name of the argument to be standardized.
#' The argument must be of class \code{matrix} or \code{data.frame}.
#' Per default, \code{arg = "data"}.
#' @param by_col
#' A boolean, set to \code{TRUE} (the default) to standardize column-wise, set
#' to \code{FALSE} to standardize by rows.
#' @param center
#' A boolean, set to \code{TRUE} (the default) for mean standardization.
#' @param scale
#' A boolean, set to \code{TRUE} (the default) for variance standardization.
#' @param ind_ign
#' A numeric vector of column indices (or row indices if \code{by_col = FALSE})
#' that are ignored when standardizing.
#' @param initialization
#' An object of class \code{strategy_call} which determines the initialization.
#' The \code{strategy_call} can be generated by one of the strategy functions
#' (any function with the name \code{*_initialization}), when the \code{x}
#' argument is unspecified.
#' Per default, \code{initialization = random_initialization()}, i.e. random
#' initialization.
#' @inheritParams random_initialization
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' strategy

standardize_initialization <- function(
    x, arg = "data", by_col = TRUE, center = TRUE, scale = TRUE,
    ind_ign = integer(), initialization = random_initialization(),
    ncores = getOption("ino_ncores"), verbose = getOption("ino_progress"),
    return_result = getOption("ino_return_result"), label = "standardize"
) {
  if (missing(x)) {
    return(strategy_call(match.call(expand.dots = TRUE)))
  }
  check_inputs(
    x = x, arg = arg, by_col = by_col, center = center, scale = scale,
    ind_ign = ind_ign, initialization = initialization, ncores = ncores,
    verbose = verbose, return_result = return_result, label = label
  )
  ino_status("Standardizing", verbose = verbose)
  x_st <- clear_ino(x, which = "all")
  x_st$prob$add[[arg]] <- lapply(
    x_st$prob$add[[arg]],
    function(arg_val) {
      if (!by_col) {
        arg_val <- t(arg_val)
      }
      for (i in setdiff(1:ncol(arg_val), ind_ign)) {
        arg_val[, i] <- scale(arg_val[, i], center = center, scale = scale)
      }
      if (!by_col) {
        arg_val <- t(arg_val)
      }
      return(arg_val)
    }
  )
  x_st <- do.call(
    what = rlang::call_name(initialization),
    args = c(
      list("x" = x_st),
      rlang::call_args(initialization),
      label = label
    )
  )
  x$runs <- c(x$runs, x_st$runs)
  # TODO: add option to simply return optimization result
  return(x)
}

#' Subset initialization
#'
#' @description
#' This function is an implementation of the subset initialization strategy.
#'
#' @param arg
#' A character, the name of the argument to be subsetted.
#' Only an argument of class \code{matrix} or \code{data.frame} can be chosen.
#' Per default, \code{arg = "data"}.
#' @param by_row
#' A boolean, set to \code{TRUE} (the default) to subset by row, set to
#' \code{FALSE} to subset by column.
#' @param how
#' A character, specifying how to select the subset.
#' Can be one of \code{"random"} (default), \code{"first"}, and \code{"kmeans"}.
#' @param prop
#' A numeric between 0 and 1, specifying the proportion of the subset.
#' @param ind_ign
#' A numeric vector of column indices (or row indices if \code{by_row = FALSE})
#' that are ignored when clustering.
#' Only relevant if \code{how = "kmeans"}.
#' @param kmeans_arg
#' A list of additional arguments for \code{\link[stats]{kmeans}}.
#' Per default, \code{kmeans_arg = list(centers = 2)}, which sets the number of
#' clusters to 2.
#' Only relevant if \code{how = "kmeans"}.
#' @inheritParams standardize_initialization
#'
#' @return
#' The updated \code{ino} object.
#'
#' @export
#'
#' @keywords
#' strategy
#'
#' @importFrom stats kmeans

subset_initialization <- function(
    x, arg = "data", by_row = TRUE, how = "random", prop = 0.5,
    ind_ign = integer(), kmeans_arg = list("centers" = 2),
    initialization = random_initialization(),
    ncores = getOption("ino_ncores"), verbose = getOption("ino_progress"),
    return_result = getOption("ino_return_result"),
    label = paste0("subset(",how,",",prop,")")
) {
  if (missing(x)) {
    return(strategy_call(match.call(expand.dots = TRUE)))
  }
  check_inputs(
    x = x, arg = arg, by_row = by_row, how = how, prop = prop,
    ind_ign = ind_ign, kmeans_arg = kmeans_arg,
    initialization = initialization, ncores = ncores, verbose = verbose,
    return_result = return_result, label = label
  )
  ino_status("Subsetting", verbose = verbose)
  x_subset <- clear_ino(x, which = "all")
  x_subset$prob$add[[arg]] <- lapply(
    x_subset$prob$add[[arg]],
    function(arg_val) {
      if (!by_row) arg_val <- t(arg_val)
      arg_val_length <- nrow(arg_val)
      arg_val_subset_length <- ceiling(arg_val_length * prop)
      if (how == "random") {
        subset_ind <- sort(sample.int(arg_val_length, arg_val_subset_length))
      } else if (how == "first") {
        subset_ind <- 1:arg_val_subset_length
      } else if (how == "kmeans") {
        arg_val_ign <- arg_val
        if (!is.null(ind_ign)) {
          arg_val_ign <- arg_val_ign[, -ind_ign, drop = FALSE]
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
  )
  x_subset <- do.call(
    what = rlang::call_name(initialization),
    args = c(
      list("x" = x_subset),
      rlang::call_args(initialization),
      label = label
    )
  )

  # TODO: run optimization with full set
  # inits <- x_subset$runs$.init
  # result <- optimize()
  # x <- save_result(x, result, init)
  # x <- subset_to_full(x, x_subset, initialization)

  x$runs <- c(x$runs, x_subset$runs)
  return(x)
  # TODO: add option to simply return optimization result
}

#' @noRd
#' @keywords
#' internal

strategy_call <- function(call) {
  structure(
    call,
    class = c("strategy_call", class(call))
  )
}

#' @exportS3Method
#' @noRd
#' @keywords
#' internal

print.strategy_call <- function(x, ...) {
  cat("<strategy_call>")
}

#' Optimization
#'
#' @description
#' This helper function performs numerical optimization based on an \code{ino}
#' object and initial values.
#'
#' @param init
#' A numeric vector of length \code{npar(x)}.
#' @inheritParams random_initialization
#'
#' @return
#' A list of outputs of \code{\link[optimizeR]{optimizeR}}.
#'
#' @keywords
#' internal
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom optimizeR optimizeR
#' @importFrom progress progress_bar

optimize <- function(x, init, ncores, verbose) {
  stopifnot(is.numeric(init), length(init) == npar(x))
  grid <- grid_ino(x)
  if (length(grid) < ncores) {
    ncores == length(grid)
  }
  cluster <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cluster))
  doSNOW::registerDoSNOW(cluster)
  pb <- progress::progress_bar$new(
    format = "Grid set :current/:total", total = length(grid), clear = TRUE
  )
  opts <- structure(
    list(function(n) {
      if (verbose) if (pb$.__enclos_env__$private$total > 1) pb$tick()
    }),
    names = "progress"
  )
  i <- 1
  foreach::foreach(
    i = 1:length(grid), .packages = c("optimizeR"), .options.snow = opts
  ) %dopar% {
    opt <- grid[[i]][[".optimizer"]]
    grid[[i]][[".optimizer"]] <- NULL
    try_silent(
      do.call(
        what = optimizeR::optimizeR,
        args = c(
          list("optimizer" = opt, "f" = x$prob$f, "p" = init),
          grid[[i]]
        )
      )
    )
  }
}

#' Save results
#'
#' @description
#' This helper function saves the results of optimization runs into the
#' submitted \code{ino} object.
#'
#' @details
#' The results are saved at \code{x$runs}, which is an object of class
#' \code{runs}. See \code{\link{new_runs}} for its documentation.
#'
#' @param x
#' An object of class \code{ino}.
#' @param result
#' The output of \code{\link{optimize}}.
#' @param strategy
#' The name of the initialization strategy.
#' @param
#' A numeric vector of initial parameters of length \code{npar(x)}.
#'
#' @return
#' The updated input \code{x}.
#'
#' @keywords internal

save_result <- function(x, result, strategy, init) {
  grid <- grid_ino(x)
  grid_overview <- attr(grid, "overview")
  names_grid_overview <- colnames(grid_overview)
  nruns <- nruns(x)
  res_fail <- sum(sapply(result, inherits, "fail"))
  if (res_fail > 0) {
    ino_warn(
      event = paste(res_fail, "of", length(result), "runs failed."),
      debug = "The failed runs are not saved."
    )
  }
  for (s in seq_along(result)) {
    if (!inherits(result[[s]], "fail")) {
      x$runs[[nruns + s]] <- c(
        list(
          ".strategy" = strategy,
          ".time" =  result[[s]][["time"]],
          ".optimum" = result[[s]][["v"]],
          ".init" = init,
          ".estimate" = result[[s]][["z"]]
          ),
        structure(
          as.list(as.character(grid_overview[s,])),
          names = names_grid_overview
        ),
        result[[s]][!names(result[[s]]) %in% c("v","z","time")]
      )
    } else {
      # TODO: save failed runs per default, add option to skip failed runs
    }
  }
  return(x)
}

#' Specify ao optimizer
#'
#' @description
#' This function is a wrapper for \code{\link[optimizeR]{set_optimizer}} with
#' the \code{\link[ao]{ao}} optimizer.
#'
#' @inheritParams optimizeR::set_optimizer
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @export
#'
#' @keywords
#' internal
#'
#' @importFrom ao ao
#' @importFrom optimizeR optimizeR

set_optimizer_ao <- function(..., out_ign = character(), test_par = list()) {
  if ("partition" %in% names(list(...)) && identical(test_par, list())) {
    test_par$validate <- FALSE
  }
  optimizeR::set_optimizer(
    opt = ao::ao,
    f = "f",
    p = "p",
    v = "optimum" ,
    z = "estimate",
    ...,
    out_ign = out_ign,
    test_par = test_par
  )
}
