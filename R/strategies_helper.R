#' @noRd
#' @keywords
#' internal

strategy_call <- function(call) {
  class(call) <- c("strategy_call", class(call))
  return(call)
}

#' @exportS3Method
#' @noRd
#' @keywords
#' internal

print.strategy_call <- function(x, ...) {
  cat("<strategy_call>")
}

#' Check inputs
#'
#' @description
#' This helper function centralizes several input checks.
#'
#' @param ...
#' Named inputs to be checked.
#'
#' @return
#' Returns \code{NULL} invisibly.
#'
#' @keywords
#' internal

check_inputs <- function(...) {
  inputs <- list(...)
  within(inputs, {
    n <- names(inputs)
    if ("x" %in% n) {
      if (!inherits(x, "ino")) {
        ino_stop(
          event = "'x' must be of class 'ino'."
        )
      }
    }
    if ("sampler" %in% n) {
      if (!is.function(sampler)) {
        ino_stop(
          event = "'sampler' must be a function."
        )
      }
      sampler_try <- try_silent(sampler())
      if (!is.numeric(sampler_try) || length(sampler_try) != npar(x)) {
        ino_stop(
          event = "'sampler' must return a numeric vector of length 'npar(x)'."
        )
      }
    }
    if ("ncores" %in% n) {
      if(length(ncores) != 1 || !is_number(ncores) ) {
        ino_stop(
          event = "'ncores' must be a positive integer."
        )
      }
    }
    if ("verbose" %in% n) {
      if(length(verbose) != 1 || (!isTRUE(verbose) && !isFALSE(verbose))) {
        no_stop(
          event = "'verbose' must be either TRUE or FALSE."
        )
      }
    }
    if ("at" %in% n) {
      if (!is.numeric(at) || length(at) != npar(x)) {
        ino_stop(
          event = "'at' must be a numeric vector of length 'npar(x)'."
        )
      }
    }
    if ("arg" %in% n) {
      if (!is.character(arg) || length(arg) != 1) {
        ino_stop(
          event = "'arg' must be a character."
        )
      }
      if (!arg %in% names(x$prob$add)) {
        ino_stop(
          event = paste0(
            "'", arg, "' is not an argument of '", x$prob$f_name, "'."
          )
        )
      }
      if (!all(
        sapply(x$prob$add[[arg]], inherits, c("matrix", "data.frame")))
      ) {
        ino_stop(
          event = paste0(
            "Argument '", arg, "' is not a matrix or data frame'."
          )
        )
      }
    }
    if ("by_col" %in% n) {
      if (length(by_col) != 1 || (!isFALSE(by_col) && !isTRUE(by_col))) {
        ino_stop(
          event = "'by_col' must be either TRUE or FALSE."
        )
      }
    }
    if ("center" %in% n) {
      if (length(center) != 1 || (!isFALSE(center) && !isTRUE(center))) {
        ino_stop(
          event = "'center' must be either TRUE or FALSE."
        )
      }
    }
    if ("scale" %in% n) {
      if (length(scale) != 1 || (!isFALSE(scale) && !isTRUE(scale))) {
        ino_stop(
          event = "'scale' must be either TRUE or FALSE."
        )
      }
    }
    if ("ind_ign" %in% n) {
      if (!is.numeric(ind_ign)) {
        ino_stop(
          event = "'ind_ign' must be a numeric vector."
        )
      }
      if (any(
        sapply(x$prob$add[[arg]],
               function(a) {
                bound <- ifelse(by_col, ncol(a), nrow(a))
                any(!ind_ign %in% seq_len(bound))
               }))
      ) {
        ino_stop(
          event = "'ind_ign' is out of bound."
        )
      }
    }
    if ("initialization" %in% n) {
      if (!inherits(initialization, "strategy_call")) {
        ino_stop(
          event = "'initialization' must be of class 'strategy_call'."
        )
      }
    }
    ###
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

    if ("by_row" %in% n) {
      if (!(is.logical(by_row) || length(by_row) == 1)) {
        ino_stop("'by_row' must be either 'TRUE' or 'FALSE'.")
      }
    }

  })
  return(invisible(NULL))
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
#' A list, each element contains
#' * the set number \code{i} corresponding to \code{grid_ino(x)}
#' * and the list output of \code{\link[optimizeR]{optimizeR}}.
#'
#' @keywords
#' internal
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom optimizeR optimizeR

optimize <- function(x, init, ncores, verbose) {
  stopifnot(is.numeric(init), length(init) == npar(x))
  grid <- grid_ino(x)
  cluster <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cluster))
  doSNOW::registerDoSNOW(cluster)
  pb <- ino_pb(title = "  grid set ", total = length(grid))
  opts <- list(progress = function(n) ino_pp(pb = pb, verbose = verbose))
  i <- 1
  foreach::foreach(
    i = 1:length(grid), .packages = c("optimizeR"), .options.snow = opts,
    .inorder = FALSE
  ) %dopar% {
    opt <- grid[[i]][[".optimizer"]]
    grid[[i]][[".optimizer"]] <- NULL
    result <- try_silent(
      do.call(
        what = optimizeR::optimizeR,
        args = c(
          list(
            "optimizer" = opt,
            "f" = x$prob$f,
            "p" = init
          ),
          grid[[i]]
        )
      )
    )
    list("i" = i, "result" = result)
  }
}

#' Save results
#'
#' @description
#' This helper function saves the results of optimization runs into the
#' submitted \code{ino} object.
#'
#' @details
#' The results are saved at \code{x$runs}, which is a list of two elements:
#' * The \code{table} element is a data frame. It has a row for each
#'   recorded optimization run, columns contain optimization results. It stores
#'   * the name of the initialization strategy \code{.strategy},
#'   * the optimization time \code{.time} (\code{difftime} object),
#'   * the function value at the optimum \code{.optimum},
#'   * the identifier for the optimizer \code{.optimizer},
#'   * and identifier for additional parameters for the target function.
#' * The \code{pars} element is a list. It has an element for each recorded
#'   optimization run. It stores
#'   * the initial parameter vector \code{.init},
#'   * the parameter estimate \code{.estimate},
#'   * and additional outputs of the optimizer.
#'
#' @param x
#' An object of class \code{ino}.
#' @param result
#' The output of \code{\link{optimize}}.
#' @param strategy
#' The name of the initialization strategy.
#'
#' @return
#' The updated object \code{x}.
#'
#' @keywords
#' internal

save_result <- function(x, result, strategy, init) {
  stopifnot(inherits(x, "ino"), is.list(result), is.character(strategy),
            length(strategy) == 1)
  grid <- grid_ino(x)
  grid_overview <- attr(grid, "overview")
  names_grid_overview <- colnames(grid_overview)
  nopt <- nrow(x$runs$table)
  res_seq <- sapply(result, `[[`, "i")
  res <- lapply(result, `[[`, "result")
  res_fail <- sum(sapply(res, inherits, "fail"))
  if (res_fail > 0) {
    ino_warn(
      event = paste(res_fail, "of", length(res), "runs failed.")
    )
  }
  for (s in res_seq) {
    if (!inherits(res[[s]], "fail")) {
      x$runs$table[nopt + s, ".strategy"] <- strategy
      x$runs$table[nopt + s, ".time"] <- res[[s]][["time"]]
      x$runs$table[nopt + s, ".optimum"] <- res[[s]][["v"]]
      x$runs$table[nopt + s, names_grid_overview] <- grid_overview[s,]
      x$runs$pars[[nopt + s]] <- c(
        list(".init" = init, ".estimate" = res[[s]]$z),
        res[[s]][!names(res[[s]]) %in% c("v","z","time")]
      )
    }
  }
  return(x)
}

#' @noRd
#' @keywords
#' internal

subset_arg <- function(x, arg, how, prop, by_row, ind_ign, kmeans_arg) {

  ### check inputs
  ino_check_inputs(
    "x" = x, "arg" = arg, "how" = how, "prop" = prop, "by_row" = by_row,
    "ind_ign" = ind_ign, "kmeans_arg" = kmeans_arg
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

  ### perform subsetting
  if (arg %in% x$f$mpvs) {
    x$f$add[[arg]] <- lapply(x$f$add[[arg]], do_subset_arg)
  } else {
    x$f$add[[arg]] <- do_subset_arg(x$f$add[[arg]])
  }

  ### return updated ino object
  return(x)
}

