#' ino: Initialization strategies for numerical optimization
#'
#' @description
#' This package implements tools for the analysis of the initialization of
#' numerical optimization.
#'
#' @docType package
#' @name ino
#' @keywords
#' internal
"_PACKAGE"

#' @noRd
#' @importFrom progress progress_bar
#' @keywords
#' internal

ino_pb <- function(title = "", total) {
  progress::progress_bar$new(
    format = paste0(title, ":current/:total"),
    total = total,
    show_after = 0,
    clear = FALSE
  )
}

#' @noRd
#' @keywords
#' internal

ino_pp <- function(pb, verbose = getOption("ino_progress")) {
  if (verbose) if (pb$.__enclos_env__$private$total > 1) pb$tick()
}

#' @noRd
#' @keywords
#' internal

ino_status <- function(msg, verbose = getOption("ino_progress")) {
  if (verbose) message("* ", msg)
}

#' @noRd
#' @keywords
#' internal

.onLoad <- function(lib, pkg) {
  options("ino_progress" = TRUE)
  options("ino_ncores" = 1)
}

#' @noRd
#' @importFrom utils packageVersion
#' @keywords
#' internal

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "Thanks for using {ino} ", utils::packageVersion("ino"), "."
  )
  packageStartupMessage(msg)
  invisible()
}

#' Create grid of parameter combinations
#'
#' @description
#' This helper function creates a grid of all parameter combinations for an
#' \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' A \code{list}, where each element is a parameter set. Each parameter set
#' contains at least a placeholder for the target parameter, which is set to
#' \code{NA} and has to be filled by the initialization strategies.
#' Additionally, each parameter set contains further arguments for the target
#' function if available. In this case, the parameter names and identifier for
#' the parameter values are added as attributes \code{"par_name"} and
#' \code{"par_id"} to the parameter set.
#'
#' @keywords
#' internal

grid_ino <- function(x) {

  ### build grid of parameter identifiers
  grid_par <- as.list(names(x$f$add))
  names(grid_par) <- names(x$f$add)
  for(mpv in x$f$mpvs) grid_par[[mpv]] <- names(x$f$add[[mpv]])
  grid_par <- expand.grid(grid_par, stringsAsFactors = FALSE)

  ### build list of parameter sets
  par_sets <- list()
  for(i in 1:max(1,nrow(grid_par))) {
    target <- list(NA)
    names(target) <- x$f$target_arg
    par_set <- c(target, x$f$add)
    for(p in colnames(grid_par)) {
      if(p %in% x$f$mpvs) {
        par_set[p] <- par_set[[p]][grid_par[i,p]]
      } else {
        par_set[p] <- par_set[p][grid_par[i,p]]
      }
    }
    attr(par_set, "par_name") <- as.character(names(x$f$add))
    attr(par_set, "par_id") <- as.character(grid_par[i,])
    par_sets[[i]] <- par_set
  }

  ### return list of parameter sets
  return(par_sets)
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

result_ino <- function(x, strategy, pars, result, opt_name) {

  ### determine number of new optimization result
  nopt <- nrow(x[["runs"]][["table"]]) + 1

  ### save optimization results
  x[["runs"]][["table"]][nopt, ".strategy"] <- strategy
  x[["runs"]][["table"]][nopt, ".time"] <- result$time
  v <- x$opt[[opt_name]]$base_arg_names[3]
  x[["runs"]][["table"]][nopt, ".optimum"] <- result$res[[v]]
  if(length(x$opt) > 1)
    x[["runs"]][["table"]][nopt, ".optimizer"] <- opt_name
  for(i in seq_along(attr(pars, "par_name")))
    if(attr(pars, "par_name")[i] %in% x$f$mpvs)
      x[["runs"]][["table"]][nopt, attr(pars, "par_name")[i]] <-
    attr(pars, "par_id")[i]
  x[["runs"]][["pars"]][[nopt]] <- list()
  x[["runs"]][["pars"]][[nopt]][[".init"]] <- pars[[x$f$target_arg]]
  z <- x$opt[[opt_name]]$base_arg_names[4]
  x[["runs"]][["pars"]][[nopt]][[".estimate"]] <- result$res[[z]]
  opt_crit <- x$opt[[opt_name]]$crit
  crit_val <- result$res[opt_crit]
  for(i in seq_along(opt_crit)) {
    if(is.numeric(crit_val[[i]]) && length(crit_val[[i]]) == 1){
      x[["runs"]][["table"]][nopt, opt_crit[i]] <- crit_val[[i]]
    } else {
      x[["runs"]][["pars"]][[nopt]][[opt_crit[i]]] <- crit_val[[i]]
    }
  }

  ### return (invisibly) updated ino object
  return(invisible(x))
}


#' Test of an \code{ino} object
#'
#' @description
#' This helper function tests the specification of an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param verbose
#' Set to \code{TRUE} (\code{FALSE}) to print (hide) the test results of the
#' setup at the console.
#'
#' @return
#' The updated object \code{x} (invisibly).
#'
#' @keywords
#' internal
#'
#' @importFrom stats rnorm

test_ino <- function(x, verbose = getOption("ino_progress")) {

  ### helper functions
  ll <- NULL
  step <- function(desc) pline(desc)
  res <- function(msg = NULL, succ = FALSE, warn = FALSE) {
    if(succ) {
      cat(crayon::green("\U2713 "))
    } else if(warn){
      cat(crayon::yellow("X "))
      warning(msg, call. = FALSE, immediate. =  TRUE)
      ll <<- NULL
    } else {
      cat("\n")
      stop(msg, call. = FALSE)
    }
  }
  pline <- function(line = NULL) {
    if(!is.null(ll)) cat(crayon::silver(ll), "\n", sep = "")
    ll <<- line
    cat(line, "\r")
    Sys.sleep(ifelse(verbose,0.1,0))
    return(line)
  }

  ### start tests
  if(!verbose) { sink(tempfile()); on.exit(sink()) }

  ### check data types
  step("check that 'f' is of class 'function'")
  res(msg = "",
      succ = "function" %in% class(x$f$f))
  step("check that 'npar' is a numeric")
  res(msg = "",
      succ = is.numeric(x$f$npar))
  step("check that 'npar' is of length 1")
  res(succ = length(x$f$npar) == 1)
  step("check that 'npar' is a whole number")
  res(msg = "",
      succ = x$f$npar %% 1 == 0)
  step("check that 'npar' is non-negative")
  res(msg = "",
      succ = x$f$npar > 0)
  step("check that 'opt' is of class 'optimizer' or a list of those")
  res(msg = "'opt' is not of class 'optimizer' or a list of those",
      succ = all(sapply(x$opt, function(x) "optimizer" %in% class(x))))
  step("check that 'mpvs' is a character (vector)")
  res(msg = "",
      succ = is.character(x$f$mpvs))

  ### check names of parameters with mpvs
  if(length(x$f$mpvs) > 0) {
    for(mpv in x$f$mpvs){
      step(paste0("check names for parameter '",mpv,"'"))
      if(length(names(x$f$add[[mpv]])) == length(x$f$add[[mpv]])) {
        res(succ = TRUE)
      } else {
        res(msg = paste0("re-named '", mpv, "' by '", mpv, "1:",
                         length(x$f$add[[mpv]]),"'"),
            succ = FALSE,
            warn = TRUE)
        names(x$f$add[[mpv]]) <- paste0(mpv,1:length(x$f$add[[mpv]]))
      }
    }
  }

  ### check that function and optimizer can be called
  step("check name of target parameter in 'f'")
  res(succ = is.character(x$f$target_arg))
  rvx <- round(rnorm(x$f$npar),1)
  step(paste0("try to draw value of length 'npar' = ", x$f$npar, ": ",
              paste(rvx, collapse = " ")))
  res(succ = (length(rvx) == x$f$npar))
  step("try to create grid of parameter sets")
  grid <- grid_ino(x)
  res(succ = is.list(grid))
  for(i in 1:min(length(grid),10)){
    step(paste("check call to 'f' with parameter set", i))
    pars <- grid[[i]]
    pars[[x$f$target_arg]] <- rvx
    f_return <- try_silent(
      timed(expr = do.call(what = x$f$f,
                           args = pars),
            secs = 1)
    )
    res(msg = f_return,
        succ = !inherits(f_return, "fail"))
    for(o in seq_along(x$opt)) {
      step(paste0("check call to '", names(x$opt)[o],
                  "' with parameter set ", i))
      opt <- x$opt[[o]]
      base_args <- list(x$f$f, pars[[x$f$target_arg]])
      names(base_args) <- opt$base_arg_names[1:2]
      f_args <- pars
      f_args[[x$f$target_arg]] <- NULL
      o_return <- try_silent(
        timed(expr = do.call(what = opt$f,
                             args = c(base_args, f_args, opt$args)),
              secs = 1)
      )
      res(msg = o_return,
          succ = !inherits(o_return, "fail"))
    }
  }

  ### return (invisibly) updated ino object
  step("completed test cases\n")
  return(invisible(x))
}

#' @noRd
#' @keywords
#' internal

ino_call <- function(call) {
  call$ncores <- 1
  call$verbose <- FALSE
  class(call) <- c("ino_call", class(call))
  return(call)
}

#' @noRd
#' @export
#' @keywords
#' internal

print.ino_call <- function(x, ...) {
  cat("<ino_call>")
}

#' @noRd
#' @keywords
#' internal

ino_check_inputs <- function(...) {
  stop0 <- function(msg) stop(msg, call. = FALSE)
  inputs <- list(...)
  arg <- at <- by_col <- by_row <- center <- how <- prop <- runs <- NULL
  sampler <- x <- NULL
  within(inputs, {
    n <- names(inputs)
    if ("x" %in% n) {
      if (!inherits(x, "ino")) {
        stop0("'x' must be of class 'ino'.")
      }
    }
    if ("runs" %in% n) {
      if (!length(runs) == 1 && is_number(runs)) {
        stop0("'runs' must be an integer.")
      }
    }
    if ("sampler" %in% n) {
      if (!is.function(sampler)) {
        stop0("'sampler' must be a function.")
      }
    }
    if ("at" %in% n) {
      if (!is.numeric(at)) {
        stop0("'at' must be a numeric vector.")
      }
    }
    if ("arg" %in% n) {
      if (!is.character(arg)) {
        stop0("'arg' must be a character.")
      }
    }
    if (all(c("arg", "x") %in% n)) {
      if (!arg %in% names(x$f$add)) {
        stop0(paste0(
          "'arg' = '", arg, "' does not seem to be an argument of '",
          x$f$name, "'."
        ))
      }
      if (arg %in% x$f$mpvs &&
        !all(sapply(x$f$add[[arg]], inherits, c("matrix", "data.frame"))) ||
        !arg %in% x$f$mpvs &&
          !inherits(x$f$add[[arg]], c("matrix", "data.frame"))) {
        stop0(paste0(
          "The argument 'arg' = '", arg, "' does not seem to be of class ",
          "'matrix' or 'data.frame'."
        ))
      }
    }
    if ("how" %in% n) {
      if (!how %in% c("random", "first", "kmeans")) {
        stop0("'how' must be one of 'random', 'first', or 'kmeans'.")
      }
    }
    if ("prop" %in% n) {
      if (!(is.numeric(prop) && all(prop <= 1) && all(prop >= 0))) {
        stop0("(Each element of) 'prop' must be between 0 and 1.")
      }
    }
    if ("by_col" %in% n) {
      if (!(is.logical(by_col) || length(by_col) == 1)) {
        stop0("'by_col' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("by_row" %in% n) {
      if (!(is.logical(by_row) || length(by_row) == 1)) {
        stop0("'by_row' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("center" %in% n) {
      if (!(is.logical(center) || length(center) == 1)) {
        stop0("'center' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if ("scale" %in% n) {
      if (!(is.logical(scale) || length(scale) == 1)) {
        stop0("'scale' must be either 'TRUE' or 'FALSE'.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) > x$f$npar) {
        stop0("'at' has more entries than the function has parameters.")
      }
    }
    if (all(c("at", "x") %in% n)) {
      if (length(at) < x$f$npar) {
        stop0("'at' has less entries than the function has parameters.")
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
