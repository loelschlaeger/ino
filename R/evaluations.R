#' Variables overview
#'
#' @description
#' This function provides an overview of the available variables for the
#' \code{...} argument of \code{\link{summary.ino}}.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' A data frame with columns
# TODO: add description
#' \describe{
#'   \item{name}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @keywords
#' evaluation

overview_vars <- function(x) {
  if (nruns(x) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  names <- c(unique(as.vector(sapply(x$runs, names))),
             if(!is.null(x$prob$global)) ".global")
  desc <- sapply(names, function(name) {
    if(name == ".strategy") {
      return("strategy label")
    } else if (name == ".time") {
      return("optimization time")
    } else if(name == ".optimum") {
      return("optimal function value")
    } else if(name == ".optimizer") {
      return("optimizer label")
    } else if(name == ".init") {
      return("initial parameters")
    } else if(name == ".global") {
      return("estimated parameters")
    } else if(name %in% attr(x$prob$add, "mpvs")) {
      return("function parameter")
    } else {
      "optimizer output"
    }
  }, USE.NAMES = FALSE)

  cbind(names, desc) %>% as_tibble() %>% arrange(names)

  # length <- apply(sapply(x$runs, function(r) sapply(r, length)), 1, unique)
  # class <- apply(sapply(x$runs, function(r) sapply(r, class)), 1, unique)
  # have_all <- apply(sapply(x$runs, function(r) sapply(r, is.null)), 1, any)

  # TODO: add available variables (including global), their type and length
  # names, desc, class, length, have_all
  # sort by names
  # as tibble

}

#' Summary of initialization runs
#'
#' @description
#' This function gives an overview of the initialization runs in an \code{ino}
#' object.
#'
#' @details
#' The following variables are available for each \code{ino} object:
#' \describe{
#'   \item{.strategy}{the name of the initialization strategy}
#'   \item{.time}{the optimization time}
#'   \item{.optimum}{the function value at the optimum}
#'   \item{.optimizer}{the identifier of the optimizer}
#' }
#'
#' @param object
#' An object of class \code{ino}.
#' @param ...
# TODO: Write documentation
#' Named function for computing statistics.
#' See function ... for an overview.
#'
#' @return
#' A \code{tibble}, optimization runs as rows and variables as columns.
#'
#' @keywords evaluation
#'
#' @importFrom dplyr %>% bind_rows
#'
#' @exportS3Method

summary.ino <- function(object, ...) {
  if (nruns(object) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
  }
  vars <- c(".strategy",".time",".optimum",".optimizer")
  out <- lapply(object$runs, `[`, vars) %>% dplyr::bind_rows()
  add_vars <- list(...)
  for(i in seq_along(add_vars)) {
    out[[names(add_vars)[i]]] <- sapply(object$runs, function(r) {
      env <- new.env()
      env$.global <- object$prob$global
      list2env(r, env)
      tryCatch(
        expr = {
          out <- eval(parse(text = add_vars[[i]]), env)
          stopifnot(length(out) == 1)
          out
        },
        error = function(cond) NA
      )
    })
  }
  out
}

#' @exportS3Method
#' @noRd
#' @keywords internal

plot.ino <- function(x, ...) {
  # TODO: Add code from Rprobit project (relative comparison)
  .Defunct(msg = "Plot method currently not available.")
}

#' Optima overview
#'
#' @description
#' This function provides an overview of the identified optima.
#'
#' @param x
#' An object of class \code{ino}.
#' @param digits
#' The number of decimal places of the optima values. The default is \code{2}.
#'
#' @return
#' A data frame with columns
#' \describe{
#'   \item{optimum}{the unique optima (with respect to \code{digits})}
#'   \item{frequency}{the number of runs the optima was reached}
#' }
#'
#' @export
#'
#' @keywords evaluation

overview_optima <- function(x, digits = 2) {
  if (nruns(x) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  # TODO: sort by frequency
  structure(
    data.frame(table(round(sapply(x$runs, `[[`, ".optimum"), digits = digits))),
    names = c("optimum", "frequency")
  )
}
