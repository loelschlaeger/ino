#' Variable names
#'
#' @description
#' This function returns the names of the available variables in an \code{ino}
#' object.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' A character vector.
#'
#' @export
#'
#' @keywords
#' evaluation

var_names <- function(x) {
  if (nruns(x) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  c(unique(unlist(lapply(x$runs, names))), if (!is.null(x$prob$global)) ".global")
}

#' Get variables
#'
#' @description
#' This function extracts available variables from an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param runs
#' An integer vector, specifying the optimization runs of interest. Can be
#' \code{NULL} (default), in which case all runs are considered.
#' @param vars
#' A character vector, specifying the variables of interest. Can be
#' \code{NULL} (default), in which case all variables are considered.
#'
#' @return
#' A list, each element is a list of variables of an optimization run.
#'
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @keywords
#' evaluation

get_vars <- function(x, runs = NULL, vars = NULL) {
  if (is.null(runs)) runs <- seq_len(nruns(x))
  if (is.null(vars)) vars <- var_names(x)
  lapply(x$runs[runs], append, list(".global" = x$prob$global)) %>%
    lapply(`[`, vars)
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
#' Expressions of variables from \code{\link{var_names}} as characters.
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
    ino_stop(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
  }
  vars <- c(".strategy", ".time", ".optimum", ".optimizer")
  out <- lapply(object$runs, `[`, vars) %>% dplyr::bind_rows()
  add_vars <- list(...)
  for (i in seq_along(add_vars)) {
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

#' Visualization of optimization time
#'
#' @description
#' This function plots boxplots of optimization times in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param by
#' A character vector of variables to group by. Can be \code{NULL} (default).
#' @param ...
#' Ignored.
#'
#' @return
#' A \code{ggplot} object.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes scale_y_continuous geom_boxplot facet_wrap
#' theme element_blank ylab
#' @importFrom rlang .data
#'
#' @keywords
#' evaluation

plot.ino <- function(x, by = NULL, ...) {
  summary(x) %>% ggplot(aes(x = "", y = .data$.time)) +
    scale_y_continuous() +
    geom_boxplot() +
    {
      if (!is.null(by)) facet_wrap(by, labeller = "label_both")
    } +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("optimization time")
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
#' @importFrom dplyr %>% arrange desc
#' @importFrom rlang .data
#'
#' @keywords evaluation

overview_optima <- function(x, digits = 2) {
  if (nruns(x) == 0) {
    ino_stop(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
  }
  structure(
    lapply(x$runs, `[[`, ".optimum") %>%
      unlist() %>%
      round(digits = digits) %>%
      table() %>%
      as.data.frame(),
    names = c("optimum", "frequency")
  ) %>%
    arrange(desc(.data$frequency))
}
