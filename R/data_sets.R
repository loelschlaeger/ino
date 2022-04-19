#' Earthquake data
#'
#' @description
#' This data set includes the number of yearly measured earthquakes from 1900
#' to 2006.
#'
#' @docType data
#'
#' @usage data(earthquakes)
#'
#' @format
#' The data set is a data frame with two integer columns, \code{year} for the
#' year and \code{obs} for the number of measured earthquakes.
#'
#' @source
#' The data was obtained from
#' \url{http://hmms-for-time-series.de/second/data/earthquakes.txt}
#' on 2022-03-25.
#'
#' @keywords
#' dataset
#'
"earthquakes"

#' Simulated choice data from a mixed probit model
#'
#' @description
#' This data set includes simulated choices and corresponding choice
#' characteristics from a (normally) mixed probit model.
#'
#' @details
#' Missing
#' \preformatted{
#' NA
#' }
#'
#' @docType data
#'
#' @usage data(probit_data)
#'
#' @format
#' Missing
#'
#' @keywords
#' dataset
#'
"probit_data"

#' Simulated choice data from a mixed logit model
#'
#' @description
#' This object is a list which includes 100 data sets of simulated choices and
#' corresponding choice characteristics from a (normally) mixed logit model.
#'
#' @details
#' The data set was simulated via the following lines:
#' \preformatted{
#'   set.seed(1)
#'   logit_data <- list()
#'   for(i in 1:100){
#'     b <- rnorm(3, sd = 9)
#'     Omega <- RprobitB::rwishart(3,diag(3))$W
#'     name <- paste0("data",i)
#'     logit_data[[name]] <- ino:::sim_mmnl(N = 100, J = 3, b, Omega, seed = i)
#'   }
#' }
#'
#' @docType data
#'
#' @usage data(logit_data)
#'
#' @format
#' Missing
#'
#' @keywords
#' dataset
#'
"logit_data"
