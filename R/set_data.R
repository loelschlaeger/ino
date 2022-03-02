#' Set data for the numerical optimization of likelihood functions
#'
#' @param x
#' Either \code{NULL} or an object of class \code{ino}.
#' @param data
#' A list of data sets.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @export
#'
#' @keywords
#' specification

set_data <- function(x, data) {

  if (class(data) != "list") {
    stop("'data' must be of class list.")
  }

  if (is.null(x)) {
    ### initialize ino object
    x <- new_ino()
    x[["data"]] <- data
  } else if (identical(x[["data"]], NA)) {
    ### add data first time
    x[["data"]] <- data
  } else if (!identical(x[["data"]], NA)){
    ### add data again
    cat("The ino object already contains data, what to do?\n")
    cat("1: Cancel\n")
    cat("2: Replace the old data by the new data\n")
    cat("3: Add the new data\n")
    cat("\n")
    input <- readline(prompt = "Action: ")
    if(input == 1){
      return(x)
    } else if (input == 2) {
      x[["data"]] <- data
    } else if (input == 3) {
      x[["data"]] <- append(data, x[["data"]])
    }
  }

  return(x)
}
