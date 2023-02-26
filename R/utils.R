#' Check for proper number
#'
#' @description
#' This function checks whether the input is proper number, i.e., a single,
#' positive \code{integer}.
#'
#' @param x
#' Any object.
#'
#' @return
#' Either \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils
#'
#' @examples
#' \dontrun{
#' is_number(1)
#' is_number(pi)
#' is_number("1")
#' }

is_number <- function(x) {
  is.vector(x) && is.numeric(x) && length(x) == 1 && x > 0 && x %% 1 == 0
}

#' Check for proper name
#'
#' @description
#' This function checks whether the input is a proper name, i.e., a single
#' (non-trivial) \code{character}.
#'
#' @param x
#' Any object.
#'
#' @return
#' Either \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils
#'
#' @examples
#' \dontrun{
#' is_name("one")
#' is_name(1)
#' is_name(LETTERS[1:2])
#' is_name("")
#' }

is_name <- function(x) {
  is.vector(x) && is.character(x) && length(x) == 1 && nchar(x) > 0
}
