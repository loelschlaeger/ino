#' The Ackley function
#'
#' @references
#' https://en.wikipedia.org/wiki/Ackley_function
#'
#' @details
#' The function has multiple local minima and one global minimum in the origin.
#'
#' @param x
#' A numeric vector of length 2.
#'
#' @examples
#' ino:::f_ackley(c(0, 0))
#'
#' @return
#' A numeric value.
#'
#' @keywords
#' function

f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) - exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}


#' The Beale function
#'
#' @references
#' https://en.wikipedia.org/wiki/Test_functions_for_optimization
#'
#' @details
#' The function has multiple local minima and one global minimum in (3, 0.5).
#'
#' @param x
#' A numeric vector of length 2.
#'
#' @examples
#' ino:::f_beale(c(3, 0.5))
#'
#' @return
#' A numeric value.
#'
#' @keywords
#' function

f_beale <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  (1.5 - x[1] + x[1] * x[2])^2 + (2.25 - x[1] + x[1] * x[2]^2)^2 + (2.625 - x[1] + x[1] * x[2]^3)^2
}


#' The Matyas function
#'
#' @references
#' https://en.wikipedia.org/wiki/Test_functions_for_optimization
#'
#' @details
#' The function has multiple local minima and one global minimum in the origin.
#'
#' @param x
#' A numeric vector of length 2.
#'
#' @examples
#' ino:::f_matyas(c(0, 0))
#'
#' @return
#' A numeric value.
#'
#' @keywords
#' function

f_matyas <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  0.26 * (x[1]^2 +  x[2]^2) - 0.48 * x[1] * x[2]
}


#' The Easom function
#'
#' @references
#' https://en.wikipedia.org/wiki/Test_functions_for_optimization
#'
#' @details
#' The function has multiple local minima and one global minimum in (pi, pi).
#'
#' @param x
#' A numeric vector of length 2.
#'
#' @examples
#' ino:::f_easom(c(pi, pi))
#'
#' @return
#' A numeric value.
#'
#' @keywords
#' function

f_easom <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}

#' The log-likelihood function of the mixed probit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Probit_model
#'
#' @details
#' The {ino} package includes a data set that was sampled from a mixed probit
#' model, which can be accessed via \code{data()}.
#'
#' @param theta
#' A numeric vector of model parameters.
#' @param data
#' A data frame of discrete choice data.
#'
#' @examples
#' b <- c(-1,1)
#' Omega <- diag(2)
#' Sigma <- diag(2)
#' O <- t(chol(Omega))
#' L <- t(chol(Sigma))
#' theta <- c(b, O[lower.tri(O)], L[lower.tri(L)])
#'
#' @return
#' A log-likelihood value.
#'
#' @keywords
#' function

f_ll_mmnp <- function(theta, data) {
  stopifnot(is.numeric(theta), is.data.frame(data), "choice" %in% colnames(data))
  warning("Not implemented yet.")
}

#' The log-likelihood function of a Poisson-hidden Markov model
#'
#' @references
#' https://en.wikipedia.org/wiki/Hidden_Markov_model
#'
#' @details
#' To illustrate the numerical optimisation of the log-likelihood of a Poisson-hidden Markov model,
#' the {ino} package uses a data set cover the number of major earthquakes (magnitude 7 or greater)
#' in the world from 1900 until 2006.
#'
#' @param theta
#' A numeric vector of model parameters.
#' @param data
#' A data frame that includes a time series of counts.
#' @param N
#' The number of states in the hidden Markov model.
#'
#' @examples
#' # f_ll_hmm()
#'
#' @importFrom stats dpois
#'
#' @return
#' A log-likelihood value.
#'
#' @keywords
#' function

f_ll_hmm <- function(theta, data, N = 2) {
  stopifnot(is.numeric(theta), is.data.frame(data))

  ## transition probability matrix (t.p.m.)
  tpm <- diag(N)
  tpm[!tpm] <- exp(theta[1:(N * (N - 1))])
  tpm <- tpm/rowSums(tpm)

  ## lambda for each state
  lambda <- theta[(N * (N - 1) + 1):(N * (N - 1) + N)]
  delta <- try(solve(t(diag(N) - tpm + 1), rep(1, N)), silent = TRUE)
  if ("try-error" %in% class(delta)) delta <- rep(1, N) / N

  ## allprobs matrix
  allprobs <- matrix(1, nrow(data), N)
  for(j in 1:N){
    allprobs[, j] <- dpois(data$obs, exp(lambda[j]))
  }

  foo <- delta %*% diag(allprobs[1,])
  llk <- log(sum(foo))
  phi <- foo/sum(foo)
  ## forward algorithm
  for(t in 2:nrow(data)){
    foo <- phi %*% tpm %*% diag(allprobs[t, ])
    llk <- llk + log(sum(foo))
    phi <- foo/sum(foo)
  }

  return(-llk)
}
