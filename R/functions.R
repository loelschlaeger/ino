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

#' The log-likelihood function of the Poisson-hidden Markov model
#'
#' @references
#' https://en.wikipedia.org/wiki/Hidden_Markov_model
#'
#' @details
#' The example uses a data set that covers the number of major earthquakes
#' (magnitude 7 or greater) in the world from 1900 until 2006.
#'
#' @param theta
#' A numeric vector of model parameters.
#' @param data
#' A data frame that includes a time series of counts.
#' @param N
#' The number of states in the hidden Markov model.
#' @param negative
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @examples
#' data <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")
#' colnames(data) <- c("year", "obs")
#' theta <- c(-1, -1, 1, 2)
#' f_ll_hmm(theta = theta, data = data, N = 2)
#'
#' @importFrom stats dpois
#'
#' @return
#' The log-likelihood value.
#'
#' @keywords
#' function

f_ll_hmm <- function(theta, data, N = 2, negative = FALSE) {
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

  return(ifelse(negative, -llk, llk))
}

#' The log-likelihood function of the normally mixed multinomial probit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_probit
#'
#' @seealso
#' [sim_mmnp()]
#'
#' @param theta
#' A vector of model coefficients in the order \code{b}, \code{o}, \code{l}.
#' @param data
#' The output of \code{\link{sim_mmnp}}.
#' @param normal_cdf
#' A function that evaluates the CDF of a multivariate normal distribution. It
#' must take the arguments
#' \itemize{
#'   \item \code{lower}, the vector of lower limits of length \code{n},
#'   \item \code{upper}, the vector of upper limits of length \code{n},
#'   \item \code{mean}, the mean vector of length \code{n},
#'   \item \code{sigma}, the covariance matrix of dimension \code{n},
#' }
#' and return a single numeric value.
#' @param negative
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' N <- 100
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' Sigma <- diag(3)
#' L <- t(chol(Sigma))
#' l <- L[lower.tri(L, diag = TRUE)][-1]
#' data <- sim_mmnp(N, b, o, l, seed = 1)
#' f_ll_mmnp(theta = c(b,o,l), data = data)
#'
#' @keywords
#' function

f_ll_mmnp <- function(theta, data, normal_cdf = mvtnorm::pmvnorm, negative = FALSE) {
  P <- ncol(data$X[[1]])
  J <- nrow(data$X[[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  o <- theta[1:(P*(P+1)/2)]; theta <- theta[-(1:(P*(P+1)/2))]
  l <- theta
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- o
  Omega <- O %*% t(O)
  L <- matrix(0, J, J)
  L[lower.tri(L, diag = TRUE)] <- c(1,l)
  Sigma <- L %*% t(L)
  N <- length(data$y)
  delta <- RprobitB:::delta
  LL <- 0
  for(n in 1:N){
    i <- data$y[n]
    X <- data$X[[n]]
    arg <- as.vector(-delta(J,i) %*% X %*% b)
    Gamma <- delta(J,i) %*% ( X %*% Omega %*% t(X) + Sigma ) %*% t(delta(J,i))
    p <- do.call(what = normal_cdf,
                 args = list(lower = -Inf, upper = arg, mean = rep(0,J-1),
                             sigma = Gamma))
    LL <- LL + log(p)
  }
  return(ifelse(negative, -LL, LL))
}

#' Simulate data from a normally mixed multinomial probit model
#'
#' @param N
#' The number of observations.
#' @param b
#' The vector of linear coefficients.
#' @param o
#' The vector of elements of the lower Cholesky root of the covariance matrix
#' of the normal mixing distribution.
#' @param l
#' The vector of elements of the lower Cholesky root of the error term
#' covariance matrix, except for the first element which is fixed to 1 for
#' identification.
#' @param seed
#' Set a seed for the simulation.
#'
#' @return
#' A list with choices \code{y} and covariate matrices \code{X}.
#'
#' @examples
#' N <- 100
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' Sigma <- diag(3)
#' L <- t(chol(Sigma))
#' l <- L[lower.tri(L, diag = TRUE)][-1]
#' data <- sim_mmnp(N, b, o, l, seed = 1)

sim_mmnp <- function(N, b, o, l, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  P <- sqrt(0.25 + 2*length(o)) - 0.5
  J <- sqrt(0.25 + 2*(length(l) + 1)) - 0.5
  stopifnot(J%%1 == 0, P %%1 == 0)
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- o
  L <- matrix(0, J, J)
  L[lower.tri(L, diag = TRUE)] <- c(1,l)
  stopifnot(length(b) == nrow(O))
  X <- list()
  y <- numeric(N)
  for (n in 1:N){
    X[[n]] <- matrix(rnorm(J*P,sd=3),nrow=J,ncol=P)
    beta <- b + O %*% rnorm(P)
    V <- X[[n]] %*% beta
    e <- L %*% rnorm(J)
    U <- V + e
    y[n] <- which.max(U)
  }
  return(list(y=y, X=X))
}

#' The log-likelihood function of the normally mixed multinomial logit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_logistic_regression
#'
#' @seealso
#' [sim_mmnl()]
#'
#' @param theta
#' A numeric vector of model coefficients in the order \code{b}, \code{o}.
#' @param data
#' The output of \code{\link{sim_mmnl}}.
#' @param R
#' The number of random draws.
#' @param negative
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' N <- 10
#' J <- 3
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' data <- sim_mmnl(N, J, b, o, seed = 1)
#' f_ll_mmnl(theta = c(b,o), data = data)
#'
#' @importFrom mvtnorm pmvnorm
#'
#' @keywords
#' function

f_ll_mmnl <- function(theta, data, R = 100, negative = FALSE) {
  P <- ncol(data$X[[1]])
  J <- nrow(data$X[[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- theta
  N <- length(data$y)
  LL <- 0
  for(n in 1:N){
    i <- data$y[n]
    X <- data$X[[n]]
    L_ni <- function(beta) exp(X[i,]%*%beta) / sum(exp(X%*%beta))
    p <- mean(replicate(R, L_ni(beta = b + O %*% rnorm(P))))
    LL <- LL + log(p)
  }
  return(ifelse(negative, -LL, LL))
}

#' Simulate data from a normally mixed multinomial logit model
#'
#' @param N
#' The number of observations.
#' @param J
#' The number of alternatives.
#' @param b
#' The vector of linear coefficients.
#' @param o
#' The vector of elements of the lower Cholesky root of the covariance matrix
#' of the normal mixing distribution.
#' @param seed
#' Set a seed for the simulation.
#'
#' @return
#' A list with choices \code{y} and covariate matrices \code{X}.
#'
#' @examples
#' N <- 100
#' J <- 3
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' data <- sim_mmnl(N, J, b, o, seed = 1)
#'
#' @importFrom evd rgumbel

sim_mmnl <- function(N, J, b, o, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  P <- sqrt(0.25 + 2*length(o)) - 0.5
  stopifnot(J%%1 == 0, P %%1 == 0)
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- o
  stopifnot(length(b) == nrow(O))
  X <- list()
  y <- numeric(N)
  for (n in 1:N){
    X[[n]] <- matrix(rnorm(J*P,sd=3),nrow=J,ncol=P)
    beta <- b + O %*% rnorm(P)
    V <- X[[n]] %*% beta
    e <- evd::rgumbel(J,loc=0,scale=1)
    U <- V + e
    y[n] <- which.max(U)
  }
  return(list(y=y, X=X))
}
