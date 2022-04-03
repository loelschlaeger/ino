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
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
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
  (1.5 - x[1] + x[1] * x[2])^2 + (2.25 - x[1] + x[1] * x[2]^2)^2 +
    (2.625 - x[1] + x[1] * x[2]^3)^2
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
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @examples
#' data(earthquakes, package = "ino")
#' ino:::f_ll_hmm(theta = c(-1, -1, 1, 2), data = earthquakes, N = 2)
#'
#' @importFrom stats dpois
#'
#' @return
#' The log-likelihood value.
#'
#' @keywords
#' function

f_ll_hmm <- function(theta, data, N = 2, neg = FALSE) {

  ### input checks
  stopifnot(is.numeric(theta), is.data.frame(data), N%%1==0)

  ### transition probability matrix (t.p.m.)
  tpm <- diag(N)
  tpm[!tpm] <- exp(theta[1:(N * (N - 1))])
  tpm <- tpm/rowSums(tpm)

  ### lambda for each state
  lambda <- theta[(N * (N - 1) + 1):(N * (N - 1) + N)]
  delta <- try(solve(t(diag(N) - tpm + 1), rep(1, N)), silent = TRUE)
  if ("try-error" %in% class(delta)) delta <- rep(1, N) / N

  ### allprobs matrix
  allprobs <- matrix(1, nrow(data), N)
  for(j in 1:N){
    allprobs[, j] <- dpois(data$obs, exp(lambda[j]))
  }

  ### forward algorithm
  foo <- delta %*% diag(allprobs[1,])
  llk <- log(sum(foo))
  phi <- foo/sum(foo)
  for(t in 2:nrow(data)){
    foo <- phi %*% tpm %*% diag(allprobs[t, ])
    llk <- llk + log(sum(foo))
    phi <- foo/sum(foo)
  }

  return(ifelse(neg, -llk, llk))
}

#' The log-likelihood function of the normally mixed multinomial probit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_probit
#'
#' @seealso
#' [sim_mmnp()]
#'
#' @details
#' The order of \code{theta} is supposed to be \code{c(b,o,l)}, where
#' \itemize{
#'   \item \code{b} denotes the coefficients,
#'   \item \code{o} the lower-triangular elements of the lower-triangular
#'         Cholesky root of \code{Omega},
#'   \item and \code{l} the lower-triangular elements of the lower-triangular
#'         Cholesky root \code{L} of the differenced (with respect to
#'         alternative 1) error term covariance matrix \code{Sigma}, with the
#'         top-left element of \code{L} fixed to 1.
#' }
#'
#' @param theta
#' The vector of model coefficients in order \code{c(b,o,l)}, see the details.
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
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' N <- 100
#' T <- 10
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' Sigma <- diag(3)
#' data <- ino:::sim_mmnp(N, T, b, Omega, Sigma, seed = 1)
#' true <- attr(data, "true")
#' ino:::f_ll_mmnp(theta = true, data = data)
#'
#' @keywords
#' function
#'
#' @importFrom RprobitB delta
#' @importFrom mvtnorm pmvnorm

f_ll_mmnp <- function(theta, data, normal_cdf = mvtnorm::pmvnorm, neg = FALSE) {

  ### model parameters
  N <- length(data$y)
  T <- length(data$y[[1]])
  P <- ncol(data$X[[1]][[1]])
  J <- nrow(data$X[[1]][[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  o <- theta[1:(P*(P+1)/2)]; theta <- theta[-(1:(P*(P+1)/2))]
  l <- theta
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- o
  Omega <- O %*% t(O)
  L_diff <- matrix(0, J-1, J-1)
  L_diff[lower.tri(L_diff, diag = TRUE)] <- c(sqrt(2),l)
  L <- cbind(0, rbind(0, L_diff))
  Sigma <- L %*% t(L)
  delta <- RprobitB::delta

  ### log-likelihood contributions
  LL <- 0
  for(n in 1:N){
    for(t in 1:T){
      i <- data$y[[n]][t]
      X <- data$X[[n]][[t]]
      arg <- as.vector(-delta(J,i) %*% X %*% b)
      Gamma <- delta(J,i) %*% ( X %*% Omega %*% t(X) + Sigma ) %*% t(delta(J,i))
      p <- do.call(what = normal_cdf,
                   args = list(lower = -Inf, upper = arg, mean = rep(0,J-1),
                               sigma = Gamma))
      LL <- LL + log(p)
    }
  }

  return(ifelse(neg, -LL, LL))
}

#' Simulate data from a normally mixed multinomial probit panel model
#'
#' @details
#' The normally mixed multinomial probit model formula is
#' \deqn{U_{nt} = X_{nt} \beta_n + \eps_{nt}},
#' where \eqn{U_{nt}} is the vector of length \code{J} of utilities for each
#' of the \eqn{J} alternatives for decider \eqn{n} at choice occasion \eqn{t},
#' \eqn{X_{nt}} is the choice occasion-specific \eqn{J} times \eqn{P} covariate
#' matrix, \eqn{\beta_n} is the decider-specific random coefficient vector with
#' \deqn{beta_n\sim\text{MVN}_P(b,\Omega)}, and \eqn{\eps_{nt}} is the choice
#' occasion-specific error term with \deqn{\eps_{nt}\sim\text{MVN}_J(0,\Sigma)}.
#'
#' Note that because utility is indifferent in terms of level and scale, not all
#' elements of \eqn{Sigma} are identified. Therefore, we take utility
#' differences with respect to the first alternative and fix the first variance
#' of the differenced error term matrix.
#'
#' @seealso
#' [f_ll_mmnp()]
#'
#' @param N
#' The number of observations.
#' @param T
#' The number of choice occasions.
#' @param b
#' The vector of coefficients.
#' @param Omega
#' The covariance matrix of the normal mixing distribution.
#' @param Sigma
#' The error term covariance matrix. The top-left element is normalized to 1.
#' @param seed
#' Set a seed for the simulation.
#'
#' @return
#' A list with choices \code{y} and covariate matrices \code{X}, each of
#' which containing sub-lists for the choice occasions.
#' The true model coefficients are added via the attribute \code{"true"}.
#' They are already normalized and can be directly compared with the maximum
#' likelihood estimate.
#'
#' @examples
#' N <- 100
#' T <- 10
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' Sigma <- diag(3)
#' data <- ino:::sim_mmnp(N, T, b, Omega, Sigma, seed = 1)
#'
#' @importFrom stats rnorm

sim_mmnp <- function(N, T, b, Omega, Sigma, seed = NULL) {

  ### input checks
  stopifnot(N%%1 == 0, N > 0, length(N) == 1)
  stopifnot(T%%1 == 0, T > 0, length(T) == 1)
  stopifnot(length(b) == ncol(Omega))
  stopifnot(ncol(Omega) == nrow(Omega))
  stopifnot(ncol(Sigma) == nrow(Sigma))
  if(!is.null(seed)) set.seed(seed)

  ### model parameters
  P <- length(b)
  J <- ncol(Sigma)
  O <- t(chol(Omega))
  o <- O[lower.tri(O, diag = TRUE)]
  L <- t(chol(Sigma))

  ### data simulation
  X <- list()
  y <- list()
  for (n in 1:N){
    Xn <- list()
    yn <- numeric(T)
    for(t in 1:T){
      Xn[[t]] <- matrix(rnorm(J*P,sd=3),nrow=J,ncol=P)
      beta <- b + O %*% rnorm(P)
      V <- Xn[[t]] %*% beta
      e <- L %*% rnorm(J)
      U <- V + e
      yn[t] <- which.max(U)
    }
    X[[n]] <- Xn
    y[[n]] <- yn
  }

  ### parameter normalization
  delta <- RprobitB::delta(J,1)
  Sigma_diff <- delta %*% Sigma %*% t(delta)
  L_diff <- t(chol(Sigma_diff))
  l_diff <- L_diff[lower.tri(L_diff, diag = TRUE)]

  out <- list(y=y, X=X)
  attr(out, "true") <- c(b,o,l_diff[-1])
  return(out)
}

#' The log-likelihood function of the normally mixed multinomial logit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_logistic_regression
#'
#' @seealso
#' [sim_mmnl()]
#'
#' @details
#' The order of \code{theta} is supposed to be \code{c(b,o)}, where
#' \itemize{
#'   \item \code{b} denotes the coefficients
#'   \item and \code{o} the lower-triangular elements of the lower-triangular
#'         Cholesky root of \code{Omega}.
#' }
#'
#' @param theta
#' The vector of model coefficients in order \code{c(b,o)}, see the details.
#' @param data
#' The output of \code{\link{sim_mmnl}}.
#' @param R
#' The number of random draws to approximate the integral for the mixed logit
#' choice probabilities.
#' @param neg
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
#' data <- ino:::sim_mmnl(N, J, b, Omega, seed = 1)
#' true <- attr(data, "true")
#' ino:::f_ll_mmnl(theta = true, data = data)
#'
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats rnorm
#'
#' @keywords
#' function

f_ll_mmnl <- function(theta, data, R = 100, neg = FALSE) {

  ### model parameters
  P <- ncol(data$X[[1]])
  J <- nrow(data$X[[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- theta
  N <- length(data$y)

  ### log-likelihood contributions
  LL <- 0
  for(n in 1:N){
    i <- data$y[n]
    X <- data$X[[n]]
    L_ni <- function(beta) exp(X[i,]%*%beta) / sum(exp(X%*%beta))
    p <- mean(replicate(R, L_ni(beta = b + O %*% rnorm(P))))
    LL <- LL + log(p)
  }

  return(ifelse(neg, -LL, LL))
}

#' Simulate data from a normally mixed multinomial logit model
#'
#' @details
#' The normally mixed multinomial logit model formula is
#' \deqn{U_n = X_n \beta_n + \eps_{n}},
#' where \eqn{U_n} is the vector of length \code{J} of utilities for each
#' of the \eqn{J} alternatives for decider \eqn{n},
#' \eqn{X_{n}} is the decider-specific \eqn{J} times \eqn{P} covariate
#' matrix, \eqn{\beta_n} is the decider-specific random coefficient vector with
#' \deqn{beta_n\sim\text{MVN}_P(b,\Omega)}, and \eqn{\eps_{n}} is the
#' decider-specific error term that is iid extreme value distributed.
#'
#' @seealso
#' [f_ll_mmnl()]
#'
#' @param N
#' The number of observations.
#' @param J
#' The number of alternatives.
#' @param b
#' The vector of coefficients.
#' @param Omega
#' The covariance matrix of the normal mixing distribution.
#' @param seed
#' Set a seed for the simulation.
#'
#' @return
#' A list with choices \code{y} and covariate matrices \code{X}.
#' The true model coefficients are added via the attribute \code{"true"}.
#'
#' @examples
#' N <- 100
#' J <- 3
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' data <- ino:::sim_mmnl(N, J, b, Omega, seed = 1)
#'
#' @importFrom evd rgumbel
#' @importFrom stats rnorm

sim_mmnl <- function(N, J, b, Omega, seed = NULL) {

  ### input checks
  stopifnot(N%%1 == 0, N > 0, length(N) == 1)
  stopifnot(J%%1 == 0, J > 2, length(J) == 1)
  stopifnot(length(b) == ncol(Omega))
  stopifnot(ncol(Omega) == nrow(Omega))
  if(!is.null(seed)) set.seed(seed)

  ### model parameters
  P <- length(b)
  O <- t(chol(Omega))
  o <- O[lower.tri(O, diag = TRUE)]

  ### data simulation
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

  out <- list(y=y, X=X)
  attr(out, "true") <- c(b,o)
  return(out)
}
