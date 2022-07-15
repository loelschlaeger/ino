#' Ackley function
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
#' f_ackley(c(0, 0))
#'
#' @return
#' The function value at \code{x}, a single numeric value.
#'
#' @keywords
#' internal function
#'
#' @export

f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}

#' Beale function
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
#' f_beale(c(3, 0.5))
#'
#' @return
#' The function value at \code{x}, a single numeric value.
#'
#' @keywords
#' internal function
#'
#' @export

f_beale <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  (1.5 - x[1] + x[1] * x[2])^2 + (2.25 - x[1] + x[1] * x[2]^2)^2 +
    (2.625 - x[1] + x[1] * x[2]^3)^2
}

#' Matyas function
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
#' f_matyas(c(0, 0))
#'
#' @return
#' The function value at \code{x}, a single numeric value.
#'
#' @export
#'
#' @keywords
#' internal function

f_matyas <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  0.26 * (x[1]^2 +  x[2]^2) - 0.48 * x[1] * x[2]
}

#' Easom function
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
#' f_easom(c(pi, pi))
#'
#' @return
#' The function value at \code{x}, a single numeric value.
#'
#' @keywords
#' internal function
#'
#' @export

f_easom <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}

#' Log-likelihood function of the Poisson-hidden Markov model
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
#' A \code{data.frame} that includes a time series of counts.
#' @param N
#' The number of states in the hidden Markov model.
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @examples
#' data(earthquakes, package = "ino")
#' f_ll_hmm(theta = c(-1, -1, 1, 2), data = earthquakes, N = 2)
#'
#' @importFrom stats dpois
#'
#' @return
#' The log-likelihood value at \code{theta}.
#'
#' @keywords
#' internal function
#'
#' @export

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

#' Log-likelihood function of the (normally mixed) multinomial probit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_probit
#'
#' @seealso
#' [sim_mnp()] for simulating a data set from a probit model.
#'
#' @details
#' The order of \code{theta} is supposed to be \code{c(b,o,l)}, where
#' \itemize{
#'   \item \code{b} denotes the vector of mean effects without the first one,
#'   \item \code{o} the lower-triangular elements of the lower-triangular
#'         Cholesky root of the effect covariance matrix \code{Omega} (if any),
#'   \item and \code{l} the lower-triangular elements of the lower-triangular
#'         Cholesky root \code{L} of the differenced (with respect to
#'         alternative 1) error term covariance matrix \code{Sigma}.
#' }
#'
#' @param theta
#' The vector of model coefficients in order \code{c(b,o,l)}, see the details.
#' @param data
#' A \code{data.frame} with the shape of the output of \code{\link{sim_mnp}}.
#' @param normal_cdf
#' A function that evaluates the CDF of an \code{n}-variate normal distribution.
#' It must take the arguments
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
#' The log-likelihood value at \code{theta}.
#'
#' @examples
#' data <- sim_mnp(N = 300, b = c(1,3), Sigma = diag(2), seed = 1)
#' theta <- attr(data, "true")
#' f_ll_mnp(theta = theta, data = data)
#' \donttest{
#' nlm(f_ll_mnp, p = theta, data = data, neg = TRUE)$estimate
#' }
#'
#' @keywords
#' internal function
#'
#' @export
#'
#' @importFrom RprobitB delta
#' @importFrom mvtnorm pmvnorm

f_ll_mnp <- function(theta, data, normal_cdf = mvtnorm::pmvnorm, neg = FALSE) {

  ### check inputs
  stopifnot(c("N","T","P","J","mix") %in% names(attributes(data)))

  ### model parameters
  N <- attr(data, "N")
  T <- attr(data, "T")
  P <- attr(data, "P")
  J <- attr(data, "J")
  mix <- attr(data, "mix")
  b <- 1
  if(P > 1) {
    b <- c(b, theta[1:(P-1)])
    theta <- theta[-(1:(P-1))]
  }
  if(mix) {
    o <- theta[1:(P*(P+1)/2)]; theta <- theta[-(1:(P*(P+1)/2))]
  }
  l <- theta
  if(mix) {
    O <- matrix(0, P, P)
    O[lower.tri(O, diag = TRUE)] <- o
    Omega <- O %*% t(O)
  }
  L_diff <- matrix(0, J-1, J-1)
  L_diff[lower.tri(L_diff, diag = TRUE)] <- l
  L <- cbind(0, rbind(0, L_diff))
  Sigma <- L %*% t(L)
  delta <- RprobitB::delta

  ### log-likelihood contributions
  LL <- 0
  ind <- 0
  for(n in 1:N) for(t in 1:T){
    ind <- ind + 1
    i <- data[ind,"y"]
    X <- matrix(as.numeric(data[ind,-(1:3)]), nrow = J, ncol = P)
    arg <- as.vector(-delta(J,i) %*% X %*% b)
    if(mix) {
      Gamma <- delta(J,i) %*% ( X %*% Omega %*% t(X) + Sigma ) %*% t(delta(J,i))
    } else {
      Gamma <- delta(J,i) %*% Sigma %*% t(delta(J,i))
    }
    p <- as.numeric(
      do.call(what = normal_cdf,
              args = list(lower = rep(-Inf,J-1), upper = arg, mean = rep(0,J-1),
                          sigma = Gamma)))
    LL <- LL + log(p)
  }

  return(ifelse(neg, -LL, LL))
}

#' Simulate data from a (normally mixed) multinomial probit model
#'
#' @seealso
#' [f_ll_mnp()] for computing the log-likelihood of a (normally mixed)
#' multinomial probit model.
#'
#' @param N
#' The number of observations.
#' @param T
#' The number of choice occasions with \code{T = 1} per default.
#' @param b
#' The vector of mean effects. The first element will be set to one.
#' @param Omega
#' The covariance matrix of the normal mixing distribution. Set to \code{NULL}
#' (the default) for no mixing distribution.
#' @param Sigma
#' The error term covariance matrix.
#' @param seed
#' Set a seed for the simulation.
#' @param covariate
#' A function that samples the covariates. It must return a single numeric
#' (random) value and must not require arguments. Per default,
#' \code{covariate = function() rnorm(n = 1, mean = 0, sd = 9)}, i.e. covariates
#' are sampled from a normal distribution with mean 0 and standard deviation 9.
#'
#' @return
#' A \code{data.frame}. The first column (\code{n}) is the identifier for the
#' decider, the next column (\code{t}) the identifier for the choice occasion.
#' Next comes the column \code{y} with the indices of the chosen alternatives.
#' The last columns contain the column-wise entries of \eqn{X_{nt}}.
#'
#' The true model coefficients are added to the output via the attribute
#' \code{"true"}. They are already normalized and can be directly compared with
#' the maximum likelihood estimate.
#'
#' Additional attributes are \code{"N"} (the number of deciders), \code{"T"}
#' (the number of choice occasions), \code{"J"} (the number of alternatives),
#' \code{"P"} (the number of choice covariates), and \code{"mix"} (a boolean
#' which is \code{TRUE} if \code{Omega} is not \code{NULL}).
#'
#' @examples
#' sim_mnp(N = 3, T = 2, b = c(1,-1), Omega = diag(2), Sigma = diag(2))
#'
#' @importFrom stats rnorm
#'
#' @keywords
#' internal function
#'
#' @export

sim_mnp <- function(
  N, T = 1, b, Omega = NULL, Sigma, seed = NULL,
  covariate = function() rnorm(n = 1, mean = 0, sd = 9)
  ) {

  ### input checks
  stopifnot(N%%1 == 0, N > 0, length(N) == 1)
  stopifnot(T%%1 == 0, T > 0, length(T) == 1)
  if(!is.null(Omega)){
    stopifnot(length(b) == ncol(Omega))
    stopifnot(ncol(Omega) == nrow(Omega))
  }
  stopifnot(ncol(Sigma) == nrow(Sigma))
  stopifnot(inherits(covariate, "function"))
  stopifnot(is.numeric(covariate()), length(covariate()) == 1)
  if(!is.null(seed)) set.seed(seed)

  ### model parameters
  P <- length(b)
  J <- ncol(Sigma)
  if(!is.null(Omega)){
    O <- t(chol(Omega))
    o <- O[lower.tri(O, diag = TRUE)]
  }
  L <- t(chol(Sigma))
  if(b[1] != 1) {
    b[1] <- 1
    warning("Set b[1] <- 1 for normalization.", call. = FALSE)
  }

  ### data simulation
  data <- data.frame("n" = rep(1:N, each = T), "t" = rep(1:T, times = N))
  ind <- 0
  for (n in 1:N){
    if(!is.null(Omega)){
      beta <- b + O %*% rnorm(P)
    } else {
      beta <- b
    }
    for(t in 1:T){
      ind <- ind + 1
      X <- matrix(replicate(J*P, covariate()),nrow=J,ncol=P)
      V <- X %*% beta
      e <- L %*% rnorm(J)
      U <- V + e
      y <- which.max(U)
      data[ind,"y"] <- y
      data[ind,paste0("x", 1:J, rep(1:P, each = J))] <- as.numeric(X)
    }
  }

  ### add parameters
  delta <- RprobitB::delta(J,1)
  Sigma_diff <- delta %*% Sigma %*% t(delta)
  L_diff <- t(chol(Sigma_diff))
  l <- L_diff[lower.tri(L_diff, diag = TRUE)]
  attr(data, "true") <- if(!is.null(Omega)) c(b[-1],o,l) else  c(b[-1],l)
  attr(data, "N") <- N
  attr(data, "T") <- T
  attr(data, "J") <- J
  attr(data, "P") <- P
  attr(data, "mix") <- !is.null(Omega)
  return(data)
}

#' Log-likelihood function of the (normally mixed) multinomial logit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Multinomial_logistic_regression
#'
#' @seealso
#' [sim_mnl()] for simulating a data set from a logit model.
#'
#' @details
#' The order of \code{theta} is supposed to be \code{c(b,o)}, where
#' \itemize{
#'   \item \code{b} denotes the coefficients
#'   \item and \code{o} the lower-triangular elements of the lower-triangular
#'         Cholesky root of \code{Omega} (if any).
#' }
#'
#' @param theta
#' The vector of model coefficients in order \code{c(b,o)}, see the details.
#' @param data
#' A \code{data.frame} with the shape of the output of \code{\link{sim_mnl}}.
#' @param R
#' The number of random draws to approximate the integral for the mixed logit
#' choice probabilities.
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @return
#' The log-likelihood value at \code{theta}.
#'
#' @examples
#' data <- sim_mnl(N = 300, J = 3, b = c(1,3), seed = 1)
#' theta <- attr(data, "true")
#' f_ll_mnl(theta = theta, data = data)
#' \donttest{
#' nlm(f_ll_mnl, p = theta, data = data, neg = TRUE)$estimate
#' }
#'
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats rnorm
#'
#' @export
#'
#' @keywords
#' internal function

f_ll_mnl <- function(theta, data, R = 100, neg = FALSE) {

  ### check inputs
  stopifnot(c("N","T","P","J","mix") %in% names(attributes(data)))

  ### model parameters
  N <- attr(data, "N")
  T <- attr(data, "T")
  P <- attr(data, "P")
  J <- attr(data, "J")
  mix <- attr(data, "mix")
  b <- theta[1:P]; theta <- theta[-(1:P)]
  if(mix){
    O <- matrix(0, P, P)
    O[lower.tri(O, diag = TRUE)] <- theta
  }

  ### log-likelihood contributions
  LL <- 0
  ind <- 0
  for(n in 1:N) for(t in 1:T){
    ind <- ind + 1
    i <- data[ind,"y"]
    X <- matrix(as.numeric(data[ind,-(1:3)]), nrow = J, ncol = P)
    if(mix){
      L_ni <- function(beta) exp(X[i,]%*%beta) / sum(exp(X%*%beta))
      p <- mean(replicate(R, L_ni(beta = b + O %*% rnorm(P))))
    } else {
      p <- exp(X[i,] %*% b) / sum(exp(X %*% b))
    }
    LL <- LL + log(p)
  }

  return(ifelse(neg, -LL, LL))
}

#' Simulate data from a (normally mixed) multinomial logit model
#'
#'
#' @seealso
#' [f_ll_mnp()] for computing the log-likelihood of a (normally mixed)
#' multinomial logit model.
#'
#' @param N
#' The number of observations.
#' @param T
#' The number of choice occasions with \code{T = 1} per default.
#' @param J
#' The number of alternatives.
#' @param b
#' The vector of coefficients.
#' @param Omega
#' The covariance matrix of the normal mixing distribution. Set to \code{NULL}
#' (the default) for no mixing distribution.
#' @param seed
#' Set a seed for the simulation.
#' @param covariate
#' A function that samples the covariates. It must return a single numeric
#' (random) value and must not require arguments. Per default,
#' \code{covariate = function() rnorm(n = 1, mean = 0, sd = 9)}, i.e. covariates
#' are sampled from a normal distribution with mean 0 and standard deviation 9.
#'
#' @return
#' A \code{data.frame}. The first column (\code{n}) is the identifier for the
#' decider, the next column (\code{t}) the identifier for the choice occasion.
#' Next comes the column \code{y} with the indices of the chosen alternatives.
#' The last columns contain the column-wise entries of \eqn{X_{nt}}.
#'
#' The true model coefficients are added to the output via the attribute
#' \code{"true"}. Additional attributes are \code{"N"} (the number of deciders),
#' \code{"T"} (the number of choice occasions), \code{"J"} (the number of
#' alternatives), \code{"P"} (the number of choice covariates), and \code{"mix"}
#' (a boolean which is \code{TRUE} if \code{Omega} is not \code{NULL}).
#'
#' @examples
#' sim_mnl(N = 3, T = 2, J = 3, b = c(-2,0.5,2), Omega = diag(3))
#'
#' @importFrom evd rgumbel
#' @importFrom stats rnorm
#'
#' @export
#'
#' @keywords
#' internal function

sim_mnl <- function(N, T = 1, J, b, Omega = NULL, seed = NULL,
                    covariate = function() rnorm(n = 1, mean = 0, sd = 9)) {

  ### input checks
  stopifnot(N%%1 == 0, N > 0, length(N) == 1)
  stopifnot(T%%1 == 0, T > 0, length(T) == 1)
  stopifnot(J%%1 == 0, J >= 2, length(J) == 1)
  if(!is.null(Omega)){
    stopifnot(length(b) == ncol(Omega))
    stopifnot(ncol(Omega) == nrow(Omega))
  }
  stopifnot(inherits(covariate, "function"))
  stopifnot(is.numeric(covariate()), length(covariate()) == 1)
  if(!is.null(seed)) set.seed(seed)

  ### model parameters
  P <- length(b)
  if(!is.null(Omega)){
    O <- t(chol(Omega))
    o <- O[lower.tri(O, diag = TRUE)]
  }

  ### data simulation
  data <- data.frame("n" = rep(1:N, each = T), "t" = rep(1:T, times = N))
  ind <- 0
  for (n in 1:N){
    if(!is.null(Omega)){
      beta <- b + O %*% rnorm(P)
    } else {
      beta <- b
    }
    for(t in 1:T){
      ind <- ind + 1
      X <- matrix(replicate(J*P, covariate()),nrow=J,ncol=P)
      V <- X %*% beta
      e <- evd::rgumbel(J,loc=0,scale=1)
      U <- V + e
      y <- which.max(U)
      data[ind,"y"] <- y
      data[ind,paste0("x", 1:J, rep(1:P, each = J))] <- as.numeric(X)
    }
  }

  ### add parameters
  attr(data, "true") <- if(!is.null(Omega)) c(b,o) else b
  attr(data, "N") <- N
  attr(data, "T") <- T
  attr(data, "J") <- J
  attr(data, "P") <- P
  attr(data, "mix") <- !is.null(Omega)
  return(data)
}
