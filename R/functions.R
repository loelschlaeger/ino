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
#' The example uses a data set from {ino} that covers the number of major
#' earthquakes (magnitude 7 or greater) in the world from 1900 until 2006.
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
#' @param J
#' The number of alternatives.
#' @param P
#' The number of choice covariates.
#' @param b
#' The mean effects vector of length \code{P}, first element must be \code{1}.
#' @param Omega
#' The covariance matrix of the normal mixing distribution of dimension \code{P}
#' times \code{P}. Set to \code{NULL} (the default) for no mixing distribution.
#' @param Sigma
#' The error term covariance matrix of dimension \code{J} times \code{J}.
#' @param X
#' A function that samples the covariates. It must return a numeric matrix
#' of dimension \code{J} times \code{P}.
#'
#' @return
#' A \code{data.frame}. The first column (\code{N}) is the identifier for the
#' decider, the next column (\code{T}) the identifier for the choice occasion.
#' Next comes the column \code{y} with the indices of the chosen alternatives.
#' The last columns contain the column-wise entries of the covariate matrices.
#'
#' The true model coefficients are added to the output via the attribute
#' \code{"true"}. They are already normalized and can be directly compared with
#' the maximum likelihood estimate.
#'
#' Additional attributes are \code{"J"} (the number of alternatives),
#' \code{"P"} (the number of choice covariates), and \code{"mix"} (a boolean
#' which is \code{TRUE} if \code{Omega} is not \code{NULL}).
#'
#' @examples
#' sim_mnp(N = 3, J = 2, P = 2, b = c(1,-1), Omega = diag(2), Sigma = diag(2))
#'
#' @importFrom stats rnorm
#'
#' @keywords
#' internal function
#'
#' @export

sim_mnp <- function(
    N, T = 1, J, P, b = stats::rnorm(P), Omega = NULL, Sigma = diag(J),
    X = function() matrix(stats::rnorm(J*P), nrow = J, ncol = P)
) {
  stopifnot(b[1] == 1)
  b <- matrix(b)
  mix <- !(is.null(Omega) || all(Omega == 0))
  if(mix) {
    O <- t(chol(Omega))
    o <- O[lower.tri(O, diag = TRUE)]
  }
  D <- diag(J)
  D[, J] <- -1
  D <- D[-J, , drop = FALSE]
  Sigma_d <- D %*% Sigma %*% t(D)
  L_d <- t(chol(Sigma_d))
  l_d <- L_d[lower.tri(L_d, diag = TRUE)]
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != J & col(Sigma) != J] <- Sigma_d
  Sigma <- Sigma + 1
  L <- t(chol(Sigma))
  beta <- lapply(1:N, function(x) if(mix) b + O %*% stats::rnorm(P) else b)
  data <- lapply(1:N, function(n) {
    out <- lapply(1:T, function(t) {
      X_nt <- X()
      U_nt <- X_nt %*% beta[[n]] + L %*% stats::rnorm(J)
      y_nt <- which.max(U_nt)
      list(X = X_nt, y = y_nt)
    })
    list(X = lapply(out, `[[`, "X"), y = sapply(out, `[[`, "y"))
  })
  structure(
    cbind(
      data.frame(
        N = rep(1:N, each = T),
        T = rep(1:T, times = N),
        y = unlist(lapply(data, `[[`, "y"))
      ),
      matrix(unlist(lapply(data, `[[`, "X")), ncol = P*J, byrow = TRUE)
    ),
    "true" = c(b, if(mix) o, l_d), "J" = J, "P" = P, "mix" = mix
  )
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
#'   \item \code{b} denotes the vector of mean effects without the first entry,
#'   \item \code{o} the lower-triangular elements of the lower-triangular
#'         Cholesky root of the effect covariance matrix \code{Omega} (if any),
#'   \item and \code{l} the lower-triangular elements of the lower-triangular
#'         Cholesky root \code{L} of the differenced (with respect to
#'         the last alternative) error term covariance matrix \code{Sigma}.
#' }
#'
#' @param theta
#' The vector of model coefficients, see the details.
#' @param data
#' A \code{data.frame}, the output of \code{\link{sim_mnp}}.
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#' @param normal_cdf
#' A function that evaluates the CDF of an \code{n}-variate normal distribution.
#' It must take the arguments
#' \itemize{
#'   \item \code{lower}, the vector of lower limits of length \code{n},
#'   \item \code{upper}, the vector of upper limits of length \code{n},
#'   \item \code{mean}, the mean vector of length \code{n},
#'   \item \code{sigma}, the \code{n} times \code{n} covariance matrix,
#' }
#' and return a single numeric value.
#'
#' @return
#' The log-likelihood value at \code{theta}.
#'
#' @examples
#' data <- sim_mnp(N = 200, J = 3, P = 2, b = c(1,3))
#' theta <- attr(data, "true")[-1]
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
#' @importFrom mvtnorm pmvnorm

f_ll_mnp <- function(theta, data, neg = FALSE, normal_cdf = mvtnorm::pmvnorm) {
  P <- attr(data, "P")
  mix <- attr(data, "mix")
  J <- attr(data, "J")
  stopifnot(is.numeric(theta), length(theta) == (P-1) + mix*(P*(P+1)/2) + (J-1)*J/2)
  b <- c(1, theta[1:(P-1)]); theta <- theta[-(1:(P-1))]
  chol_2_cov <- function(chol) {
    dim <- -0.5 + sqrt(0.25 + 2*length(chol))
    cov <- matrix(0, dim, dim)
    cov[lower.tri(cov, diag = TRUE)] <- chol
    cov %*% t(cov)
  }
  if(mix) {
    Omega <- chol_2_cov(theta[1:(P*(P+1)/2)]); theta <- theta[-(1:(P*(P+1)/2))]
  }
  Sigma_d <- chol_2_cov(theta[1:((J-1)*J/2)])
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != J & col(Sigma) != J] <- Sigma_d
  Sigma <- Sigma + 1
  delta <- function(diff_alt, J){
    D <- diag(J)
    D[,diff_alt] <- -1
    D[-diff_alt, , drop = FALSE]
  }
  ll <- sum(unlist(lapply(unique(data$N), function(n) {
      sapply(unique(data[data$N == n, "T"]), function(t) {
        ind <- which(data$N == n & data$T == t)
        y <- data[ind, "y"]
        X <- matrix(as.numeric(data[ind, -(1:3)]), nrow = J, ncol = P)
        D <- delta(y, J)
        sys <- as.numeric(D %*% X %*% b)
        cov <- if(mix) X %*% Omega %*% t(X) + Sigma else Sigma
        cov <- D %*% cov %*% t(D)
        prob <- do.call(
          what = normal_cdf,
          args = list(lower = -Inf, upper = -sys, mean = 0, sigma = cov)
        )
        lprob <- suppressWarnings(log(prob))
        ifelse(is.finite(lprob), lprob, 0)
      })
  })))
  ifelse(neg, -ll, ll)
}

