#' @noRd
#' @examples
#' N <- 100
#' J <- 3
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' data <- sim_MMNL_data(N, J, b, o, seed = 1)
#' theta_true <- c(b,o)
#' theta_est <- nlm(f = nLL_MMNL, p = theta_true, data = data)$estimate
#' @importFrom evd rgumbel

sim_MMNL_data <- function(N, J, b, o, seed = NULL) {
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

#' @noRd
#' @importFrom mvtnorm pmvnorm

P_ni_MMNL <- function(i, X, b, O, R) {
  P <- length(b)
  J <- nrow(X)
  L_ni <- function(beta) exp(X[i,]%*%beta) / sum(exp(X%*%beta))
  p <- mean(replicate(R, L_ni(beta = b + O %*% rnorm(P))))
  return(p)
}

#' @noRd
#' @param theta
#' theta <- c(b, o)
#' @param R
#' Number of draws.

nLL_MMNL <- function(theta, data, R = 100) {
  P <- ncol(data$X[[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- theta
  N <- length(data$y)
  LL <- 0
  for(n in 1:N){
    LL <- LL + log(P_ni_MMNL(i = data$y[n], X = data$X[[n]], b = b, O = O, R = R))
  }
  return(-LL)
}

