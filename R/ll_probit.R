#' @noRd
#' @examples
#' N <- 100
#' b <- c(-2,0.5,2)
#' Omega <- diag(3)
#' O <- t(chol(Omega))
#' o <- O[lower.tri(O, diag = TRUE)]
#' Sigma <- diag(3)
#' L <- t(chol(Sigma))
#' l <- L[lower.tri(L, diag = TRUE)][-1]
#' data <- sim_MMNP_data(N, b, o, l, seed = 1)
#' theta_true <- c(b,o,l)
#' theta_est <- nlm(f = nLL_MMNP, p = theta_true, data = data)$estimate

sim_MMNP_data <- function(N, b, o, l, seed = NULL) {
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

#' @noRd
#' @importFrom mvtnorm pmvnorm

P_ni_MMNP <- function(i, X, b, O, L, algorithm) {
  J <- nrow(L)
  delta <- RprobitB:::delta
  arg <- as.vector(-delta(J,i) %*% X %*% b)
  Omega <- O %*% t(O)
  Sigma <- L %*% t(L)
  Gamma <- delta(J,i) %*% ( X %*% Omega %*% t(X) + Sigma ) %*% t(delta(J,i))
  p <- mvtnorm::pmvnorm(lower = -Inf, upper = arg, mean = rep(0,J-1),
                        sigma = Gamma, algorithm = algorithm)[1]
  return(p)
}

#' @noRd
#' @param theta
#' theta <- c(b, o, l)

nLL_MMNP <- function(theta, data, algorithm = mvtnorm::Miwa(steps = 10)) {
  P <- ncol(data$X[[1]])
  J <- nrow(data$X[[1]])
  b <- theta[1:P]; theta <- theta[-(1:P)]
  o <- theta[1:(P*(P+1)/2)]; theta <- theta[-(1:(P*(P+1)/2))]
  l <- theta
  O <- matrix(0, P, P)
  O[lower.tri(O, diag = TRUE)] <- o
  L <- matrix(0, J, J)
  L[lower.tri(L, diag = TRUE)] <- c(1,l)
  N <- length(data$y)
  LL <- 0
  for(n in 1:N){
    LL <- LL + log(P_ni_MMNP(i = data$y[n], X = data$X[[n]], b = b, O = O,
                             L = L, algorithm = algorithm))
  }
  return(-LL)
}

