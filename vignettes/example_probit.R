knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.path = "figures/probit-",
  fig.dim = c(8, 6),
  out.width = "75%",
  # all optimizations are pre-computed to save building time
  eval = FALSE
)
# library("ino")
devtools::load_all()
# data("Nop_probit")
set.seed(1)
ggplot2::theme_set(ggplot2::theme_minimal())

future::plan(future::multisession, workers = 10)

progressr::handlers(global = TRUE)

sim_mnp <- function(
    N, Tp = 1, J, P, b = stats::rnorm(P), Omega = NULL, Sigma = diag(J),
    X = function(n, t) matrix(stats::rnorm(J * P), nrow = J, ncol = P),
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(
    b[1] == 1, is.function(X), names(formals(X)) == c("n", "t"),
    is.numeric(N), length(N) == 1, N > 0, N %% 1 == 0,
    is.numeric(Tp), length(Tp) == 1, Tp > 0, Tp %% 1 == 0,
    is.numeric(J), length(J) == 1, J > 1, J %% 1 == 0,
    is.numeric(P), length(P) == 1, P > 0, P %% 1 == 0
  )
  b <- matrix(b)
  mix <- !(is.null(Omega) || all(Omega == 0))
  if(mix) {
    O <- t(chol(Omega))
    diag(O) <- abs(diag(O))
    o <- O[lower.tri(O, diag = TRUE)]
  }
  D <- diag(J)
  D[, J] <- -1
  D <- D[-J, , drop = FALSE]
  Sigma_d <- D %*% Sigma %*% t(D)
  L_d <- t(chol(Sigma_d))
  diag(L_d) <- abs(diag(L_d))
  l_d <- L_d[lower.tri(L_d, diag = TRUE)]
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != J & col(Sigma) != J] <- Sigma_d
  Sigma <- Sigma + 1
  L <- t(chol(Sigma))
  beta <- lapply(1:N, function(x) if(mix) b + O %*% stats::rnorm(P) else b)
  data <- lapply(1:N, function(n) {
    out <- lapply(1:Tp, function(t) {
      X_nt <- X(n, t)
      U_nt <- X_nt %*% beta[[n]] + L %*% stats::rnorm(J)
      y_nt <- which.max(U_nt)
      list(X = X_nt, y = y_nt)
    })
    list(X = lapply(out, `[[`, "X"), y = sapply(out, `[[`, "y"))
  })
  data_y <- unlist(lapply(data, `[[`, "y"))
  data_X <- matrix(unlist(lapply(data, `[[`, "X")), ncol = P*J, byrow = TRUE)
  colnames(data_X) <- paste(
    "X", paste0(rep(1:J, times = P), rep(1:P, each = J)), sep = "."
  )
  true <- c(b[-1], if(mix) o, l_d)
  chol_ind <- function(dim) {
    ind <- paste0(rep(1:dim, times = dim), rep(1:dim, each = dim))
    ind[lower.tri(matrix(ind, dim, dim), diag = TRUE)]
  }
  names(true) <- c(
    if (P >= 2) paste("b", 2:P, sep = "."),
    if (mix) paste("o", chol_ind(P), sep = "."),
    paste("l", chol_ind(J - 1), sep = ".")
  )
  structure(
    cbind(
      data.frame(n = rep(1:N, each = Tp), t = rep(1:Tp, times = N), y = data_y),
      data_X
    ),
    "true" = true, "J" = J, "P" = P, "mix" = mix
  )
}

X <- function(n, t) {
  J <- 3
  cbind(stats::rnorm(J, mean = 10, sd = 3), stats::rnorm(J, mean = 0, sd = 0.3))
}
X(n = 1, t = 1)

N <- 100
Tp <- 20
b <- c(1, -10)
Omega <- matrix(c(0.2, 0.5, 0.5, 2), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.2, -0.5, 1, 0.2, 0.2, 0.2, 1), 3, 3)
probit_data <- sim_mnp(N, Tp, J = 3, P = 2, b, Omega, Sigma, X, seed = 1)

head(probit_data)

round(theta <- attr(probit_data, "true"), 2)

f_ll_mnp <- function(
    theta, data, neg = FALSE, normal_cdf = mvtnorm::pmvnorm, threshold = 1e-6
  ) {
  stopifnot(
    is.numeric(threshold), length(threshold) == 1, threshold > 0, threshold < 1,
    is.data.frame(data), c("P", "mix", "J") %in% names(attributes(data))
  )
  P <- attr(data, "P")
  mix <- attr(data, "mix")
  J <- attr(data, "J")
  stopifnot(
    is.numeric(theta), length(theta) == (P-1) + mix *(P*(P+1)/2) + (J-1)*J/2,
    is.function(normal_cdf),
    c("lower", "upper", "mean", "sigma") %in% names(formals(normal_cdf))
  )
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
  ll <- sum(unlist(lapply(unique(data$n), function(n) {
      sapply(unique(data[data$n == n, "t"]), function(t) {
        ind <- which(data$n == n & data$t == t)
        y <- data[ind, "y"]
        X <- matrix(as.numeric(data[ind, -(1:3)]), nrow = J, ncol = P)
        D <- delta(y, J)
        sys <- as.numeric(D %*% X %*% b)
        cov <- if(mix) X %*% Omega %*% t(X) + Sigma else Sigma
        cov <- D %*% cov %*% t(D)
        ### round entries to avoid failure in check for covariance matrix
        ### due to minor discrepancy of symmetric elements
        cov <- round(cov, 6)
        prob <- do.call(
          what = normal_cdf,
          args = list(lower = -Inf, upper = -sys, mean = 0, sigma = cov)
        )
        if (!is.finite(prob) || prob < threshold) prob <- threshold
        log(as.numeric(prob))
      })
  })))
  ifelse(neg, -ll, ll)
}

f_ll_mnp(theta = theta, data = probit_data)

Nop_probit <- Nop$new(objective = f_ll_mnp, npar = 7, data = probit_data, neg = TRUE)$
  set_optimizer(optimizeR::optimizer_nlm(iterlim = 1000))

Nop_probit$true(theta)

print(Nop_probit)

Nop_probit$
  initialize_random(runs = 100)$
  optimize(optimization_label = "random")

Nop_probit$
  reduce("data", how = "random", proportion = 0.2)$
  initialize_random(runs = 100)$
  optimize(optimization_label = "reduced")$
  fixed_argument("reset", argument_name = "data")$
  initialize_continue(which_run = "reduced")$
  optimize(optimization_label = "initialized_reduced")

# Nop_probit$deviation(
#   reference = Nop_probit$true(), which_run = c("random", "subset"),
#   parameter_labels = c("b.2", "o.11", "o.21", "o.22", "l.11", "l.21", "l.22"),
#   ylim = c(-10, 10)
# )

Nop_probit$
  standardize("data", ignore = 1:3)$
  initialize_random(runs = 100)$
  optimize(optimization_label = "standardized")$
  fixed_argument("reset", argument_name = "data")

Nop_probit$
  standardize("data", ignore = 1:3)$
  reduce("data", how = "random", proportion = 0.2)$
  initialize_random(runs = 100)$
  optimize(optimization_label = "standardized_reduced")$
  fixed_argument("reset", argument_name = "data")$
  standardize("data", ignore = 1:3)$
  initialize_continue(which_run = "standardized_reduced")$
  optimize(optimization_label = "initialized_standardized_reduced")

Nop_probit$plot(group_by = ".optimization_label", relative = TRUE)

# library("dplyr")
# summary(Nop_probit, which_element = c("seconds", "label")) %>%
#   group_by(label) %>%
#   summarize(
#     mean_seconds = mean(seconds, na.rm = TRUE),
#     sd_seconds = sd(seconds, na.rm = TRUE)
#   ) %>%
#   arrange(mean_seconds)
