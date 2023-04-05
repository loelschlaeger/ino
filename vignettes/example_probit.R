## ---- include = FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.dim = c(8, 6), 
  out.width = "75%",
  eval = FALSE
)
# library("ino")
devtools::load_all() # remove later
options("ino_verbose" = FALSE)


## -----------------------------------------------------------------------------------------
J <- 3
P <- 3
scale <- c(1, 1e-2, 1e+2)
X <- function(n, t) {
  class <- sample(0:1, 1)
  mean <- ifelse(class, 1, -1)
  covariates <- stats::rnorm(J * P, mean = mean, sd = 1) / scale
  matrix(covariates, nrow = J, ncol = P, byrow = TRUE)
}
X(1, 1)


## -----------------------------------------------------------------------------------------
N <- 100
T <- 10
b <- c(1, -1, 2) * scale
Omega <- diag(P)
Sigma <- diag(J)
probit_data <- sim_mnp(N, T, J, P, b, Omega, Sigma, X)


## ---- eval = FALSE------------------------------------------------------------------------
## theta <- attr(probit_data, "true")[-1]
## f_ll_mnp(theta = theta, data = probit_data, neg = TRUE)
## nlm(f_ll_mnp, p = theta, data = probit_data, neg = TRUE, print.level = 2)$estimate


## -----------------------------------------------------------------------------------------
probit_ino <- Nop$new(
  f = f_ll_mnp,
  npar = length(theta),
  data = probit_data,
  neg = TRUE
)


## -----------------------------------------------------------------------------------------
probit_ino$set_optimizer(optimizer_nlm(iterlim = 1000))


## -----------------------------------------------------------------------------------------
probit_ino$true_parameter <- theta


## -----------------------------------------------------------------------------------------
probit_ino$test(at = theta)


## -----------------------------------------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100)


## -----------------------------------------------------------------------------------------
for(how in c("random", "kmeans")) for(prop in c(0.2,0.5)) {
  probit_ino <- subset_initialization(
    probit_ino, arg = "data", how = how, prop = prop,
    ind_ign = 1:3, initialization = random_initialization(runs = 100)
  )
}


## ---- eval = FALSE------------------------------------------------------------------------
## library("dplyr", warn.conflicts = FALSE)
## summary(probit_ino, "iterations" = "iterations") %>% filter(iterations >= 1000)


## ---- eval = FALSE------------------------------------------------------------------------
## ind <- which(summary(probit_ino, "iterations" = "iterations")$iterations >= 1000)
## probit_ino <- clear_ino(probit_ino, which = ind)


## ---- out.width = "100%", fig.dim = c(10, 6), eval = FALSE--------------------------------
## plot(probit_ino, by = ".strategy", time_unit = "mins", nrow = 1)

