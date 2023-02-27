options("ino_verbose" = TRUE)

rm(list=ls())
devtools::load_all()

ackley <- Nop$new(f = f_ackley, npar = 2)$
  set_optimizer(optimizer_nlm())$
  set_optimizer(optimizer_optim())

self <- ackley
private <- self$.__enclos_env__$private

ackley$optimize(runs = 10)
ackley$results()


rm(list=ls())
devtools::load_all()

J <- 3
P <- 2
scale <- c(1, 1e+2)
X <- function(n, t) {
  class <- sample(0:1, 1)
  mean <- ifelse(class, 1, -1)
  covariates <- stats::rnorm(J * P, mean = mean, sd = 1) / scale
  matrix(covariates, nrow = J, ncol = P, byrow = TRUE)
}
N <- 10
T <- 2
b <- c(1, -1) * scale
Omega <- diag(P)
Sigma <- diag(J)
probit_data <- sim_mnp(N, T, J, P, b, Omega, Sigma, X)
theta <- attr(probit_data, "true")[-1]
probit_ino <- Nop$new(
  f = f_ll_mnp,
  npar = length(theta),
  data = probit_data,
  neg = TRUE)$
  set_optimizer(optimizer_nlm(iterlim = 1000))

probit_ino$true_parameter <- theta

self <- probit_ino
private <- self$.__enclos_env__$private

probit_ino


probit_ino$remove_optimizer(1)


which_element = "default", which_run = "all", which_optimizer = "all",
digits = getOption("ino_digits", default = 2), only_comparable = FALSE



