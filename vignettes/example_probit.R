## ---- setup, include = FALSE-----------------------------------------------------------------------------
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
options("ino_verbose" = TRUE)


## ---- choice covariates, eval = TRUE---------------------------------------------------------------------
J <- 3
P <- 2
X <- function(n, t) {
  matrix(stats::rnorm(J * P, mean = 0, sd = 3), nrow = J)
}
X(n = 1, t = 1)


## ---- simulate data, eval = TRUE-------------------------------------------------------------------------
N <- 200
T <- 20
b <- c(1, -1)
Omega <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.25, -0.5, 1, 0.25, 0.25, 0.25, 1), 3, 3)
probit_data <- sim_mnp(N, T, J, P, b, Omega, Sigma, X, seed = 1)


## ---- head of data, eval = TRUE--------------------------------------------------------------------------
head(probit_data)


## ---- true parameter, eval = TRUE------------------------------------------------------------------------
(theta <- attr(probit_data, "true"))


## ---- likelihood evaluation, eval = TRUE-----------------------------------------------------------------
f_ll_mnp(theta = theta, data = probit_data)


## ---- define Nop, eval = TRUE----------------------------------------------------------------------------
probit_ino <- Nop$new(f = f_ll_mnp, npar = 7, data = probit_data, neg = TRUE)$
  set_optimizer(optimizer_nlm(iterlim = 1000))


## ---- set true parameter, eval = TRUE--------------------------------------------------------------------
probit_ino$true_parameter <- theta


## ---- print initial Nop object, eval = TRUE--------------------------------------------------------------
print(probit_ino)


## ---- optimize with random initial values----------------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "random", ncores = 4)


## ---- eval = TRUE----------------------------------------------------------------------------------------
probit_ino$reduce("data", how = "random", proportion = 0.25)


## --------------------------------------------------------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "subset", ncores = 4)$
  reset_argument("data")$
  continue(ncores = 4)


## ---- eval = FALSE---------------------------------------------------------------------------------------
library("dplyr", warn.conflicts = FALSE)
summary(probit_ino, "iterations" = "iterations") %>% filter(iterations >= 1000)


## ---- eval = FALSE---------------------------------------------------------------------------------------
ind <- which(summary(probit_ino, "iterations" = "iterations")$iterations >= 1000)
probit_ino <- clear_ino(probit_ino, which = ind)


## ---- out.width = "100%", fig.dim = c(10, 6), eval = FALSE-----------------------------------------------
plot(probit_ino, by = ".strategy", time_unit = "mins", nrow = 1)

