## ---- setup, include = FALSE--------------------------------------------------------
#> knitr::opts_chunk$set(
#>   collapse = TRUE,
#>   comment = "#>",
#>   fig.align = "center",
#>   fig.dim = c(8, 6),
#>   out.width = "75%",
#>   eval = FALSE
#> )
#> # library("ino")
devtools::load_all() # remove later
options("ino_verbose" = TRUE)


## ---- choice covariates, eval = TRUE------------------------------------------------
X <- function(n, t) {
  J <- 3
  cbind(stats::rnorm(J, mean = 10, sd = 3), stats::rnorm(J, mean = 0, sd = 0.3))
}
X(n = 1, t = 1)


## ---- simulate data, eval = TRUE----------------------------------------------------
N <- 200
T <- 20
b <- c(1, -10)
Omega <- matrix(c(0.2, 0.5, 0.5, 2), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.2, -0.5, 1, 0.2, 0.2, 0.2, 1), 3, 3)
probit_data <- sim_mnp(N, T, J = 3, P = 2, b, Omega, Sigma, X, seed = 1)


## ---- head of data, eval = TRUE-----------------------------------------------------
head(probit_data)


## ---- true parameter, eval = TRUE---------------------------------------------------
round(theta <- attr(probit_data, "true"), 2)


## ---- likelihood evaluation, eval = TRUE--------------------------------------------
f_ll_mnp(theta = theta, data = probit_data)


## ---- define Nop, eval = TRUE-------------------------------------------------------
probit_ino <- Nop$new(f = f_ll_mnp, npar = 7, data = probit_data, neg = TRUE)$
  set_optimizer(optimizer_nlm(iterlim = 1000))


## ---- set true parameter, eval = TRUE-----------------------------------------------
probit_ino$true_parameter <- theta


## ---- print initial Nop object, eval = TRUE-----------------------------------------
print(probit_ino)


## ---- optimize with random initial values-------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "random")


## -----------------------------------------------------------------------------------
probit_ino$
  reduce("data", how = "random", proportion = 0.2)$
  optimize(initial = "random", runs = 100, label = "subset")$
  reset_argument("data")$
  continue()


## -----------------------------------------------------------------------------------
probit_ino$
  standardize("data", by_column = TRUE, ignore = 1:3)$
  optimize(initial = "random", runs = 100, label = "standardized")$
  reset_argument("data")

