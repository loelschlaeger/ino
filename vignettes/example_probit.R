## ---- setup, include = FALSE-------------------------------------------------------------------
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


## ---- choice covariates, eval = TRUE-----------------------------------------------------------
X <- function(n, t) {
  cbind(stats::rnorm(3, mean = 10, sd = 3), stats::rnorm(3, mean = 0, sd = 0.3))
}
X(n = 1, t = 1)


## ---- simulate data, eval = TRUE---------------------------------------------------------------
N <- 200
T <- 20
b <- c(1, -10)
Omega <- matrix(c(0.2, 0.5, 0.5, 2), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.2, -0.5, 1, 0.2, 0.2, 0.2, 1), 3, 3)
probit_data <- sim_mnp(N, T, J = 3, P = 2, b, Omega, Sigma, X, seed = 1)


## ---- head of data, eval = TRUE----------------------------------------------------------------
head(probit_data)


## ---- true parameter, eval = TRUE--------------------------------------------------------------
(theta <- attr(probit_data, "true"))


## ---- likelihood evaluation, eval = TRUE-------------------------------------------------------
f_ll_mnp(theta = theta, data = probit_data)


## ---- define Nop, eval = TRUE------------------------------------------------------------------
probit_ino <- Nop$new(f = f_ll_mnp, npar = 7, data = probit_data, neg = TRUE)$
  set_optimizer(optimizer_nlm(iterlim = 1000))


## ---- set true parameter, eval = TRUE----------------------------------------------------------
probit_ino$true_parameter <- theta


## ---- print initial Nop object, eval = TRUE----------------------------------------------------
print(probit_ino)


## ---- optimize with random initial values------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "random", ncores = 4)


## ---- eval = TRUE------------------------------------------------------------------------------
probit_ino$reduce("data", how = "random", proportion = 0.25)


## ----------------------------------------------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "subset", ncores = 4)$
  reset_argument("data")$
  continue()


## ---- eval = TRUE------------------------------------------------------------------------------
probit_ino <- ino::probit_ino

random_initial <- lapply(probit_ino$results(which_run = "random"), `[[`, "initial") |>
  do.call(what = rbind) |> sweep(2, probit_ino$true_parameter) |>
  as.data.frame() |>
  cbind(type = "random_initial")

subset_initial <- lapply(
  probit_ino$results(which_run = "subset", which_element = "previous_run"), 
  `[[`, "previous_run.parameter"
  ) |>
  do.call(what = rbind) |> sweep(2, probit_ino$true_parameter) |>
  as.data.frame() |>
  cbind(type = "subset_initial")

library(ggplot2)
library(reshape2)

melt(rbind(random_initial, subset_initial)) |> 
  ggplot(aes(x = variable, y = value)) +
  geom_point(aes(color = type), position = "jitter") +
  labs(
    y = "difference to true parameter"
  ) +
  scale_x_discrete(
    labels = c("b.2", "o.11", "o.21", "o.22", "l.11", "l.21", "l.22"),
    name = "parameter"
  ) +
  scale_y_continuous(
    name = "difference to true parameter",
    limits = c(-5, 5)
  )

