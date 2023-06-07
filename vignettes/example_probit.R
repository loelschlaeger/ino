<<<<<<< HEAD
## ---- setup, include = FALSE-------------------------------------------------------------------
=======
## ---- setup, include = FALSE----------------------------------------------------------------------------------------------------------------
>>>>>>> master
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
<<<<<<< HEAD
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
=======
  fig.path = "figures/probit-",
  fig.dim = c(8, 6), 
  out.width = "75%",
  # all optimizations are pre-computed to save building time
  eval = FALSE
)
library("ino")
options("ino_verbose" = TRUE) 
data("probit_ino")
set.seed(1)
ggplot2::theme_set(ggplot2::theme_minimal())


## ---- choice covariates, eval = TRUE--------------------------------------------------------------------------------------------------------
X <- function(n, t) {
  J <- 3
  cbind(stats::rnorm(J, mean = 10, sd = 3), stats::rnorm(J, mean = 0, sd = 0.3))
>>>>>>> master
}
X(n = 1, t = 1)


<<<<<<< HEAD
## ---- simulate data, eval = TRUE---------------------------------------------------------------
N <- 200
T <- 30
b <- c(1, -5)
Omega <- matrix(c(1, 0.1, 0.1, 2), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.2, -0.5, 1, 0.2, 0.2, 0.2, 1), 3, 3)
probit_data <- sim_mnp(N, T, J = 3, P = 2, b, Omega, Sigma, X, seed = 1)


## ---- head of data, eval = TRUE----------------------------------------------------------------
head(probit_data)


## ---- true parameter, eval = TRUE--------------------------------------------------------------
(theta <- attr(probit_data, "true"))


## ---- likelihood evaluation, eval = TRUE-------------------------------------------------------
f_ll_mnp(theta = theta, data = probit_data)


## ---- define Nop, eval = TRUE------------------------------------------------------------------
=======
## ---- simulate data, eval = TRUE------------------------------------------------------------------------------------------------------------
N <- 100
Tp <- 20
b <- c(1, -10)
Omega <- matrix(c(0.2, 0.5, 0.5, 2), 2, 2)
Sigma <- matrix(c(1, -0.5, 0.2, -0.5, 1, 0.2, 0.2, 0.2, 1), 3, 3)
probit_data <- sim_mnp(N, Tp, J = 3, P = 2, b, Omega, Sigma, X, seed = 1)


## ---- head of data, eval = TRUE-------------------------------------------------------------------------------------------------------------
head(probit_data)


## ---- true parameter, eval = TRUE-----------------------------------------------------------------------------------------------------------
round(theta <- attr(probit_data, "true"), 2)


## ---- likelihood evaluation, eval = TRUE----------------------------------------------------------------------------------------------------
f_ll_mnp(theta = theta, data = probit_data)


## ---- define Nop----------------------------------------------------------------------------------------------------------------------------
>>>>>>> master
probit_ino <- Nop$new(f = f_ll_mnp, npar = 7, data = probit_data, neg = TRUE)$
  set_optimizer(optimizer_nlm(iterlim = 1000))


<<<<<<< HEAD
## ---- set true parameter, eval = TRUE----------------------------------------------------------
probit_ino$true_parameter <- theta


## ---- print initial Nop object, eval = TRUE----------------------------------------------------
print(probit_ino)


## ---- optimize with random initial values------------------------------------------------------

saveRDS(probit_ino, "probit_ino.rds")

probit_ino$optimize(initial = "random", runs = 20, label = "random", ncores = 5)


for (proportion in seq(0.1, 0.9, 0.1)) {

  saveRDS(probit_ino, "probit_ino.rds")

  probit_ino$reduce("data", how = "first", proportion = proportion)$
    optimize(initial = "random", runs = 20, label = paste0("subset_", proportion) , ncores = 5)$
    reset_argument("data")$
    continue()

}


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
=======
## ---- set true parameter--------------------------------------------------------------------------------------------------------------------
probit_ino$true_parameter <- theta


## ---- print initial Nop object, eval = TRUE-------------------------------------------------------------------------------------------------
print(probit_ino)


## ---- optimize with random initial values---------------------------------------------------------------------------------------------------
probit_ino$optimize(initial = "random", runs = 100, label = "random")


## ---- optimize on subset--------------------------------------------------------------------------------------------------------------------
probit_ino$
  reduce("data", how = "random", proportion = 0.2)$
  optimize(initial = "random", runs = 100, label = "subset")$
  reset_argument("data")$
  continue()


## ---- deviation, eval = TRUE, warning = FALSE-----------------------------------------------------------------------------------------------
probit_ino$deviation(
  reference = probit_ino$true_parameter, which_run = c("random", "subset"),
  parameter_labels = c("b.2", "o.11", "o.21", "o.22", "l.11", "l.21", "l.22"),
  ylim = c(-10, 10)
)


## ---- optimize with standardized data-------------------------------------------------------------------------------------------------------
probit_ino$
  standardize("data", by_column = TRUE, ignore = 1:3)$
  optimize(initial = "random", runs = 100, label = "standardized")$
  reset_argument("data")


## ---- optimize with standardize and subset--------------------------------------------------------------------------------------------------
probit_ino$
  standardize("data", by_column = TRUE, ignore = 1:3)$
  reduce("data", how = "random", proportion = 0.2)$
  optimize(initial = "random", runs = 100, label = "standardized_subset")$
  reset_argument("data")$
  standardize("data", by_column = TRUE, ignore = 1:3)$
  continue()


## ---- times, eval = TRUE, message = FALSE---------------------------------------------------------------------------------------------------
plot(probit_ino, by = "label", relative = TRUE, xlim = c(-1, 3))


## ---- summary, eval = TRUE, warning = FALSE, message = FALSE--------------------------------------------------------------------------------
library("dplyr")
summary(probit_ino, which_element = c("seconds", "label")) %>% 
  group_by(label) %>% 
  summarize(
    mean_seconds = mean(seconds, na.rm = TRUE),
    sd_seconds = sd(seconds, na.rm = TRUE)
  ) %>%
  arrange(mean_seconds)
>>>>>>> master

