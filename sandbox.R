# Installation ------------------------------------------------------------

# path <- "../ino_0.1.0.9000.tar.gz"
# install.packages(path, repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()
library("tidyverse")
options("ino_ncores" = parallel::detectCores() - 1)
# Sys.setenv(LANG = "en")


# Example: Ackley function ------------------------------------------------

ackley <- Nop$new(f = f_ackley, npar = 2)$
  set_true_parameter(true_par = c(0, 0), set_true_value = TRUE)$
  set_optimizer(optimizer = optimizer_nlm(), label = "nlm")$
  set_optimizer(optimizer = optimizer_optim(), label = "optim")$
  print()

ackley$evaluate(c(0,0))

ackley$optimize(
  initial = c(-3,3), runs = 1, which_optimizer = "nlm",
  save_results = FALSE, return_results = TRUE
)

ackley$test()

ackley$optimize(runs = 10)

ackley$summary(dist = "sqrt(sum((true_parameter - parameter)^2))")

ackley$optima(digits = 2)

plot(ackley, by = "optimizer")


# Example: HMM LL for earthquake data -------------------------------------

hmm <- Nop$new(f = f_ll_hmm, npar = 6)$
  set_argument("data" = earthquakes, "N" = 2, "neg" = TRUE)$
  set_optimizer(optimizer = optimizer_nlm(), label = "nlm")$
  print()

hmm$optimize(runs = 10, label = "random")

hmm$reduce("data", how = "first", prop = 0.5)

hmm$optimize(runs = 10, label = "subset", reset_arguments_afterwards = TRUE)$continue()

hmm$optima()

summary(hmm, c("label", "value"))

summary(hmm, "all") %>%
  group_by(label) %>%
  summarize("mean_time" = mean(seconds))

plot(hmm, by = "label")


# Example: HMM LL for financial data --------------------------------------

db_data <- fHMM::download_data(symbol = "DBK.DE", from = "2020-01-01", file = NULL)

library("dplyr")
db_data <- as_tibble(db_data) %>%
  summarize(date = as.Date(Date, format = "%Y-%m-%d"),
            obs = c(NA, diff(log(Close), lag = 1) * 100)) %>%
  filter(!is.na(obs)) %>%
  print()

hmm_finance <- Nop$new(
  f = f_ll_hmm,
  npar = 6,
  data = db_data,
  N = 2,
  neg = TRUE
)$set_optimizer(optimizer_nlm())

sampler <- function() c(log(stats::runif(2, 0.1, 0.9)),
                        stats::rnorm(2),
                        log(stats::runif(2, 0.5, 2)))

hmm_finance$optimize(initial = sampler, runs = 20, label = "random")

hmm_finance$reduce("data", how = "first", prop = 0.2)
hmm_finance$optimize(initial = sampler, runs = 20, label = "subset", reset_arguments_afterwards = TRUE)$continue()

hmm_finance$optima()

plot(hmm_finance, by = "label")

hmm_finance$summary(c("label", "seconds", "value")) %>%
  group_by(label) %>%
  summarize("mean_time" = mean(seconds))


# Example: Probit LL ------------------------------------------------------

N <- 100
T <- 1
J <- 3
P <- 3
b <- c(1,-1,0.5)
Sigma <- diag(J)
X <- function() {
  class <- sample(0:1, 1)
  mean <- ifelse(class, 2, -2)
  matrix(stats::rnorm(J*P, mean = mean), nrow = J, ncol = P)
}
probit_data <- sim_mnp(
  N = N, T = T, J = J, P = P, b = b, Sigma = Sigma, X = X
)
true <- attr(probit_data, "true")[-1]

probit <- Nop$new(
  f = f_ll_mnp,
  npar = 5,
  data = probit_data,
  neg = TRUE
)$set_true_parameter(true_par = true, set_true_value = TRUE)

probit$set_optimizer(optimizer_nlm(iterlim = 1000))

probit$test(time_limit_opt = 30)

probit$optimize("random", runs = 10, label = "random")

probit$reduce("data", how = "unsimilar", proportion = 0.5)

probit$optimize("random", runs = 10, label = "subset", reset_arguments_afterwards = TRUE)$continue()

probit$standardize("data")

probit$optimize("random", runs = 10, label = "standardize", reset_arguments_afterwards = TRUE)$continue()

probit$optima()

plot(probit, by = "label")

