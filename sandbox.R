# Installation ------------------------------------------------------------

# path <- "../ino_0.1.0.tar.gz"
# install.packages(path, repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()
library(tidyverse)


# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = f_ackley,
  npar = 2,
  global = c(0,0),
  opt = list("nlm" = set_optimizer_nlm(),
             "optim" = set_optimizer_optim())
)

x <- random_initialization(x, runs = 20)

overview_optima(x, digits = 2)

var_names(x)

get_vars(x, runs = 2:3, vars = c(".init", ".estimate", "iterations"))

summary(x, dist_global = "sqrt(sum((.global-.estimate)^2))") %>%
  group_by(.optimizer) %>%
  ggplot(aes(x = .optimizer, y = dist_global)) +
  geom_boxplot()

plot(x, by = ".optimizer")


# Example: HMM LL ---------------------------------------------------------

hmm_ino <- setup_ino(
  f = f_ll_hmm,
  npar = 4,
  data = earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm()
)

random_initialization(hmm_ino, return_result = TRUE)

hmm_ino <- random_initialization(hmm_ino, runs = 10)

hmm_ino <- fixed_initialization(hmm_ino, at = c(-1, -1, 0.1, 0.2))

hmm_ino <- subset_initialization(
  hmm_ino, arg = "data", how = "first", prop = 0.5,
  initialization = random_initialization(runs = 10)
)

overview_optima(hmm_ino)

summary(hmm_ino) %>%
  group_by(.strategy) %>%
  summarize("mean_time" = mean(.time))

plot(hmm_ino, by = ".strategy")


# Example: Logit LL -------------------------------------------------------

set.seed(1)
logit_data <- list()
b <- rnorm(3, sd = 3)
Omega <- RprobitB::rwishart(3,diag(3))$W
for(i in 1:10){
  name <- paste0("data",i)
  logit_data[[name]] <- sim_mnl(N = 300, J = 3, b = b, Omega = Omega, seed = i)
}

logit_ino <- setup_ino(
  f = f_ll_mnl,
  npar = 9,
  global = attr(logit_data[[1]], "true"),
  data = logit_data,
  R = list("R1" = 10, "R2" = 100),
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "R"
)

for(i in 1:10)
  logit_ino <- random_initialization(logit_ino)

for(i in 1:10)
  logit_ino <- subset_initialization(
    logit_ino, how = "kmeans", initialization = random_initialization()
  )

summary(logit_ino, group = "R")

overview_optima(logit_ino, digits = 2)

plot(logit_ino, var = ".time", by = c(".strategy", "R"))


# Example: Probit LL ------------------------------------------------------

set.seed(1)
probit_data <- list()
for(i in 1:100){
  b <- c(1, rnorm(2, sd = 3))
  Sigma <- RprobitB::rwishart(3, diag(3))$W
  name <- paste0("data",i)
  probit_data[[name]] <- sim_mnp(N = 100, b = b, Sigma = Sigma, seed = i)
}

probit_ino <- setup_ino(
  f = f_ll_mnp,
  npar = 5,
  data = probit_data[1:10],
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "data"
)

probit_ino <- random_initialization(probit_ino, runs = 10)

probit_ino <- standardize_initialization(
  probit_ino, ind_ign = 1:2, initialization = random_initialization(runs = 10)
)

probit_ino <- subset_initialization(
  probit_ino, arg = "data", how = "kmeans", prop = 0.5, by_row = TRUE,
  col_ign = c(1,2), initialization = random_initialization(runs = 10)
)

summary(probit_ino)

overview_optima(probit_ino, digits = 2)

plot(probit_ino, var = ".time")

