# Installation ------------------------------------------------------------

# path <- "../ino_0.1.0.9000.tar.gz"
# install.packages(path, repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()
library(tidyverse)
options(ino_ncores = parallel::detectCores() - 1)

# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = f_ackley,
  npar = 2,
  global = c(0,0),
  opt = list(
    "nlm" = set_optimizer_nlm(),
    "optim" = set_optimizer_optim()
  )
)

random_initialization(x) %>% get_vars()

x <- random_initialization(x, runs = 20)

overview_optima(x, digits = 2)

var_names(x)

summary(x, dist = "sqrt(sum((.global-.estimate)^2))") %>%
  group_by(.optimizer) %>%
  ggplot(aes(x = .optimizer, y = dist)) +
  geom_boxplot()

plot(x, by = ".optimizer")


# Example: HMM LL ---------------------------------------------------------

hmm_ino <- setup_ino(
  f = f_ll_hmm,
  npar = 4,
  data = earthquakes,
  N = 2,
  neg = TRUE,
  opt = list(
    "nlm" = set_optimizer_nlm(),
    "ao" = set_optimizer_ao(partition = list(1:2, 3:4))
  )
)

hmm_ino <- fixed_initialization(hmm_ino, at = c(-1, -1, 0.1, 0.2))

hmm_ino <- random_initialization(hmm_ino, runs = 10)

hmm_ino <- subset_initialization(
  hmm_ino, how = "first", prop = 0.4,
  initialization = random_initialization(runs = 10)
)

overview_optima(hmm_ino)

summary(hmm_ino) %>%
  group_by(.strategy) %>%
  summarize("mean_time" = mean(.time))

plot(hmm_ino, by = ".strategy")


# Example: Logit LL -------------------------------------------------------

logit_data <- sim_mnl(N = 100, J = 3, b = rnorm(2), Omega = NULL)

logit_ino <- setup_ino(
  f = f_ll_mnl,
  npar = 5,
  global = attr(logit_data[[1]], "true"),
  data = logit_data,
  neg = TRUE,
  opt = set_optimizer_nlm()
)

logit_ino <- random_initialization(logit_ino, runs = 10)

logit_ino <- subset_initialization(
  logit_ino, how = "random", prop = 0.5,
  initialization = random_initialization(runs = 10)
)

logit_ino <- standardize_initialization(
  logit_ino, initialization = random_initialization(runs = 10)
)

summary(logit_ino)

overview_optima(logit_ino, digits = 2)

plot(logit_ino, by = ".strategy")


# Example: Probit LL ------------------------------------------------------

b <- c(1, 2)
Sigma <- RprobitB::rwishart(2, diag(2))$W
probit_data <- sim_mnp(N = 100, b = b, Sigma = Sigma)

probit_ino <- setup_ino(
  f = f_ll_mnp,
  npar = 5,
  global = attr(probit_data[[1]], "true"),
  data = probit_data,
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

