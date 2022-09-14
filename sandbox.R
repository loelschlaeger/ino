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


# Example: Probit LL ------------------------------------------------------

probit_data <- sim_mnp(N = 100, T = 10, J = 3, P = 3, b = c(1,2,3))

probit_ino <- setup_ino(
  f = f_ll_mnp,
  npar = 5,
  global = attr(probit_data, "true")[-1],
  data = probit_data,
  neg = TRUE,
  opt = list(
    "nlm" = set_optimizer_nlm(),
    "ao" = set_optimizer_ao(partition = list(1:2, 3:5))
  )
)

probit_ino <- random_initialization(probit_ino, runs = 10)

probit_ino <- standardize_initialization(
  probit_ino, ind_ign = 1:3, initialization = random_initialization(runs = 10)
)

probit_ino <- subset_initialization(
  probit_ino, arg = "data", how = "kmeans", prop = 0.5,
  ind_ign = 1:3, initialization = random_initialization(runs = 10)
)
# have different T after sub-setting, than ll comp with fixed T fails

summary(probit_ino)

overview_optima(probit_ino, digits = 2)

plot(probit_ino, var = ".time")

