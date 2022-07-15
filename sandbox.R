# Installation ------------------------------------------------------------

# ppath <- "../ino_0.1.0.tar.gz"
# install.packages(ppath, repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()

# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = f_ackley,
  npar = 2,
  opt = list("nlm"   = set_optimizer_nlm(),
             "optim" = set_optimizer_optim())
)

for(i in 1:5)
  x <- random_initialization(x)

for(at in list(c(1, 0.5), c(0.3, 2)))
  x <- fixed_initialization(x, at = at)

summary(x, "average_time" = mean(.time))

plot(x, var = ".time", by = ".strategy") + ggplot2::theme_minimal()

overview_optima(x, digits = 2)

# Example: HMM LL ---------------------------------------------------------

hmm_ino <- setup_ino(
  f = f_ll_hmm,
  npar = 4,
  data = ino::earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm()
)

for(i in 1:5)
  hmm_ino <- random_initialization(hmm_ino)

for(at in list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))
  hmm_ino <- fixed_initialization(hmm_ino, at = at)

for(i in 1:5)
  hmm_ino <- subset_initialization(
    hmm_ino, arg = "data", how = "first", prop = 0.2,
    initialization = random_initialization())

for(at in list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))
  hmm_ino <- subset_initialization(
    hmm_ino, arg = "data", how = "first", prop = 0.2,
    initialization = fixed_initialization(at = at))

for(i in 1:5)
  hmm_ino <- standardize_initialization(hmm_ino)

summary(hmm_ino, "mean" = mean(.time))

overview_optima(hmm_ino, digits = 2)

plot(hmm_ino, var = ".time", by = ".strategy")

# Example: Probit LL ------------------------------------------------------

set.seed(1)
probit_data <- list()
for(i in 1:100){
  b <- c(1,rnorm(2, sd = 3))
  Sigma <- RprobitB::rwishart(3,diag(3))$W
  name <- paste0("data",i)
  probit_data[[name]] <- sim_mnp(N = 100, b = b, Sigma = Sigma, seed = i)
}

probit_ino <- setup_ino(
  f = f_ll_mnp,
  npar = 11,
  data = probit_data[1:2],
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "data"
)

for(i in 1:10)
  probit_ino <- random_initialization(probit_ino)

for(i in 1:10)
  probit_ino <- subset_initialization(
    probit_ino, arg = "data", how = "kmeans", prop = 0.5, by_row = TRUE,
    col_ign = c(1,2), initialization = random_initialization())

summary(probit_ino)

overview_optima(probit_ino, digits = 2)

plot(probit_ino, var = ".time")

# Example: Logit LL -------------------------------------------------------

set.seed(1)
logit_data <- list()
for(i in 1:100){
  b <- rnorm(3, sd = 3)
  Omega <- RprobitB::rwishart(3,diag(3))$W
  name <- paste0("data",i)
  logit_data[[name]] <- sim_mnl(N = 300, J = 3, b = b, Omega = Omega, seed = i)
}

logit_ino <- setup_ino(
  f = f_ll_mnl,
  npar = 9,
  data = logit_data[[1]],
  R = list("R1" = 10, "R2" = 100),
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "R"
)

for(i in 1:10)
  logit_ino <- random_initialization(logit_ino)

for(i in 1:10)
  logit_ino <- subset_initialization(
    logit_ino, how = "kmeans", initialization = random_initialization())

summary(logit_ino, group = "R")

overview_optima(logit_ino, digits = 2)

plot(logit_ino, var = ".time", by = c(".strategy", "R"))

