# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()


# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ackley,
  npar = 2,
  opt = list("nlm"   = set_optimizer_nlm(),
             "optim" = set_optimizer_optim()),
  verbose = FALSE)

x <- true_ino(x, par = c(0,0), overview = FALSE)

for(i in 1:5)
  x <- random_initialization(x)

for(at in list(c(1, 0.5), c(0.3, 2)))
  x <- fixed_initialization(x, at = at)

summary(x, group = c(), var = "minimum", "count" = n())
summary(x, "count" = n())
summary(x, "count" = n(), "average_time" = mean(.time))
summary(x, group = ".strategy", "count" = n(), "average_time" = mean(.time))
summary(x)
plot(x, var = ".time", by = ".strategy")
plot(x, var = ".time", by = ".strategy") + ggplot2::theme_minimal()
plot(x, var = ".time", by = ".optimizer", type = "histogram")
plot(x, var = ".time", by = ".optimizer", type = "barplot")
plot(x, var = ".time", by = c(".optimizer", ".strategy"))
nr_optima(x, round = 2)

# Example: HMM LL ---------------------------------------------------------

hmm_ino <- setup_ino(
  f = ino:::f_ll_hmm,
  npar = 4,
  data = ino::earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm(),
  verbose = F)

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
plot(hmm_ino, var = ".time", by = ".strategy")

# Example: Probit LL ------------------------------------------------------

probit_ino <- setup_ino(
  f = ino:::f_ll_mnp,
  npar = 11,
  data = ino::probit_data,
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "data",
  verbose = FALSE)

probit_ino <- true_ino(probit_ino, par = lapply(ino::probit_data, attr, "true"))

probit_ino <- random_initialization(probit_ino)

probit_ino <- subset_initialization(probit_ino, how = "kmeans",
                                    initialization = random_initialization())

summary(probit_ino, "mean" = mean)
plot(probit_ino, var = ".time")


# Example: Logit LL -------------------------------------------------------

logit_ino <- setup_ino(
  f = ino:::f_ll_mnl,
  npar = 9,
  data = ino::logit_data[1:2],
  R = list("R1" = 10, "R2" = 100),
  neg = TRUE,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06, iterlim = 2, crit = "iterations"),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10, iterlim = 2, crit = "iterations")),
  mpvs = c("data","R"),
  verbose = TRUE
)

logit_ino <- random_initialization(logit_ino, runs = 2)

summary(logit_ino, group = "R", "mean" = mean)
plot(logit_ino, var = ".time", by = "R")




standardize_initialization(
  x, initialization = subset_initialization(
    initialization = fixed_initialization())
)
