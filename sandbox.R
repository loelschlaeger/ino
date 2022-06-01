# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()


# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ackley,
  npar = 2,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10)),
  verbose = F)

for(i in 1:5) x <- random_initialization(x)
for(at in list(c(1, 0.5), c(0.3, 2))) x <- fixed_initialization(x, at = at)

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

x <- setup_ino(
  f = ino:::f_ll_hmm,
  npar = 4,
  data = ino::earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm(),
  verbose = F)

for(i in 1:5) x <- random_initialization(x)
for(at in list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2))) x <- fixed_initialization(x, at = at)

summary(x, "mean" = mean(.time))
plot(x, var = ".time", by = ".strategy")

# Example: Probit LL ------------------------------------------------------

probit_ino <- setup_ino(
  f = ino:::f_ll_mnp,
  npar = 11,
  data = ino::probit_data,
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "data",
  verbose = F)

probit_ino <- random_initialization(probit_ino)

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
