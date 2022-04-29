# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()


# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ackley,
  npar = 2,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06,
                                        crit = c("iterations","minimum")),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10,
                                        crit = c("iterations","minimum"))
             ),
  verbose = TRUE)

x <- random_initialization(x,
                           runs = 100)
x <- fixed_initialization(x,
                          at = list(c(1, 0.5), c(0.3, 2)))

summary(x, var = "minimum", "count" = n())
plot(x, var = ".time", by = ".strategy")
plot(x, var = ".time", by = ".strategy") + ggplot2::theme_minimal()
plot(x, var = ".time", by = ".optimizer", type = "histogram")
## this should throw an error
plot(x, var = ".time", by = c(".optimizer", ".strategy"))

# Example: HMM LL ---------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ll_hmm,
  npar = 4,
  data = ino::earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm(),
  verbose = F)

x <- random_initialization(x,
                           runs = 10)
x <- fixed_initialization(x,
                          at = list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))

summary(x, "mean" = mean)
plot(x, var = ".time", by = ".strategy")

# Example: Probit LL ------------------------------------------------------

probit_ino <- setup_ino(
  f = ino:::f_ll_mmnp,
  npar = 11,
  data = ino::probit_data[1:2],
  neg = TRUE,
  opt = set_optimizer_nlm(),
  mpvs = "data",
  verbose = TRUE
)

probit_ino <- random_initialization(probit_ino,
                                    runs = 2)

summary(probit_ino, "mean" = mean)
plot(probit_ino, var = ".time")


# Example: Logit LL -------------------------------------------------------

logit_ino <- setup_ino(
  f = ino:::f_ll_mmnl,
  npar = 9,
  data = ino::logit_data[1:2],
  R = list("R1" = 10,
           "R2" = 100
           ),
  neg = TRUE,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06,
                                        iterlim = 2,
                                        crit = "iterations"),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10,
                                        iterlim = 2,
                                        crit = "iterations")
             ),
  mpvs = c("data","R"),
  verbose = TRUE
)

logit_ino <- random_initialization(logit_ino,
                                   runs = 2)

summary(logit_ino, group = "R", "mean" = mean)
plot(logit_ino, var = ".time", by = "R")
