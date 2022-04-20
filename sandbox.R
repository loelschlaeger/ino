# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()


# Example: Ackley ---------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ackley,
  npar = 2,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06,
                                        crit = "iterations"),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10,
                                        crit = "iterations")
             ),
  verbose = TRUE)

x <- random_initialization(x,
                           runs = 100)
x <- fixed_initialization(x,
                          at = list(c(1, 0.5), c(0.3, 2)))

# summary(x, "time")
# plot(x)
plot(x, var = ".time", by = ".strategy")
plot(x, var = ".time", by = ".strategy") + theme_minimal()
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
  verbose = TRUE)

x <- random_initialization(x,
                           runs = 10)
x <- fixed_initialization(x,
                          at = list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))
plot(x, var = ".time", by = ".strategy")

# Example: Probit LL ------------------------------------------------------

N <- 100
T <- 100
b <- c(2,-2)
Omega <- matrix(c(1,0.1,0.1,0.5),ncol = 2)
Sigma <- diag(3)
data <- ino:::sim_mmnp(N, T, b, Omega, Sigma, seed = 1)
true <- attr(data, "true")
starting_values <- true
est <- nlm(f_ll_mmnp, p = starting_values, data = data, neg = TRUE)$estimate
abs(true - est)


# Example: Logit LL -------------------------------------------------------

x <- setup_ino(
  f = ino:::f_ll_mmnl,
  npar = 9,
  data = ino::logit_data,
  R = list("R1" = 100,
           "R2" = 1000
           ),
  neg = TRUE,
  opt = list("opt1" = set_optimizer_nlm(gradtol = 1e-06,
                                        crit = "iterations"),
             "opt2" = set_optimizer_nlm(gradtol = 1e-10,
                                        crit = "iterations")
             ),
  mpvs = c("data","R"),
  verbose = TRUE
)

x <- random_initialization(x,
                           runs = 2)
x <- fixed_initialization(x,
                          at = list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))
