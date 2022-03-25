# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()
library(magrittr)


# Example: Ackley ---------------------------------------------------------

x <- set_f(f = ino:::f_ackley, npar = 2) %>%
  set_optimizer("nlm") %>%
  random_initialization(runs = 10) %>%
  fixed_initialization(at = list(c(1, 0.5), c(0.3, 2), c(2, 0.3), c(1, 2)))

summary(x)

optimization_time(x)
optimization_time(x, plot_hist = TRUE)
optimization_time(x, plot_freq = TRUE)
optimization_time(x, plot_hist = TRUE, plot_freq = TRUE)
nr_optima(x)
nr_optima(x, plot = TRUE)


# Example: HMM LL ---------------------------------------------------------

x <- set_f(f = ino::f_ll_hmm, npar = 4, N = 2, negative = TRUE) %>%
  set_data(data(earthquakes)) %>%
  set_optimizer("nlm") %>%
  fixed_initialization(x, at = list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))

summary(x)
optimization_time(x)
nr_optima(x)

x <- random_initialization(x, runs = 2)
optimization_time(x)
nr_optima(x)

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

N <- 100
J <- 3
b <- c(-2,0.5,2)
Omega <- diag(3)
data <- sim_mmnl(N, J, b, Omega, seed = 1)
true <- attr(data, "true")
starting_values <- true #rnorm(length(true))
est <- nlm(f_ll_mmnl, p = starting_values, data = data, R = 100, neg = TRUE)$estimate
abs(true - est)


