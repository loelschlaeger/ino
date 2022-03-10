# Installation ------------------------------------------------------------

# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()

# Example: Ackley ---------------------------------------------------------

### setup
x <- set_f(f = ino:::f_ackley, npar = 2)
x <- set_optimizer(x, "nlm")
summary(x)

### strategies
x <- random_initialization(x, runs = 10)
x <- fixed_initialization(x, at = list(c(1, 0.5), c(0.3, 2), c(2, 0.3), c(1, 2)))

### evaluation
optimization_time(x)
optimization_time(x, plot_hist = TRUE)
optimization_time(x, plot_freq = TRUE)
optimization_time(x, plot_hist = TRUE, plot_freq = TRUE)
nr_optima(x)
nr_optima(x, plot = TRUE)


# Example: HMM LL ---------------------------------------------------------
library(magrittr)
earthquake_data <- read.table("http://hmms-for-time-series.de/second/data/earthquakes.txt")
colnames(earthquake_data) <- c("year", "obs")

### set number of parameters to be estimated
nr_states <- 2
nr_paras <- nr_states * (nr_states - 1) + nr_states

### set up the ino object
# set likelihood function
x <- set_f(f = ino::f_ll_hmm, npar = nr_paras, N = 2, negative = TRUE) %>%
  # set data
  set_data(list(earthquake_data, earthquake_data)) %>%
  # set optimizer
  set_optimizer("nlm")

summary(x)

x <- fixed_initialization(x, at = list(c(-1, -1, 1, 2), c(-1, -1, 0.1, 0.2)))
optimization_time(x)
nr_optima(x)


# Example: Probit LL ------------------------------------------------------

N <- 100
b <- c(-2,0.5,2)
Omega <- diag(3)
O <- t(chol(Omega))
o <- O[lower.tri(O, diag = TRUE)]
Sigma <- diag(3)
L <- t(chol(Sigma))
l <- L[lower.tri(L, diag = TRUE)][-1]
data <- sim_mmnp(N, b, o, l, seed = 1)
true <- c(b,o,l)
starting_values <- rnorm(length(true))
est <- nlm(f_ll_mmnp, p = starting_values, data = data, normal_cdf = mvtnorm::pmvnorm, negative = TRUE)$estimate
abs(true - est)

# Example: Logit LL -------------------------------------------------------

N <- 10
J <- 3
b <- c(-2,0.5,2)
Omega <- diag(3)
O <- t(chol(Omega))
o <- O[lower.tri(O, diag = TRUE)]
data <- sim_mmnl(N, J, b, o, seed = 1)
true <- c(b,o)
starting_values <- rnorm(length(true))
est <- nlm(f_ll_mmnl, p = starting_values, data = data, R = 100, negative = TRUE)$estimate
abs(true - est)


