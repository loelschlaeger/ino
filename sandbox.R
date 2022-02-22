### install
# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()

### setup
x <- set_f(f = ino:::f_ackley, npar = 2)
x <- set_optimizer(x, "nlm")
summary(x)

### strategies
x <- random_initialization(x, runs = 10)
x <- fixed_initialization(x, at = list(c(1, 0.5, 0.3, 2), c(2, 0.3, 1, 2)))

### evaluation
optimization_time(x)
nr_optima(x)
nr_optima(x, plot = TRUE)

### fit HMM
earthquake_data <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")
colnames(earthquake_data) <- c("year", "obs")

# set number of states N
nr_states <- 2
# set number of parameters: N * (N-1) for the transition probability matrix +
# N for the state-dependent distributions
nr_paras <- nr_states * (nr_states - 1) + nr_states
x <- set_f(f = ino::f_ll_hmm, npar = nr_paras)
# set_data...
starting_values <- c(-1, -1, 1, 2)
mod <- nlm(f_ll_hmm, starting_values, earthquake_data, N = nr_states, print.level = 2, iterlim = 1000)
mod$estimate
exp(mod$estimate)

# bad starting values:
starting_values <- c(-1, -1, 0.1, 0.2)
mod <- nlm(f_ll_hmm, starting_values, earthquake_data, N = nr_states, print.level = 2, iterlim = 1000)
exp(mod$estimate)

