### install
# install.packages("../ino_0.0.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()

### setup
x <- set_f(f = ino:::f_ackley, npar = 2)
x <- set_optimizer(x, "nlm")
summary(x)

### strategies
x <- random_initialization(x, runs = 10)
x <- fixed_initialization(x, at = list(c(1, 0.5, 0.3), c(2, 0.3, 1)))

### evaluation
optimization_time(x)
nr_optima(x)
nr_optima(x, plot = TRUE)
