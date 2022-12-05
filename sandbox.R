# Installation ------------------------------------------------------------

# path <- "../ino_0.1.0.9000.tar.gz"
# install.packages(path, repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))
devtools::load_all()
library("tidyverse")
options(ino_ncores = parallel::detectCores() - 1)
# Sys.setenv(LANG = "en")


# Example: Ackley function ------------------------------------------------

ackley <- Nop$new(f = f_ackley, npar = 2)$
  set_true_par(true_par = c(0, 0), set_true_val = TRUE)$
  set_optimizer(optimizer = optimizer_nlm(), label = "nlm")$
  set_optimizer(optimizer = optimizer_optim(), label = "optim")$
  print()

ackley$test()

ackley$evaluate(c(0,0))

ackley$optimize(initial = c(-3,3), runs = 1)

ackley$overview_optima(digits = 2)




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


# Example: HMM LL for earthquake data -------------------------------------

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


# Example: HMM LL for financial data --------------------------------------

library("fHMM")
file <- tempfile()
fHMM::download_data(symbol = "DBK.DE", from = "2020-01-01", file = file)

library("dplyr")
db_data <- read.csv(file) %>%
  as_tibble() %>%
  summarize(date = as.Date(Date, format = "%Y-%m-%d"),
            obs = c(NA, diff(log(Close), lag=1) * 100)) %>%
  filter(!is.na(obs)) %>%
  print()

library("ggplot2")
ggplot(db_data, aes(x = date, y = obs)) +
  geom_point() +
  geom_line() +
  ylab("log-returns [%]")

library("ino")
hmm_ino <- setup_ino(
  f = f_ll_hmm,
  npar = 6,
  data = db_data,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm()
)

sampler <- function() c(log(stats::runif(2, 0.1, 0.9)),
                        stats::rnorm(2),
                        log(stats::runif(2, 0.5, 2)))

hmm_ino <- random_initialization(hmm_ino, runs = 100, sampler = sampler)

starting_values <- list(c(-2, -2, 0, 0, log(2), log(3)),
                        c(-1.5, -1.5, -2, 2, log(1), log(2)),
                        c(-1, -1, -3, 3, log(2), log(2)))
for(val in starting_values)
  hmm_ino <- fixed_initialization(hmm_ino, at = val)

for(prop in c(0.05, 0.25, 0.5))
  hmm_ino <- subset_initialization(
    hmm_ino, arg = "data", how = "first", prop = prop,
    initialization =  random_initialization(runs = 100, sampler = sampler)
  )

overview_optima(hmm_ino)

plot(hmm_ino, by = ".strategy")


# Example: Probit LL ------------------------------------------------------

N <- 100
T <- 10
J <- 3
P <- 3
b <- c(1,-1,0.5)
Sigma <- diag(J)
X <- function() {
  class <- sample(0:1, 1)
  mean <- ifelse(class, 2, -2)
  matrix(stats::rnorm(J*P, mean = mean), nrow = J, ncol = P)
}
probit_data <- replicate(10, sim_mnp(
  N = N, T = T, J = J, P = P, b = b, Sigma = Sigma, X = X
), simplify = FALSE)
true <- attr(probit_data[[1]], "true")[-1]

probit_ino <- setup_ino(
  f = f_ll_mnp,
  npar = 5,
  global = true,
  data = probit_data,
  neg = TRUE,
  mpvs = "data",
  opt = set_optimizer_nlm(iterlim = 1000)
)

probit_ino <- random_initialization(probit_ino, runs = 100)

saveRDS(probit_ino, "probit_ino.rds")

for(how in c("random", "kmeans")) for(prop in c(0.2,0.5)) {
  probit_ino <- subset_initialization(
    probit_ino, arg = "data", how = how, prop = prop,
    ind_ign = 1:3, initialization = random_initialization(runs = 100)
  )
}

saveRDS(probit_ino, "probit_ino.rds")

probit_ino <- standardize_initialization(
  probit_ino, ind_ign = 1:3, initialization = random_initialization(runs = 100)
)

saveRDS(probit_ino, "probit_ino.rds")

probit_ino <- update_opt(
  probit_ino,
  opt = set_optimizer_ao(partition = list(1:2, 1:5))
)

probit_ino <- random_initialization(probit_ino, runs = 100)

saveRDS(probit_ino, "probit_ino.rds")

overview_optima(probit_ino, digits = 2)

plot(probit_ino, by = c(".strategy",".optimizer"))

