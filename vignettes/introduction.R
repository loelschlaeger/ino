## ---- setup, include = FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.path = "figures/introduction-",
  fig.dim = c(8, 6),
  out.width = "75%"
)
devtools::load_all()
options("ino_verbose" = TRUE)
set.seed(1)


## ---- faithful data--------------------------------------------------
str(faithful)


## ---- faithful, warning = FALSE--------------------------------------
library("ggplot2")
ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  xlab("eruption time (min)")


## ---- mixture ll-----------------------------------------------------
normal_mixture_llk <- function(theta, data, neg = TRUE){
  stopifnot(length(theta) == 5)
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  llk <- sum(log(lambda * dnorm(data, mu[1], sd[1]) + (1 - lambda) * dnorm(data, mu[2], sd[2])))
  ifelse(neg, -llk, llk)
}
normal_mixture_llk(theta = 1:5, data = faithful$eruptions)


## ---- em algorithm---------------------------------------------------
em <- function(normal_mixture_llk, theta, epsilon = 1e-08, iterlim = 1000, data) {
  llk <- normal_mixture_llk(theta, data, neg = FALSE)
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  for (i in 1:iterlim) {
    class_1 <- lambda * dnorm(data, mu[1], sd[1])
    class_2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
    posterior <- class_1 / (class_1 + class_2)
    lambda <- mean(posterior)
    mu[1] <- mean(posterior * data) / lambda
    mu[2] <- (mean(data) - lambda * mu[1]) / (1 - lambda)
    sd[1] <- sqrt(mean(posterior * (data - mu[1])^2) / lambda)
    sd[2] <- sqrt(mean((1 - posterior) * (data - mu[2])^2) / (1 - lambda))
    llk_old <- llk
    theta <- c(mu, log(sd), qlogis(lambda))
    llk <- normal_mixture_llk(theta, data, neg = FALSE)
    if (is.na(llk)) stop("fail")
    if (llk - llk_old < epsilon) break
  }
  list("neg_llk" = -llk, "estimate" = theta, "iterations" = i)
}


## ---- initialize geyser----------------------------------------------
geyser <- Nop$new(
  f = normal_mixture_llk,
  npar = 5,
  data = faithful$eruptions
)


## ---- print geyser---------------------------------------------------
print(geyser)


## ---- geyser optimizer-----------------------------------------------
geyser$
  set_optimizer(optimizer_nlm(), label = "nlm")$
  set_optimizer(optimizer_optim(), label = "optim")


## ---- set em algorithm-----------------------------------------------
em_optimizer <- optimizeR::define_optimizer(
  optimizer = em, objective = "normal_mixture_llk",
  initial = "theta", value = "neg_llk", parameter = "estimate"
)
geyser$set_optimizer(em_optimizer, label = "em")


## ---- validate geyser------------------------------------------------
geyser$test(verbose = TRUE)


## ---- example evaluation---------------------------------------------
geyser$evaluate(at = 1:5)


## ---- example optimization-------------------------------------------
geyser$optimize(initial = "random", which_optimizer = "nlm", save_result = FALSE, return_result = TRUE)


## ---- random initialization------------------------------------------
geyser$optimize(initial = "random", runs = 100, label = "random", save_results = TRUE, seed = 1)


## ---- show optima----------------------------------------------------
geyser$optima(digits = 0, sort_by = "value")


## ---- check assumptions about optimization results, include = FALSE----
optima <- geyser$optima(digits = 0, sort_by = "value")
stopifnot(as.numeric(as.character(optima[1, "value"])) == 276)
most_occuring <- geyser$optima(digits = 0, sort_by = "frequency")[1:2, ]
most_occuring_value <- as.numeric(as.character(most_occuring$value))
stopifnot(most_occuring_value == c(421, 276))


## ---- show optima optimizer-wise-------------------------------------
geyser$optima(digits = 0, sort_by = "value", which_optimizer = "nlm")
geyser$optima(digits = 0, sort_by = "value", which_optimizer = "optim")
geyser$optima(digits = 0, sort_by = "value", which_optimizer = "em")


## ---- closest parameters---------------------------------------------
(mle <- geyser$closest_parameter(value = 276, which_optimizer = "nlm"))
geyser$evaluate(at = as.vector(mle))
mle_run <- attr(mle, "run")
(bad <- geyser$closest_parameter(value = 421, which_optimizer = "nlm"))
geyser$evaluate(at = as.vector(bad))
bad_run <- attr(bad, "run")


## ---- transform parameter--------------------------------------------
transform <- function(theta) c(theta[1:2], exp(theta[3:4]), plogis(theta[5]))
(mle <- transform(mle))
(bad <- transform(bad))


## ---- estimated-mixtures---------------------------------------------
mixture_density <- function (data, mu, sd, lambda) {
  lambda * dnorm(data, mu[1], sd[1]) + (1 - lambda) * dnorm(data, mu[2], sd[2])
}
ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  labs(x = "eruption time (min)", colour = "parameter") +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = mle[1:2], sd = mle[3:4], lambda = mle[5])
    }, aes(color = "mle"), linewidth = 1
  ) +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = bad[1:2], sd = bad[3:4], lambda = bad[5])
    }, aes(color = "bad"), linewidth = 1
  )


## ---- extract gradients----------------------------------------------
geyser$results(which_run = c(mle_run, bad_run), which_optimizer = "nlm", which_element = "gradient")


## ---- custom sampler-------------------------------------------------
sampler <- function() stats::rnorm(5, mean = 2, sd = 0.5)
geyser$optimize(initial = sampler, runs = 100, label = "custom_sampler")


## ---- summary of custom sampler results------------------------------
summary(geyser, which_run = "custom_sampler", digits = 2) |>
  head(n = 10)


## ---- check assumptions about optimization results 2, include = FALSE----
noptima_new <- nrow(geyser$optima(digits = 0, sort_by = "value", which_run = "custom_sampler"))
noptima_old <- nrow(geyser$optima(digits = 0, sort_by = "value", which_run = "random"))
stopifnot(noptima_new > noptima_old)
most_occuring <- geyser$optima(digits = 0, sort_by = "frequency", which_run = "custom_sampler")[1, ]
most_occuring_value <- as.numeric(as.character(most_occuring$value))
stopifnot(most_occuring_value == 276)


## ---- overview optima for custom sampler-----------------------------
geyser$optima(digits = 0, sort_by = "value", which_run = "custom_sampler")


## ---- fixed starting values------------------------------------------
mu_1 <- c(1.7, 2.3)
mu_2 <- c(4.3, 3.7)
sd_1 <- sd_2 <- c(log(0.8), log(1.2))
lambda <- c(qlogis(0.4), qlogis(0.6))
starting_values <- asplit(expand.grid(mu_1, mu_2, sd_1, sd_2, lambda), MARGIN = 1)


## ---- optimization with educated guesses-----------------------------
geyser$optimize(initial = starting_values, label = "educated_guess")


## ---- overview optima for educated guesses---------------------------
geyser$optima(digits = 0, which_run = "educated_guess")


## ---- bad educated guess---------------------------------------------
geyser$optimize(initial = rep(0, 5), label = "bad_educated_guess")
summary(geyser, which_run = "bad_educated_guess")


## ---- clear optimization results-------------------------------------
geyser$clear(which_run = "bad_educated_guess")


## ---- standardize data-----------------------------------------------
geyser$standardize("data")
str(geyser$get_argument("data"))


## ---- optimization with standardized data----------------------------
geyser$
  optimize(runs = 100, label = "data_standardized")$
  reset_argument("data")


## ---- reduce data----------------------------------------------------
geyser$reduce(argument_name = "data", how = "random", prop = 0.3, seed = 1)
str(geyser$get_argument("data"))


## ---- optimization with reduced data---------------------------------
geyser$
  optimize(runs = 100, label = "data_subset")$
  reset_argument("data")$
  continue()


## ---- plot-by-label--------------------------------------------------
geyser$plot(by = "label", relative = TRUE)


## ---- plot-by-optimizer----------------------------------------------
geyser$plot(by = "optimizer", relative = FALSE)


## ---- print final geyser object--------------------------------------
print(geyser)


## ---- extract best value and parameter-------------------------------
geyser$best_value()
geyser$best_parameter()

