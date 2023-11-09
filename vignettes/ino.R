## ---- setup, include = FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.align = "center",
  fig.path  = "figures/ino-",
  fig.dim   = c(8, 6),
  out.width = "75%"
)


## ---- load ino---------------------------------------------------------------------------------
# install.packages("ino")
library("ino")


## ---- faithful plot, warning = FALSE-----------------------------------------------------------
library("ggplot2")
ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  xlab("eruption time (min)")


## ---- define mixture ll------------------------------------------------------------------------
normal_mixture_llk <- function(mu, sd, lambda, data) {
  sd <- exp(sd)
  lambda <- plogis(lambda)
  sum(log(lambda * dnorm(data, mu[1], sd[1]) + (1 - lambda) * dnorm(data, mu[2], sd[2])))
}
normal_mixture_llk(mu = 1:2, sd = 3:4, lambda = 5, data = faithful$eruptions)


## ---- initialize Nop, eval = FALSE-------------------------------------------------------------
Nop_mixture <- Nop$new(
  objective = normal_mixture_llk,    # the objective function
  target = c("mu", "sd", "lambda"),  # names of target arguments
  npar = c(2, 2, 1),                 # lengths of target arguments
  data = faithful$eruptions          # values for fixed arguments
)

self <- Nop_mixture
private <- self$.__enclos_env__$private

## ---- example evaluation, eval = FALSE---------------------------------------------------------
Nop_mixture$evaluate(at = 1:5) # same value as above


## ---- define optimizer, eval = FALSE-----------------------------------------------------------
## nlm <- optimizeR::Optimizer$new("stats::nlm")
## Nop_mixture$set_optimizer(nlm)


## ---- example optimization, eval = FALSE-------------------------------------------------------
## Nop_mixture$
##   initialize_random(runs = 100)$
##   optimize(which_direction = "max")


## ---- access results, eval = FALSE-------------------------------------------------------------
## Nop_mixture$results(which_run = 42, which_element = c("value", "parameter"))


## ---- overview optima, eval = FALSE------------------------------------------------------------
## Nop_mixture$optima()


## ---- define em algorithm----------------------------------------------------------------------
em <- function(normal_mixture_llk, theta, epsilon = 1e-08, iterlim = 1000, data) {
  llk <- normal_mixture_llk(theta, data)
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
    llk <- normal_mixture_llk(theta, data)
    if (is.na(llk)) stop("em failed")
    if (llk - llk_old < epsilon) break
  }
  list("llk" = llk, "estimate" = theta, "iterations" = i)
}


## ---- set em algorithm, eval = FALSE-----------------------------------------------------------
## em_optimizer <- optimizeR::define_optimizer(
##   .optimizer = em, .objective = "normal_mixture_llk",
##   .initial = "theta", .value = "neg_llk", .parameter = "estimate",
##   .direction = "max"
## )
## Nop_mixture$
##   set_optimizer(em_optimizer, optimizer_label = "em")

