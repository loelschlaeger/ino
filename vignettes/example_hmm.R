## ----download dax data, message = FALSE, warning = FALSE, eval = TRUE--------------------------------------------------------
library("fHMM")
library("dplyr")
dax <- download_data(symbol = "^GDAXI", from = "1990-01-01", to = "2020-01-01") %>%
  as_tibble() %>%
  reframe(
    date = as.Date(Date, format = "%Y-%m-%d"),
    logreturn = c(NA, diff(log(Close), lag = 1))
  ) %>%
  filter(!is.na(logreturn)) %>%
  print()


## ----plot-dax-data, message = FALSE, warning = FALSE, eval = TRUE------------------------------------------------------------
library("ggplot2")
ggplot(dax, aes(x = date, y = logreturn)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent())


## ----define ino object-------------------------------------------------------------------------------------------------------
Nop_hmm <- Nop$new(
  f = fHMM::ll_hmm, 
  npar = 6, 
  observations = dax$logreturn,
  sdds = "normal",
  states = 2, 
  negative = TRUE
)


## ----set optimizer-----------------------------------------------------------------------------------------------------------
Nop_hmm$set_optimizer(optimizeR::Optimizer$new("stats::nlm"))


## ----parallel setting--------------------------------------------------------------------------------------------------------
future::plan(future::multisession, workers = 10)


## ----progress setting--------------------------------------------------------------------------------------------------------
progressr::handlers(global = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------
Nop_hmm$
  initialize_random(runs = 100)$
  optimize(optimization_label = "random_naive")


## ----naive random initialization---------------------------------------------------------------------------------------------
sampler <- function() {
  c(stats::runif(2, -2, -1), stats::rnorm(2), log(stats::runif(2, 0.5, 2)))
}


## ----random initialization---------------------------------------------------------------------------------------------------
Nop_hmm$
  initialize_random(sampler = sampler, runs = 100)$
  optimize(optimization_label = "random")


## ----optimization of educated guesses----------------------------------------------------------------------------------------
Nop_hmm$
  initialize_grid(
    lower = c(-2, -2, -0.1, -0.1, log(0.1), log(0.1)),
    upper = c(-1, -1, 0.1, 0.1, log(1), log(1)),
    breaks = 2
  )$
  optimize(optimization_label = "educated_guess")


## ----subset initialization---------------------------------------------------------------------------------------------------
Nop_hmm$
  reduce_argument("observations", how = "first", proportion = 0.25)$
  initialize_random(sampler = sampler, runs = 100)$
  optimize(optimization_label = "reduced")$
  fixed_argument("reset", argument_name = "observations")$
  initialize_continue("reduced")$
  optimize(optimization_label = "initialized_reduced")


## ----standardize initialization 1, eval = TRUE-------------------------------------------------------------------------------
Nop_hmm$standardize_argument("observations")


## ----standardize initialization values, eval = TRUE--------------------------------------------------------------------------
observations <- Nop_hmm$fixed_argument("get", "observations")
(center <- attr(observations, "center"))
(scale <- attr(observations, "scale"))


## ----standardize initialization 2--------------------------------------------------------------------------------------------
Nop_hmm$
  initialize_random(sampler = sampler, runs = 100)$
  optimize(optimization_label = "standardized")$
  fixed_argument(action = "reset", argument_name = "observations")


## ----transform, eval = TRUE--------------------------------------------------------------------------------------------------
transform <- function(x) {
  c(x[1:2], x[3:4] * scale + center, log(exp(x[3:4]) * scale + center))
}


## ----overview of optima, eval = TRUE-----------------------------------------------------------------------------------------
Nop_hmm$optima(sort_by_value = TRUE, digits = 0)


## ----get number of converged runs, include = FALSE, eval = TRUE--------------------------------------------------------------
library("dplyr")
optima <- Nop_hmm$optima(sort_by_value = TRUE, digits = 0)
global <- optima |> arrange(value) |> slice(1) |> pull(n)
total <- sum(optima$n)
local <- total - global


## ----summary of results, eval = TRUE-----------------------------------------------------------------------------------------
Nop_hmm$results |> select(value, parameter, seconds)


## ----best parameter, eval = TRUE---------------------------------------------------------------------------------------------
Nop_hmm$minimum


## ----proportion of converged runs, eval = TRUE-------------------------------------------------------------------------------
Nop_hmm$results |> 
  filter(.original) |>
  mutate(global_optimum = value < -22445) |>
  group_by(.optimization_label) |>
  summarise(proportion = mean(global_optimum, na.rm = TRUE))


## ----optimization-time, eval = TRUE, warning = FALSE, message = FALSE--------------------------------------------------------
Nop_hmm$results |> 
  autoplot(group_by = "optimization", relative = TRUE)


## ----summary statistics, warning = FALSE, message = FALSE, eval = TRUE-------------------------------------------------------
Nop_hmm$results |>
  group_by(.optimization_label) %>%
  summarise(
    median_seconds = median(seconds, na.rm = TRUE),
    sd_seconds = sd(seconds, na.rm = TRUE)
  ) %>%
  arrange(median_seconds)

