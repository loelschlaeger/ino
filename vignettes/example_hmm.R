knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.path = "figures/hmm-",
  fig.dim = c(8, 6),
  out.width = "75%",
  # all optimizations are pre-computed to save building time
  eval = FALSE
)
devtools::load_all()
data("Nop_hmm")
set.seed(1)
ggplot2::theme_set(ggplot2::theme_minimal())

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

library("ggplot2")
ggplot(dax, aes(x = date, y = logreturn)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent())

Nop_hmm <- Nop$new(
  objective = fHMM::ll_hmm,
  npar = 6,
  observations = dax$logreturn,
  sdds = "normal",
  states = 2,
  negative = TRUE
)

Nop_hmm$set_optimizer(optimizeR::optimizer_nlm())

future::plan(future::multisession, workers = 10)

progressr::handlers(global = TRUE)

Nop_hmm$
  initialize_random(runs = 100)$
  optimize(optimization_label = "random_naive")

sampler <- function() {
  c(stats::runif(2, -2, -1), stats::rnorm(2), log(stats::runif(2, 0.5, 2)))
}

Nop_hmm$
  initialize_random(sampler = sampler, runs = 100, seed = 1)$
  optimize(optimization_label = "random")

Nop_hmm$
  initialize_grid(
    lower = c(-2, -2, -0.1, -0.1, log(0.1), log(0.1)),
    upper = c(-1, -1, 0.1, 0.1, log(1), log(1)),
    breaks = 2
  )$
  optimize(optimization_label = "educated_guess")

Nop_hmm$
  reduce("observations", how = "first", proportion = 0.25)$
  initialize_random(sampler = sampler, runs = 100, seed = 1)$
  optimize(optimization_label = "reduced")$
  fixed_argument("reset", argument_name = "observations")$
  initialize_continue("reduced")$
  optimize(optimization_label = "initialized_reduced")

Nop_hmm$standardize("observations")

observations <- Nop_hmm$get_argument("observations")
(center <- attr(observations, "center"))
(scale <- attr(observations, "scale"))

Nop_hmm$
  initialize_random(sampler = sampler, runs = 100, seed = 1)$
  optimize(optimization_label = "standardized")$
  fixed_argument(action = "reset", argument_name = "observations")

transform <- function(x) {
  c(x[1:2], x[3:4] * scale + center, log(exp(x[3:4]) * scale + center))
}

Nop_hmm$optima(sort_by = "value", digits = 0)

library("dplyr")
optima <- Nop_hmm$optima(sort_by = "value", digits = 0)
global <- optima %>% arrange(value) %>% slice(1) %>% pull(frequency)
total <- sum(optima$frequency)
local <- total - global

Nop_hmm$summary(which_element = c("value", "parameter", "seconds"), digits = 2) %>%
  head(n = 10)

Nop_hmm$elements()

Nop_hmm$best("parameter")

Nop_hmm$best("value")

Nop_hmm$summary(
  which_element = c("value", "seconds"),
  which_run = "comparable",
  add_identifier = ".optimization_label"
) %>%
  mutate(global_optimum = value < -22445) %>%
  group_by(.optimization_label) %>%
  summarise(proportion = mean(global_optimum, na.rm = TRUE))

Nop_hmm$plot(
  which_run = c("comparable", "standardized", logical = "or"),
  group_by = ".optimization_label", relative = TRUE
)

Nop_hmm$summary(
  which_element = "seconds",
  which_run = c("comparable", "standardized", logical = "or"),
  add_identifier = ".optimization_label"
) %>%
  group_by(.optimization_label) %>%
  summarise(
    median_seconds = median(seconds, na.rm = TRUE),
    sd_seconds = sd(seconds, na.rm = TRUE)
  ) %>%
  arrange(median_seconds)
