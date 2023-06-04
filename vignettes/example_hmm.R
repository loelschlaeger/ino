## ---- setup, include = FALSE----------------------------------------------------------------------------------------------------------------
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
library("ino")
options("ino_verbose" = TRUE) 
data("hmm_ino")
set.seed(1)
ggplot2::theme_set(ggplot2::theme_minimal())


## ---- download dax data, message = FALSE, warning = FALSE, eval = TRUE----------------------------------------------------------------------
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


## ---- plot-dax-data, message = FALSE, warning = FALSE, eval = TRUE--------------------------------------------------------------------------
library("ggplot2")
ggplot(dax, aes(x = date, y = logreturn)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent())


## ---- define ino object---------------------------------------------------------------------------------------------------------------------
hmm_ino <- Nop$new(
  f = f_ll_hmm, 
  npar = 6, 
  data = dax$logreturn, 
  N = 2, 
  neg = TRUE
)$
set_optimizer(optimizer_nlm())


## ---- random initialization-----------------------------------------------------------------------------------------------------------------
sampler <- function() {
  c(stats::runif(2, -2, -1), stats::rnorm(2), log(stats::runif(2, 0.5, 2)))
}
hmm_ino$optimize(initial = sampler, runs = 100, label = "random")


## ---- grid of educated guesses, eval = TRUE-------------------------------------------------------------------------------------------------
tpm_entry_1 <- tpm_entry_2 <- c(-2, -2.5)
mu_1 <- c(0, -0.05)
mu_2 <- c(0, 0.05)
sd_1 <- c(log(0.1), log(0.5))
sd_2 <- c(log(0.75), log(1))
starting_values <- asplit(expand.grid(
  tpm_entry_1, tpm_entry_2, mu_1, mu_2, sd_1, sd_2), 
  MARGIN = 1
)


## ---- optimization of educated guesses------------------------------------------------------------------------------------------------------
hmm_ino$optimize(initial = starting_values, label = "educated_guess")


## ---- subset initialization first-----------------------------------------------------------------------------------------------------------
hmm_ino$
  reduce("data", how = "first", prop = 0.25)$
  optimize(initial = sampler, runs = 100, label = "subset")$
  reset_argument("data")$
  continue()


## ---- standardize initialization------------------------------------------------------------------------------------------------------------
hmm_ino$
  standardize("data")$
  optimize(initial = sampler, runs = 100, label = "standardize")$
  reset_argument("data")


## ---- overview of optima, eval = TRUE-------------------------------------------------------------------------------------------------------
hmm_ino$optima(sort_by = "value", only_comparable = TRUE, digits = 0)


## ---- get number of converged runs, include = FALSE, eval = TRUE----------------------------------------------------------------------------
library("dplyr")
optima <- hmm_ino$optima(sort_by = "value", only_comparable = TRUE, digits = 0)
global <- optima %>% arrange(value) %>% slice(1) %>% pull(frequency)
total <- sum(optima$frequency)
local <- total - global


## ---- summary of results, eval = TRUE-------------------------------------------------------------------------------------------------------
summary(hmm_ino, which_element = c("value", "parameter", "seconds")) %>% 
  head(n = 10)


## ---- best parameter, eval = TRUE-----------------------------------------------------------------------------------------------------------
hmm_ino$best_parameter()


## ---- best value, eval = TRUE---------------------------------------------------------------------------------------------------------------
hmm_ino$best_value()


## ---- proportion of converged runs, eval = TRUE---------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds"), global_optimum = "value < -22445", only_comparable = TRUE) %>% 
  group_by(label) %>% 
  summarise(proportion = mean(global_optimum, na.rm = TRUE))


## ---- optimization-time, eval = TRUE--------------------------------------------------------------------------------------------------------
plot(hmm_ino, by = "label")


## ---- summary statistics, warning = FALSE, message = FALSE, eval = TRUE---------------------------------------------------------------------
summary(hmm_ino, c("label", "seconds")) %>% 
  group_by(label) %>% 
  summarise(
    median_seconds = median(seconds, na.rm = TRUE),
    sd_seconds = sd(seconds, na.rm = TRUE)
  ) %>%
  arrange(median_seconds)

