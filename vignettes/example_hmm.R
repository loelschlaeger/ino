## ---- include = FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "75%",
  eval = FALSE
)
# library("ino")
devtools::load_all() # remove later
set.seed(1)
options("ino_ncores" = 3)
options("ino_verbose" = TRUE)
# hmm_ino <- ino::hmm_ino # add later


## ---- download dax data, message = FALSE, warning = FALSE, eval = TRUE-------------------------------------
library("dplyr")
dax <- fHMM::download_data(symbol = "^GDAXI", from = "1990-01-01", to = "2020-01-01") %>%
  as_tibble() %>%
  reframe(
    date = as.Date(Date, format = "%Y-%m-%d"),
    logreturn = c(NA, diff(log(Close), lag = 1))
  ) %>%
  filter(!is.na(logreturn)) %>%
  print()


## ---- plot dax data, message = FALSE, warning = FALSE, eval = TRUE-----------------------------------------
library("ggplot2")
ggplot(dax, aes(x = date, y = logreturn)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent())


## ---- define ino object------------------------------------------------------------------------------------
hmm_ino <- Nop$new(
  f = f_ll_hmm,
  npar = 6,
  data = dax$logreturn,
  N = 2,
  neg = TRUE
)$
set_optimizer(optimizer_nlm())


## ---- random initialization--------------------------------------------------------------------------------
sampler <- function() {
  c(stats::runif(2, -2, -1), stats::rnorm(2), log(stats::runif(2, 0.5, 2)))
}
hmm_ino$optimize(initial = sampler, runs = 100, label = "random")


## ---- educated guesses-------------------------------------------------------------------------------------
tpm_entry_1 <- tpm_entry_2 <- c(-2, -2.5)
mu_1 <- c(0, -0.05)
mu_2 <- c(0, 0.05)
sd_1 <- c(log(0.1), log(0.5))
sd_2 <- c(log(0.75), log(1))

starting_values <- asplit(expand.grid(
  tpm_entry_1, tpm_entry_2, mu_1, mu_2, sd_1, sd_2),
  MARGIN = 1
)

hmm_ino$optimize(initial = starting_values, label = "educated_guess")


## ---- subset initialization--------------------------------------------------------------------------------
hmm_ino$
  reduce("data", how = "first", prop = 1/3)$
  optimize(initial = sampler, runs = 100, label = "subset_first")$
  reset_argument("data")$
  continue()

hmm_ino$
  reduce("data", how = "last", prop = 1/3)$
  optimize(initial = sampler, runs = 100, label = "subset_last")$
  reset_argument("data")$
  continue()


## ---- standardize initialization---------------------------------------------------------------------------
hmm_ino$
  standardize("data")$
  optimize(initial = sampler, runs = 100, label = "standardize")$
  reset_argument("data")


## ---- overview of optima, eval = TRUE----------------------------------------------------------------------
hmm_ino$optima()


## ---- summary of results, eval = TRUE----------------------------------------------------------------------
hmm_ino$summary(c("value", "seconds", "label")) %>%
  head(n = 10)


## ---- best parameter, eval = TRUE--------------------------------------------------------------------------
hmm_ino$best_parameter()


## ---- best value, eval = TRUE------------------------------------------------------------------------------
hmm_ino$best_value()


## ---- optimization time, eval = TRUE-----------------------------------------------------------------------
plot(hmm_ino, by = "label")


## ---- warning = FALSE, message = FALSE, eval = TRUE--------------------------------------------------------
summary(hmm_ino, c("label", "seconds")) %>%
  group_by(label) %>%
  summarise(avg_time = mean(seconds, na.rm = TRUE))


## ---- eval = TRUE------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds")) %>%
  mutate(global_optimum = ifelse(value < -14157, 1, 0)) %>%
  group_by(label) %>%
  summarise(proportion_global_optimum = mean(global_optimum, na.rm = TRUE))


## ---- eval = TRUE------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds")) %>%
  filter(value < -14157) %>%
  group_by(label) %>%
  summarise(mean_time = mean(seconds))


## ---- eval = TRUE------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds")) %>%
  filter(value < -14157) %>%
  ggplot(aes(x = "", y = seconds)) +
    scale_y_continuous() +
    geom_boxplot() +
    facet_wrap("label", labeller = "label_both", nrow = 1) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("seconds")

