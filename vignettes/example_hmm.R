## ---- include = FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "75%"
)
# library("ino")
devtools::load_all() # remove later
set.seed(1)


## ---- message = FALSE, warning = FALSE----------------------------------------------------------------------
library("dplyr")
dax <- fHMM::download_data(symbol = "^GDAXI", from = "2000-01-01", to = "2020")
db_data <- data %>%
  as_tibble() %>%
  reframe(
    date = as.Date(Date, format = "%Y-%m-%d"),
    logreturn = c(NA, diff(log(Close), lag = 1))
  ) %>%
  filter(!is.na(logreturn)) %>%
  print()


## ---- message = FALSE, warning = FALSE----------------------------------------------------------------------
library("ggplot2")
ggplot(db_data, aes(x = date, y = logreturn)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent())


## ---- eval = FALSE------------------------------------------------------------------------------------------
## hmm_ino <- Nop$new(
##     f = f_ll_hmm,
##     npar = 6,
##     data = db_data$logreturn,
##     N = 2,
##     neg = TRUE
##   )$
##   set_optimizer(optimizer_nlm())


## ---- eval = FALSE------------------------------------------------------------------------------------------
## sampler <- function() {
##   c(stats::runif(2, -2, -1), stats::rnorm(2), log(stats::runif(2, 0.5, 2)))
## }
## hmm_ino$optimize(initial = sampler, runs = 50, label = "random")


## ---- eval = FALSE------------------------------------------------------------------------------------------
## tpm_entry_1 <- tpm_entry_2 <- c(-2, -2.5)
## mu_1 <- c(0, -0.05)
## mu_2 <- c(0, 0.05)
## sd_1 <- c(log(0.1), log(0.5))
## sd_2 <- c(log(0.75), log(1))
##
## starting_values <- asplit(expand.grid(
##   tpm_entry_1, tpm_entry_2, mu_1, mu_2, sd_1, sd_2), MARGIN = 1)
##
## hmm_ino$optimize(initial = starting_values, label = "educated_guess")


## ---- eval = FALSE------------------------------------------------------------------------------------------
## hmm_ino$
##   reduce("data", how = "first", prop = 0.2)$
##   optimize(initial = sampler, runs = 50, label = "subset")$
##   continue()$
##   reset_argument("data")


## ----eval=TRUE----------------------------------------------------------------------------------------------
hmm_ino$optima()


## ----eval=TRUE----------------------------------------------------------------------------------------------
hmm_ino$summary(c("value", "seconds", "label")) %>%
  head(n = 10)


## ----eval=TRUE----------------------------------------------------------------------------------------------
hmm_ino$best_parameter


## ----eval=TRUE----------------------------------------------------------------------------------------------
hmm_ino$best_value


## ----eval=TRUE----------------------------------------------------------------------------------------------
plot(hmm_ino, by = "label")


## ----warning = FALSE, message = FALSE, eval=TRUE------------------------------------------------------------
summary(hmm_ino, c("label", "seconds")) %>%
  group_by(label) %>%
  summarise(avg_time = mean(seconds, na.rm = TRUE))


## ----eval=TRUE----------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds")) %>%
  mutate(global_optimum = ifelse(value < -14157, 1, 0)) %>%
  group_by(label) %>%
  summarise(proportion_global_optimum = mean(global_optimum, na.rm = TRUE))


## ----eval=TRUE----------------------------------------------------------------------------------------------
summary(hmm_ino, c("label", "value", "seconds")) %>%
  filter(value < -14157) %>%
  group_by(label) %>%
  summarise(mean_time = mean(seconds))


## ----eval=TRUE----------------------------------------------------------------------------------------------
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

