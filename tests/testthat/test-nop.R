# self <- object
# private <- self$.__enclos_env__$private

# Example 0: Polynomial ---------------------------------------------------

test_that("Example 0: Defining the problem works", {
  f <- function(x) -x^4 - x^3 - x^2 - x
  gradient <- function(x) -4 * x^3 - 3 * x^2 - 2 * x - 1
  hessian <- function(x) -12 * x^2 - 6 * x - 2
  Nop_pol <- Nop$new(
    f = f, target = "x", npar = 1, gradient = gradient, hessian = hessian
  )
  checkmate::expect_r6(Nop_pol, "Nop")
  expect_identical(Nop_pol$npar, c("x" = 1))
  expect_error(Nop_pol$npar <- 2, "read-only")
  expect_equal(
    Nop_pol$evaluate(0, TRUE, TRUE),
    structure(0, gradient = -1, hessian = -2)
  )
  expect_snapshot(Nop_pol)
  checkmate::expect_tibble(Nop_pol$results, nrows = 0, ncols = 4)
  expect_error(ggplot2::autoplot(Nop_pol), "Input `xlim` is bad")
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_pol, xlim = c(-1, 1))))
  Nop_pol$initialize_fixed(0.1)
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_pol)))
})

# Example 1: Ackley function ----------------------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(f = ackley, npar = 2)
Nop_ackley$verbose <- FALSE
nlm_opt <- optimizeR::Optimizer$new(which = "stats::nlm")
optim_opt <- optimizeR::Optimizer$new(which = "stats::optim")

test_that("Example 1: Defining optimizers works", {
  expect_error(Nop_ackley$optimize(), "No optimizer specified yet.")
  Nop_ackley$
    set_optimizer(nlm_opt, optimizer_label = "nlm")$
    set_optimizer(optim_opt)
})

test_that("Example 1: Minimization works", {
  Nop_ackley$initialize_reset()
  expect_error(
    Nop_ackley$fresh_label <- "label",
    "read-only"
  )
  expect_warning(
    Nop_ackley$optimize(),
    "No initial values defined by user"
  )
  Nop_ackley$
    initialize_random(runs = 5)$optimize()$
    initialize_random(sampler = function() runif(2))$optimize()$
    initialize_fixed(0:1)$optimize()$
    initialize_fixed(list(1:2, 2:3, 3:4))$optimize()$
    initialize_grid()
  expect_warning(
    Nop_ackley$optimize(which_optimizer = 3)
  )
  expect_snapshot(Nop_ackley)
})

test_that("Example 1: Results can be accessed", {
  checkmate::expect_tibble(Nop_ackley$results)
  checkmate::expect_list(Nop_ackley$minimum, len = 2)
  checkmate::expect_tibble(Nop_ackley$optima())
  comb <- expand.grid(
    which_direction = c("min", "max"),
    only_original = c(TRUE, FALSE),
    group_by = c(NULL, "optimization", "optimizer"),
    sort_by_value = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(comb))) {
    optima <- Nop_ackley$optima(comb[i, 1], comb[i, 2], comb[i, 3], comb[i, 4])
    if (is.null(comb[1, 3])) {
      checkmate::expect_tibble(optima)
    } else {
      checkmate::expect_list(optima)
    }
  }
})

test_that("Example 1: Maximization works", {
  Nop_ackley$
    initialize_random(sampler = function() stats::runif(sum(Nop_ackley$npar)))$
    optimize(
    which_direction = "max", which_optimizer = "stats::optim",
    lower = 0.5, upper = 1
  )
  checkmate::expect_list(Nop_ackley$maximum, len = 2)
})

test_that("Example 1: Plotting works", {
  expect_error(ggplot2::autoplot(Nop_ackley), "Input `xlim` is bad")
  expect_error(
    ggplot2::autoplot(Nop_ackley, xlim = c(-1, 1)), "Input `xlim2` is bad"
  )
  expect_true(ggplot2::is_ggplot(
    ggplot2::autoplot(Nop_ackley, xlim = c(-1, 1), xlim2 = c(-1, 1))
  ))
  Nop_ackley$initialize_random(runs = 10)
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_ackley)))
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_ackley$optima())))
  autoplot_optima_group <- ggplot2::autoplot(Nop_ackley$optima(group_by = "optimizer"))
  expect_true(is.list(autoplot_optima_group))
  expect_length(autoplot_optima_group, 2)
  expect_true(ggplot2::is_ggplot(autoplot_optima_group[[1]]))
  expect_true(ggplot2::is_ggplot(autoplot_optima_group[[2]]))
})

test_that("Example 1: Deviation can be computed and visualized", {
  expect_s3_class(Nop_ackley$deviation(), "Nop_deviation")
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_ackley$deviation())))
})

test_that("Example 1: Initials can be filtered and promising values selected", {
  Nop_ackley$initialize_reset()
  expect_length(Nop_ackley$initial_values, 0)
  Nop_ackley$initialize_random(100)
  Nop_ackley$initialize_promising(0.9, "value_small")
  Nop_ackley$initialize_promising(0.9, "value_large")
  Nop_ackley$initialize_promising(0.9, "gradient_small")
  Nop_ackley$initialize_promising(0.9, "gradient_large")
  Nop_ackley$initialize_promising(0.9, "condition_small")
  Nop_ackley$initialize_promising(0.9, "condition_large")
  Nop_ackley$initialize_reset()
  Nop_ackley$initialize_fixed(
    list(
      1:2, # positive gradient
      -(1:2) # negative gradient
    )
  )
  Nop_ackley$initialize_filter("gradient_negative")
  expect_length(Nop_ackley$initial_values, 1)
  Nop_ackley$initialize_filter("gradient_positive")
  expect_length(Nop_ackley$initial_values, 0)
  Nop_ackley$initialize_fixed(
    list(
      1:2, # positive eigenvalues
      c(-0.4, -0.3) # negative eigenvalues
    )
  )
  Nop_ackley$initialize_filter("hessian_negative")
  expect_length(Nop_ackley$initial_values, 1)
  Nop_ackley$initialize_filter("hessian_positive")
  expect_length(Nop_ackley$initial_values, 0)
})

# Example 2: Mixture model ------------------------------------------------

normal_mixture_llk <- function(mu, sigma, lambda, data) {
  sigma <- exp(sigma)
  lambda <- plogis(lambda)
  sum(log(lambda * dnorm(data, mu[1], sigma[1]) +
    (1 - lambda) * dnorm(data, mu[2], sigma[2])))
}

Nop_mixture <- Nop$new(
  f = normal_mixture_llk, target = c("mu", "sigma", "lambda"), npar = c(2, 2, 1)
)

test_that("Example 2: Evaluate with fixed arguments missing", {
  expect_error(
    Nop_mixture$evaluate(),
    "Function argument `data` is required but not specified yet."
  )
})

data <- faithful$eruptions

test_that("Example 2: Fixed arguments can be defined", {
  expect_error(
    Nop_mixture$fixed_argument("get", "data"),
    "not available"
  )
  Nop_mixture$fixed_argument("set", data = data)
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    data
  )
  Nop_mixture$fixed_argument("remove", "data")
  expect_error(
    Nop_mixture$fixed_argument("get", "data"),
    "not available"
  )
})

test_that("Example 2: Fixed arguments can be modified", {
  Nop_mixture$fixed_argument("set", data = data)
  Nop_mixture$fixed_argument("modify", data = 1)
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    1
  )
  Nop_mixture$fixed_argument("reset", "data")
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    data
  )
})

test_that("Example 2: Fixed argument can be standardized", {
  Nop_mixture$standardize_argument("data")
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    normalize::normalize(data)
  )
  Nop_mixture$fixed_argument("reset", "data")
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    data
  )
})

test_that("Example 2: Fixed argument can be subsetted", {
  Nop_mixture$reduce_argument("data", proportion = 0.5, how = "first")
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    portion::portion(data, proportion = 0.5, how = "first")
  )
  Nop_mixture$fixed_argument("reset", "data")
  expect_identical(
    Nop_mixture$fixed_argument("get", "data"),
    data
  )
})

test_that("Example 2: Initialization can be continued", {
  Nop_mixture$
    set_optimizer(optimizeR::Optimizer$new(which = "stats::nlm"))$
    initialize_fixed(c(2, 4, 1, 2, -1))$
    optimize(optimization_label = "continue")
  Nop_mixture$initialize_continue("continue")
  expect_identical(
    Nop_mixture$results |> dplyr::pull(parameter),
    Nop_mixture$initial_values
  )
})

test_that("Example 2: Plotting results works", {
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(Nop_mixture$results)))
})
