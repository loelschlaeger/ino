options("ino_verbose" = FALSE)

test_that("ackley Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(Nop$new(), "Please specify argument `f`.")
  expect_error(Nop$new(f = 1), "Argument `f` is not a function.")
  expect_error(Nop$new(f = f_ackley), "Please specify argument `npar`.")
  expect_error(Nop$new(f = f_ackley, npar = 0), "Argument `npar` is not a positive integer.")
  expect_identical(ackley$f, f_ackley)
  expect_identical(ackley$npar, 2L)
  expect_error(
    {
      ackley$f <- function(x) x
    },
    "is read only."
  )
  expect_error(
    {
      ackley$npar <- 1
    },
    "is read only."
  )
  expect_error(Nop$new(f = function() 1, npar = 0), "should have at least one argument.")
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("hmm Nop object with parameters can be initialized", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = earthquakes)
  expect_s3_class(hmm, c("Nop", "R6"), exact = TRUE)
  expect_error(hmm$set_argument(earthquakes), "Please name argument 1.")
  expect_error(hmm$set_argument("data" = earthquakes), "already exists")
  expect_snapshot(print(hmm))
  hmm$set_argument("test_arg1" = 1, "test_arg2" = 2)
  expect_snapshot(print(hmm))
  expect_error(hmm$remove_argument("test_arg3"), "does not exist.")
  expect_s3_class(hmm$remove_argument("test_arg2"), "Nop")
  expect_error(hmm$set_argument(), "Please specify an argument.")
  expect_error(hmm$get_argument(), "Please specify `argument_name`.")
  expect_equal(hmm$get_argument("test_arg1"), 1)
  expect_error(hmm$get_argument("does_not_exist"), "does not exist")
  expect_error(hmm$get_argument(1), "must be a single character")
  expect_error(hmm$remove_argument(), "Please specify `argument_name`.")
  expect_error(hmm$remove_argument(argument_name = 1:2), "must be a `character`")
  hmm$remove_argument("data")
  expect_equal(hmm$arguments, list("test_arg1" = 1))
  expect_error(
    {
      hmm$arguments <- list()
    },
    "is read only."
  )
})

test_that("ackley function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$evaluate(1), "Argument `at` must be a numeric vector of length 2.")
  expect_type(ackley$evaluate(c(1, 2)), "double")
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, "data" = earthquakes)
  hmm$set_argument("N" = 2, "neg" = TRUE)
  expect_type(hmm$evaluate(at = c(0, 2, 1, 4, 2, 3)), "double")
  hmm$remove_argument("neg")
  expect_type(hmm$evaluate(at = c(0, 2, 1, 4, 2, 3)), "double")
  hmm$remove_argument("N")
  expect_error(hmm$evaluate(at = c(0, 2, 1, 4, 2, 3)), "does not exist")
})

test_that("true value can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_value)
  ackley$true_value <- 2
  expect_equal(ackley$true_value, 2)
  ackley$set_true_value(3)
  expect_equal(ackley$true_value, 3)
  expect_snapshot(ackley)
  expect_error(
    {
      ackley$true_value <- "1"
    },
    "must be a `numeric`."
  )
})

test_that("true parameter can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_parameter)
  expect_error(
    {
      ackley$true_parameter <- 2
    },
    "must be a numeric vector of length 2."
  )
  ackley$true_parameter <- c(0, 0)
  expect_equal(ackley$true_parameter, c(0, 0))
  expect_error(ackley$set_true_parameter(1:2, "not_a_logical"), "must be `TRUE` or `FALSE`.")
  expect_error(ackley$set_true_parameter(3))
  ackley$set_true_parameter(c(1, 0))
  expect_equal(ackley$true_parameter, c(1, 0))
  expect_null(ackley$true_value)
  ackley$set_true_parameter(c(0, 0), set_true_value = TRUE)
  expect_equal(ackley$true_value, f_ackley(c(0, 0)))
  expect_snapshot(ackley)
  expect_error(ackley$set_true_parameter(c(1, 1), set_true_value = FALSE), "also update the true optimum value.")
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$set_optimizer(), "Please specify argument `optimizer`.")
  expect_error(ackley$set_optimizer("not_an_optimizer_object"), "must be an object of class `optimizer`.")
  expect_error(ackley$set_optimizer(optimizer_nlm(), label = 1), "must be a `character` of length 1.")
  ackley$set_optimizer(optimizer_nlm(), label = "nlm")
  expect_snapshot(ackley)
  expect_error(ackley$set_optimizer(optimizer_nlm(), label = "nlm"))
  ackley$set_optimizer(optimizer_optim())
  expect_snapshot(ackley)
})

test_that("optimizer can be removed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm(), label = "A")
  ackley$set_optimizer(optimizer_nlm(), label = "B")
  ackley$set_optimizer(optimizer_nlm(), label = "C")
  ackley$set_optimizer(optimizer_nlm())
  expect_snapshot(ackley)
  ackley2 <- ackley$clone()
  ackley2$remove_optimizer("all")
  expect_snapshot(ackley2)
  ackley$remove_optimizer(2)
  expect_snapshot(ackley)
  ackley$remove_optimizer(c("stats::nlm", "A"))
  expect_snapshot(ackley)
  expect_error(ackley$remove_optimizer("does_not_exist"))
})

test_that("function can be optimized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(ackley$optimize(runs = -1), "must be a positive integer.")
  expect_error(ackley$optimize(runs = "1"), "must be a positive integer.")
  expect_error(ackley$optimize(verbose = "yes"), "`verbose` must be either `TRUE` or `FALSE`.")
  ackley$optimize(runs = 5)
  ackley$optimize(runs = 1, initial = runif(2))
  ackley$optimize(runs = 3, initial = function() runif(2), seed = 1)
  ackley$optimize(initial = c(0, 0))
  expect_error(ackley$optimize(initial = c(1:3)), "misspecified.")
  expect_snapshot(ackley)
  expect_error(ackley$optimize(save_results = "TRUE"), "`save_results` must be either `TRUE` or `FALSE`.")
  expect_error(ackley$optimize(return_results = "TRUE"), "`return_results` must be either `TRUE` or `FALSE`.")
  expect_error(ackley$optimize(return_results = TRUE, simplify = "TRUE"), "`simplify` must be either `TRUE` or `FALSE`.")
  out <- ackley$optimize(runs = 5, return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  expect_length(out, 5)
  expect_length(out[[1]], 2)
  ackley$remove_optimizer(2)
  out <- ackley$optimize(runs = 1, return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  out <- ackley$optimize(runs = 1, return_results = TRUE, save_results = FALSE, simplify = FALSE)
  expect_type(out, "list")
  expect_error(ackley$optimize(initial = function() "not_a_numeric"), "should return a `numeric`")
  expect_error(ackley$optimize(initial = "initial_misspecified"), "`initial` is misspecified")
  expect_error(ackley$optimize(reset_arguments_afterwards = "FALSE"), "must be either")
  ackley$optimize()
  expect_length(ackley$best_parameter, 2)
  expect_error(
    {
      ackley$best_parameter <- "bad"
    },
    "read only"
  )
  expect_length(ackley$best_value, 1)
  expect_error(
    {
      ackley$best_value <- "bad"
    },
    "read only"
  )
  expect_true(ackley$show_minimum)
  ackley$show_minimum <- FALSE
  expect_error(
    {
      ackley$show_minimum <- "bad"
    },
    "must be"
  )
  expect_false(ackley$show_minimum)
  expect_length(ackley$best_parameter, 2)
  expect_error(
    {
      ackley$best_parameter <- "bad"
    },
    "read only"
  )
  expect_length(ackley$best_value, 1)
  expect_error(
    {
      ackley$best_value <- "bad"
    },
    "read only"
  )
  expect_error(ackley$optimize(hide_warnings = "bad"), "must be")
})

test_that("parallel optimization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(ackley$optimize(ncores = "1"), "`ncores` must be a positive `integer`.")
  skip_on_cran()
  ackley$optimize(runs = 1000, ncores = 2, save_results = FALSE, reset_arguments_afterwards = TRUE)
})

test_that("Nop object can be tested", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_warning(ackley$test(), "No optimizer specified, testing optimizer is skipped.")
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(ackley$test(time_limit_fun = -1), "`time_limit_fun` is not a positive `integer`.")
  expect_error(ackley$test(time_limit_opt = -1), "`time_limit_opt` is not a positive `integer`.")
  expect_error(ackley$test(verbose = "FALSE"), "`verbose` must be either `TRUE` or `FALSE`.")
  expect_true(ackley$test())
  bad_f <- Nop$new(f = function(x) stop(), 1)
  expect_error(bad_f$test(), "Function call failed.")
  lengthy_f <- Nop$new(f = function(x) 1:2, 1)
  expect_error(lengthy_f$test(), "Test function call returned a `numeric` of length 2.")
  character_f <- Nop$new(f = function(x) "not_a_numeric", 1)
  expect_error(character_f$test(), "function call returned an object of class `character`.")
  slow_f <- Nop$new(f = function(x) {
    Sys.sleep(2)
    1
  }, 1)
  expect_warning(
    expect_warning(slow_f$test(time_limit_fun = 1), "The time limit of 1s was reached"),
    "No optimizer specified, testing optimizer is skipped."
  )
  slow_f$set_optimizer(optimizer_nlm())
  expect_warning(slow_f$test(time_limit_fun = 3, time_limit_opt = 1), "The time limit of 1s was reached")
  ackley$remove_optimizer(1:2)
  bad_optimizer_fun <- function(f, p) {
    if (identical(p, 1:2)) stop()
    list(v = 1, z = 1:2)
  }
  bad_optimizer <- optimizeR::define_optimizer(bad_optimizer_fun, objective = "f", initial = "p", value = "v", parameter = "z")
  ackley$set_optimizer(bad_optimizer)
  expect_error(ackley$test(at = 1:2), "Optimization with optimizer `bad_optimizer_fun` failed.")
})

test_that("standardization works", {
  N <- 10
  T <- 1
  J <- 3
  P <- 3
  b <- c(1, -1, 0.5)
  Sigma <- diag(J)
  X <- function() {
    class <- sample(0:1, 1)
    mean <- ifelse(class, 2, -2)
    matrix(stats::rnorm(J * P, mean = mean), nrow = J, ncol = P)
  }
  probit_data <- sim_mnp(N = N, T = T, J = J, P = P, b = b, Sigma = Sigma, X = X)
  true <- attr(probit_data, "true")[-1]
  probit <- Nop$new(f = f_ll_mnp, npar = 5, data = probit_data, neg = TRUE)$
    set_true_parameter(true_par = true, set_true_value = TRUE)
  expect_error(probit$standardize(), "specify `argument_name`")
  expect_error(probit$standardize(1), "must be a `character`")
  probit$standardize("data", ignore = 1:3)
  expect_identical(dim(probit_data), dim(probit$arguments$data))
  probit$reset_argument("data")
  expect_identical(probit_data, probit$arguments$data)
  expect_error(probit$standardize("data", by_column = "TRUE"), "`by_column` must be `TRUE` or `FALSE`")
  expect_error(probit$standardize("data", ignore = "not_an_integer"), "Argument 'ignore' must be a vector of indices.")
  probit$standardize("data", by_column = FALSE)
  probit$reset_argument("data")
  expect_identical(probit_data, probit$arguments$data)
  probit$standardize("data", by_column = FALSE, ignore = 5:10)
})

test_that("reducing works", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  hmm$set_argument("data" = earthquakes, "N" = 2, "neg" = TRUE)
  expect_error(hmm$reset_argument(), "Please specify `argument_name`.")
  expect_error(hmm$reset_argument(1), "must be a `character`")
  expect_error(hmm$reduce(), "Please specify argument `argument_name`.")
  expect_error(hmm$reduce("data", how = "random", by_row = "TRUE"), "'by_row' must be `TRUE` or `FALSE`.")
  expect_error(hmm$reduce("data", how = "bad_argument"), "'how' must be one of")
  expect_error(hmm$reduce("data", proportion = 1), "'proportion' must be a numeric between 0 and 1.")
  expect_error(hmm$reduce("N"), "must be a `data.frame` or a `matrix`.")
  hmm$reduce("data", how = "random", proportion = 0.5)
  hmm$reset_argument("data")
  hmm$reduce("data", how = "first", proportion = 0.5)
  hmm$reset_argument("data")
  hmm$reduce("data", how = "first", by_row = FALSE, proportion = 0.5)
  hmm$reset_argument("data")
  hmm$reduce("data", how = "last", proportion = 0.9)
  hmm$reset_argument("data")
  expect_error(hmm$reduce("data", how = "similar", ignore = "not_an_integer"), "Argument 'ignore' must be a vector of indices.")
  hmm$reduce("data", how = "similar", ignore = 1, seed = 1)
  hmm$reset_argument("data")
  hmm$reduce("data", how = "similar", ignore = 1, seed = 1)
  hmm$reset_argument("data")
  hmm$reduce("data", how = "unsimilar", ignore = 2)
  hmm$reset_argument("data")
  hmm$set_optimizer(optimizer = optimizer_nlm(), label = "nlm")
  hmm$reduce("data", how = "first", proportion = 0.5)
})

test_that("continue optimization works", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  hmm$set_argument("data" = earthquakes, "N" = 2, "neg" = TRUE)
  hmm$set_optimizer(optimizer_nlm())
  hmm$set_optimizer(optimizer_optim())
  hmm$reduce("data", how = "random", proportion = 0.5, seed = 1)
  hmm$optimize(seed = 1, runs = 2)
  hmm$reset_argument("data")
  expect_true(is.list(hmm$continue(save_results = FALSE, return_results = TRUE)))
  hmm$continue()
  expect_s3_class(hmm, "Nop")
})

test_that("summary works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  ackley$set_true_value(0)
  expect_error(ackley$summary(), "No optimization results saved.")
  ackley$optimize(runs = 10)
  out <- ackley$summary()
  expect_named(ackley$summary(), c("value", "parameter", "seconds"))
  expect_true(is.data.frame(ackley$summary("distance" = "true_value - value")))
  expect_true(is.character(ackley$summary_columns))
  expect_error(
    {
      ackley$summary_columns <- "bad"
    },
    "read only"
  )
})

test_that("overview of optima works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$optimize(runs = 10)
  out <- ackley$optima()
  expect_named(out, c("value", "frequency"))
})

test_that("plotting works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$optimize(runs = 10)
  pdf(file = tempfile())
  expect_s3_class(ackley$plot(), "ggplot")
  dev.off()
})
