options("ino_verbose" = FALSE)

test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(
    Nop$new(),
    "Please specify argument `f`."
  )
  expect_error(
    Nop$new(f = 1),
    "Argument `f` is not a function."
  )
  expect_error(
    Nop$new(f = f_ackley),
    "Please specify argument `npar`."
  )
  expect_error(
    Nop$new(f = f_ackley, npar = 0),
    "Argument `npar` is not a positive <integer>."
  )
  expect_identical(ackley$f, f_ackley)
  expect_identical(ackley$npar, 2L)
  expect_error(
    {
      ackley$f <- function(x) x
    },
    "is read only"
  )
  expect_error(
    {
      ackley$npar <- 1
    },
    "is read only"
  )
  expect_error(
    Nop$new(f = function() 1, npar = 0),
    "should have at least one argument."
  )
})

test_that("Nop object can be printed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("Parameters for Nop object can be set, get, and removed", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = earthquakes)
  expect_s3_class(hmm, c("Nop", "R6"), exact = TRUE)
  expect_error(
    hmm$set_argument(earthquakes),
    "Please name argument 1."
  )
  expect_error(
    hmm$set_argument("data" = earthquakes),
    "already exists"
  )
  expect_snapshot(print(hmm))
  hmm$set_argument("test_arg1" = 1, "test_arg2" = 2)
  expect_snapshot(print(hmm))
  expect_error(
    hmm$remove_argument("test_arg3"),
    "is not yet specified"
  )
  expect_s3_class(hmm$remove_argument("test_arg2"), "Nop")
  expect_error(
    hmm$set_argument(),
    "Please specify an argument."
  )
  expect_error(
    hmm$get_argument(),
    "Please specify `argument_name`."
  )
  expect_equal(hmm$get_argument("test_arg1"), 1)
  expect_error(
    hmm$get_argument("does_not_exist"),
    "is not yet specified"
  )
  expect_error(
    hmm$get_argument(1),
    "must be a single"
  )
  expect_error(
    hmm$remove_argument(),
    "Please specify `argument_name`."
  )
  expect_error(
    hmm$remove_argument(argument_name = 1:2),
    "must be a"
  )
  hmm$remove_argument("data")
  expect_equal(hmm$arguments, list("test_arg1" = 1))
  expect_error(
    {
      hmm$arguments <- list()
    },
    "is read only."
  )
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$set_optimizer(),
    "Please specify argument `optimizer`."
  )
  expect_error(
    ackley$set_optimizer(
      "not_an_optimizer_object"
    ),
    "must be an"
  )
  expect_error(
    ackley$set_optimizer(optimizer_nlm(), label = 1),
    "must be a"
  )
  ackley$set_optimizer(optimizer_nlm(), label = "nlm")
  expect_snapshot(ackley)
  expect_error(
    ackley$set_optimizer(optimizer_nlm(), label = "nlm"),
    "already exists, choose another one"
  )
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
  expect_warning(
    ackley$remove_optimizer("does_not_exist"),
    "No optimizer selected."
  )
})

test_that("ackley function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$evaluate(1),
    "It must be of length 2."
  )
  expect_type(ackley$evaluate(c(1, 2)), "double")
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
})

test_that("long function evaluation can be interrupted", {
  f <- function(x) {
    Sys.sleep(1.5)
    x
  }
  long_f <- Nop$new(f = f, npar = 1)
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 1),
    "time limit reached"
  )
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 2),
    1
  )
})

test_that("warnings in function evaluations can be hidden", {
  f <- function(x) {
    warning("this is a warning")
    x
  }
  warning_f <- Nop$new(f = f, npar = 1)
  expect_warning(
    warning_f$evaluate(at = 1),
    "this is a warning"
  )
  expect_warning(
    warning_f$evaluate(at = 1, hide_warnings = TRUE),
    regexp = NA
  )
})

test_that("errors in function evaluations can be returned", {
  f <- function(x) {
    stop("this is an error")
    x
  }
  error_f <- Nop$new(f = f, npar = 1)
  expect_equal(
    error_f$evaluate(at = 1),
    "this is an error"
  )
})

test_that("HMM likelihood function can be evaluated", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, "data" = earthquakes$obs)
  hmm$set_argument("N" = 2, "neg" = TRUE)
  expect_type(hmm$evaluate(at = c(0, 2, 1, 4, 2, 3)), "double")
  hmm$remove_argument("neg")
  expect_type(hmm$evaluate(at = c(0, 2, 1, 4, 2, 3)), "double")
})

test_that("function can be optimized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(runs = -1),
    "must be a positive"
  )
  expect_error(
    ackley$optimize(runs = "1"),
    "must be a positive"
  )
  expect_error(
    ackley$optimize(verbose = "yes"),
    "must be"
  )
  ackley$optimize(runs = 5)
  ackley$optimize(runs = 1, initial = runif(2))
  ackley$optimize(runs = 3, initial = function() runif(2), seed = 1)
  ackley$optimize(initial = c(0, 0))
  expect_snapshot(ackley)
  expect_error(
    ackley$optimize(save_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = TRUE, simplify = "TRUE"),
    "must be"
  )
  out <- ackley$optimize(runs = 5, return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  expect_length(out, 5)
  expect_length(out[[1]], 2)
  ackley$remove_optimizer(2)
  out <- ackley$optimize(
    runs = 1, return_results = TRUE, save_results = FALSE
  )
  expect_type(out, "list")
  out <- ackley$optimize(
    runs = 1, return_results = TRUE, save_results = FALSE, simplify = FALSE
  )
  expect_type(out, "list")
  ackley$optimize()
  expect_error(
    ackley$optimize(hide_warnings = "bad"),
    "must be"
  )
})

test_that("parallel optimization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(ncores = "1"),
    "must be a positive."
  )
  skip_on_cran()
  ackley$optimize(
    runs = 100, ncores = 2, save_results = FALSE
  )
})

test_that("Nop object can be tested", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_warning(
    ackley$test(),
    "No optimizer specified, testing optimizer is skipped."
  )
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(
    ackley$test(time_limit = -1),
    "is not a positive"
  )
  expect_error(
    ackley$test(verbose = "FALSE"),
    "must be"
  )
  expect_true(ackley$test())
  bad_f <- Nop$new(f = function(x) stop("error message"), 1)
  expect_error(
    bad_f$test(),
    "Function call threw an error"
  )
  lengthy_f <- Nop$new(f = function(x) 1:2, 1)
  expect_error(
    lengthy_f$test(),
    "Test function call is of length 2."
  )
  character_f <- Nop$new(f = function(x) "not_a_numeric", 1)
  expect_error(
    character_f$test(),
    "Function call threw an error"
  )
  expect_warning(
    {
      slow_f <- Nop$new(f = function(x) {
        Sys.sleep(2)
        1
      }, 1)
    },
    "Function `f` is unnamed."
  )
  expect_warning(
    expect_warning(
      slow_f$test(time_limit = 1),
      "Time limit of 1s was reached"
    ),
    "No optimizer specified, testing optimizer is skipped."
  )
  ackley$remove_optimizer(1:2)
  bad_optimizer_fun <- function(f, p) {
    if (identical(p, 1:2)) stop("error message")
    list(v = f(p), z = 1:2)
  }
  bad_optimizer <- optimizeR::define_optimizer(
    bad_optimizer_fun,
    objective = "f", initial = "p", value = "v",
    parameter = "z"
  )
  ackley$set_optimizer(bad_optimizer)
  expect_error(
    ackley$test(at = 1:2),
    "Optimization threw an error"
  )
})

test_that("standardization works", {
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = earthquakes$obs, "N" = 2, "neg" = TRUE
  )
  expect_error(
    hmm$standardize(),
    "Please specify"
  )
  expect_error(
    hmm$standardize(1),
    "must be a single"
  )
  expect_s3_class(hmm$standardize("data"), c("Nop", "R6"), exact = TRUE)
})

test_that("reduction works", {
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = earthquakes$obs, "N" = 2, "neg" = TRUE
  )
  expect_error(
    hmm$reduce(),
    "Please specify"
  )
  expect_error(
    hmm$reduce(1),
    "must be a single"
  )
  expect_s3_class(hmm$reduce("data"), c("Nop", "R6"), exact = TRUE)
})

test_that("argument can be reset", {
  data <- earthquakes
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = data, "N" = 2, "neg" = TRUE
  )
  hmm$standardize("data")
  hmm$reset_argument("data")
  expect_equal(data, hmm$get_argument("data"))
  hmm$reduce("data")
  hmm$reset_argument("data")
  expect_equal(data, hmm$get_argument("data"))
})

test_that("continue optimization works", {
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = earthquakes$obs, "N" = 2, "neg" = TRUE
  )$set_optimizer(optimizer_nlm())$
    standardize("data")$
    optimize(runs = 2)$
    optimize(runs = 2)$
    reset_argument("data")$
    continue()
  expect_s3_class(hmm, "Nop")
})

test_that("summary works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$summary(),
    "No optimization results saved yet."
  )
})

test_that("overview of optima works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$optima(),
    "No optimization results saved yet."
  )
})

test_that("plotting works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  pdf(file = tempfile())
  expect_s3_class(ackley$plot(), "ggplot")
  dev.off()
})

test_that("best value and parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  expect_length(
    ackley$best_parameter(), 2
  )
  expect_length(
    ackley$best_value(), 1
  )
})
