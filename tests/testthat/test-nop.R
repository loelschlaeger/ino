options("ino_verbose" = FALSE)

test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(
    Nop$new(),
    "specify argument"
  )
  expect_error(
    Nop$new(f = 1),
    "is not a"
  )
  expect_error(
    Nop$new(f = f_ackley),
    "specify argument"
  )
  expect_error(
    Nop$new(f = f_ackley, npar = 0),
    "must be a single, positive"
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
    "should have at least one argument"
  )
})

test_that("Nop object can be printed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("Parameters for Nop object can be set", {
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
})

test_that("Parameters for Nop object can be get", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = earthquakes, test_arg = 6)
  expect_error(
    hmm$get_argument(),
    "Please specify"
  )
  expect_equal(hmm$get_argument("test_arg"), 6)
  expect_error(
    hmm$get_argument("does_not_exist"),
    "is not yet specified"
  )
  expect_error(
    hmm$get_argument(1),
    "must be a single"
  )
})

test_that("Parameters for Nop object can be removed", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = earthquakes)
  expect_error(
    hmm$remove_argument("arg_does_not_exist"),
    "is not yet specified"
  )
  expect_s3_class(hmm$remove_argument("data"), "Nop")
  expect_error(
    hmm$remove_argument(),
    "Please specify"
  )
  expect_error(
    hmm$remove_argument(argument_name = 1:2),
    "must be a"
  )
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$set_optimizer(),
    "Please specify argument"
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
    "already exists, please choose another one"
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
    "must be of length 2"
  )
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
})

test_that("long function evaluations can be interrupted", {
  expect_warning(
    long_f <- Nop$new(f = function(x) { Sys.sleep(0.5); x}, npar = 1),
    "is unnamed"
  )
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 0.2),
    "time limit reached"
  )
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 0.8),
    1
  )
})

test_that("warnings in function evaluation can be hidden", {
  expect_warning(
    warning_f <- Nop$new(f = function(x) { warning("huhu"); x}, npar = 1),
    "is unnamed"
  )
  expect_warning(
    warning_f$evaluate(at = 1),
    "huhu"
  )
  expect_warning(
    warning_f$evaluate(at = 1, hide_warnings = TRUE),
    regexp = NA
  )
})

test_that("errors in function evaluation can be returned", {
  expect_warning(
    error_f <- Nop$new(f = function(x) { stop("shit"); x}, npar = 1),
    "is unnamed"
  )
  expect_equal(
    error_f$evaluate(at = 1),
    "shit"
  )
})

test_that("HMM likelihood function can be evaluated", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, "data" = earthquakes$obs)
  at <- rnorm(6)
  expect_error(
    hmm$evaluate(),
    "is not yet specified"
  )
  hmm$set_argument("N" = 2, "neg" = TRUE)
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = earthquakes$obs, N = 2, neg = TRUE)
  )
  hmm$remove_argument("neg")
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = earthquakes$obs, N = 2)
  )
})

test_that("ackley function can be optimized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(runs = -1),
    "must be a single, positive"
  )
  expect_error(
    ackley$optimize(verbose = "yes"),
    "must be"
  )
  expect_error(
    ackley$optimize(hide_warnings = "bad"),
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
  expect_true(all(sapply(out, length) == 2))
  ackley$remove_optimizer(2)
  out <- ackley$optimize(
    runs = 1, return_results = TRUE, save_results = FALSE
  )
  expect_type(out, "list")
  out <- ackley$optimize(
    runs = 1, return_results = TRUE, save_results = FALSE, simplify = FALSE
  )
  expect_type(out, "list")
})

test_that("parallel optimization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(ncores = 1.4),
    "must be a single, positive"
  )
  skip_on_cran()
  ackley$optimize(
    runs = 40, ncores = 2, save_results = FALSE
  )
})

test_that("Nop object can be tested", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$test(at = 1),
    "must be of length 2"
  )
  expect_warning(
    ackley$test(),
    "No optimizer specified, testing optimizer is skipped."
  )
  ackley$set_optimizer(optimizer_nlm())
  expect_true(ackley$test(verbose = FALSE))
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

test_that("results can be accessed", {
  runs <- 10
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = runs, save_results = TRUE, return_results = FALSE)
  results <- ackley$results()
  expect_type(results, "list")
  expect_length(results, runs)
})

test_that("number of results can be accessed", {
  runs <- 10
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = runs, save_results = TRUE, return_results = FALSE)
  expect_equal(ackley$number_runs(), runs)
})

test_that("overview of available elements can be created", {
  # TODO
})

test_that("results can be cleared", {
  # TODO
})

test_that("results can be summarized", {
  # TODO
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

test_that("optimization times can be plotted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  pdf(file = tempfile())
  expect_s3_class(ackley$plot(), "ggplot")
  dev.off()
})

test_that("best value can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  expect_length(
    ackley$best_value(), 1
  )
})

test_that("best parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  expect_length(
    ackley$best_parameter(), 2
  )
})

test_that("best parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  expect_length(
    ackley$closest_parameter(0), 2
  )
})

test_that("existence of additional argument can be checked", {
  # TODO
})

test_that("completeness of additional arguments can be checked", {
 # TODO
})

test_that("target argument can be checked", {
  # TODO
})

test_that("selection of element can be checked", {
  # TODO
})

test_that("cheap function evaluation works", {
  # TODO
})

test_that("check function optimization works", {
  # TODO
})

test_that("optimization run can be labeled", {
  # TODO
})

test_that("optimization run can be saved", {
  # TODO
})

test_that("results of continued and previous runs can be merged", {
  # TODO
})

test_that("existence of additional argument can be checked", {
  # TODO
})

test_that("original argument can be saved", {
  # TODO
})

test_that("original argument can be reset", {
  # TODO
})

test_that("run ids can be get", {
  # TODO
})

test_that("optimizer ids can be get", {
  # TODO
})

test_that("f can be extracted", {
  # TODO
})

test_that("f_name can be extracted", {
  # TODO
})

test_that("f_target can be extracted", {
  # TODO
})

test_that("npar can be extracted", {
  # TODO
})

test_that("arguments can be extracted", {
  # TODO
})

test_that("true value can be extracted", {
  # TODO
})

test_that("true parameter can be extracted", {
  # TODO
})

test_that("show minimum can be extracted", {
  # TODO
})

test_that("optimizer can be extracted", {
  # TODO
})

test_that("new label can be generated", {
  # TODO
})
