test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(Nop$new(), "Please specify argument `f`.")
  expect_error(Nop$new(f = 1), "Argument `f` is not a function.")
  expect_error(Nop$new(f = f_ackley), "Please specify argument `npar`.")
  expect_error(Nop$new(f = f_ackley, npar = 0), "Argument `npar` is not a positive integer.")
  expect_identical(ackley$f, f_ackley)
  expect_identical(ackley$npar, 2L)
  expect_error({ackley$f <- function(x) x}, "is read only.")
  expect_error({ackley$npar <- 1}, "is read only.")
  expect_error(Nop$new(f = function() 1, npar = 0), "should have at least one argument.")
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("Nop object with parameters can be initialized", {
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
  expect_error(hmm$remove_argument(argument_name = 1:2), "must be a character")
  hmm$remove_argument("data")
  expect_equal(hmm$arguments, list("test_arg1" = 1))
  expect_error({hmm$arguments <- list()}, "is read only.")
})

test_that("function can be evaluated", {
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
  expect_error({ackley$true_value <- "1"}, "must be a single numeric.")
})

test_that("true parameter can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_parameter)
  expect_error({ackley$true_parameter <- 2}, "must be a numeric vector of length 2.")
  ackley$true_parameter <- c(0,0)
  expect_equal(ackley$true_parameter, c(0,0))
  expect_error(ackley$set_true_parameter(1:2, "not_a_logical"), "must be `TRUE` or `FALSE`.")
  expect_error(ackley$set_true_parameter(3))
  ackley$set_true_parameter(c(1,0))
  expect_equal(ackley$true_parameter, c(1,0))
  expect_null(ackley$true_value)
  ackley$set_true_parameter(c(0,0), set_true_value = TRUE)
  expect_equal(ackley$true_value, f_ackley(c(0,0)))
  expect_snapshot(ackley)
  expect_error(ackley$set_true_parameter(c(1,1), set_true_value = FALSE), "also update the true optimum value.")
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$set_optimizer(), "Please specify argument `optimizer`.")
  expect_error(ackley$set_optimizer("not_an_optimizer_object"), "must be an object of class `optimizer`.")
  expect_error(ackley$set_optimizer(optimizer_nlm(), label = 1), "must be a single character.")
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
  ackley$optimize(initial = c(0,0))
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
  expect_error(ackley$optimize(initial = function() "not_a_numeric"), "should return a numeric vector")
  expect_error(ackley$optimize(initial = "initial_misspecified"), "`initial` is misspecified")
})

test_that("parallel optimization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(ackley$optimize(ncores = "1"), "`ncores` must be a positive integer.")
  skip_on_cran()
  t1_seq <- Sys.time()
  ackley$optimize(runs = 5000, ncores = 1, save_results = FALSE)
  t2_seq <- Sys.time()
  t1_par <- Sys.time()
  ackley$optimize(runs = 5000, ncores = 2, save_results = FALSE)
  t2_par <- Sys.time()
  expect_gt(difftime(t2_seq, t1_seq), difftime(t2_par, t1_par))
})

test_that("Nop object can be tested", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  ackley$set_optimizer(optimizer_nlm())
  ackley$set_optimizer(optimizer_optim())
  expect_error(ackley$test(time_limit = -1), "`time_limit` is not a positive integer.")
  expect_error(ackley$test(verbose = "FALSE"), "`verbose` must be either `TRUE` or `FALSE`.")
  sink(tempfile())
  # TODO: sink does not work here
  expect_true(ackley$test())
  sink()
  expect_true(ackley$test(verbose = FALSE))
})

test_that("standardization works", {
  # TODO
})

test_that("reducing works", {
  # TODO
})

test_that("overview of optima works", {
  # TODO
})

test_that("summary works", {
  # TODO
})

test_that("plotting works", {
  # TODO
})






