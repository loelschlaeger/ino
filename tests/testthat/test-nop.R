test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(Nop$new(), "Please specify argument 'f'.")
  expect_error(Nop$new(f = 1), "Argument 'f' is not a function.")
  expect_error(Nop$new(f = f_ackley), "Please specify argument 'npar'.")
  expect_error(Nop$new(f = f_ackley, npar = 0),
               "Argument 'npar' must be a positive integer.")
  expect_identical(ackley$f, f_ackley)
  expect_identical(ackley$npar, 2L)
  expect_error({ackley$f <- function(x) x}, "is read only.")
  expect_error({ackley$npar <- 1}, "is read only.")
  expect_error(Nop$new(f = function() 1, npar = 0),
               "should have at least one argument.")
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
  expect_equal(hmm$get_argument("test_arg1"), 1)
  expect_error(hmm$get_argument("does_not_exist"), "does not exist")
  expect_error(hmm$remove_argument(), "Please specify 'argument_name'.")
  expect_error(hmm$remove_argument(argument_name = 1:2))
})

test_that("function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$evaluate(1),
               "Argument 'at' must be a numeric vector of length 2.")
  expect_type(ackley$evaluate(c(1, 2)), "double")
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, "data" = earthquakes)$
    set_argument("N" = 2, "neg" = TRUE)
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
  expect_error({ackley$true_parameter <- 2},
               "must be a numeric vector of length 2.")
  ackley$true_parameter <- c(0,0)
  expect_equal(ackley$true_parameter, c(0,0))
  expect_error(ackley$set_true_parameter(1:2, "not_a_logical"),
               "must be `TRUE` or `FALSE`.")
  expect_error(ackley$set_true_parameter(3))
  ackley$set_true_parameter(c(1,0))
  expect_equal(ackley$true_parameter, c(1,0))
  expect_null(ackley$true_value)
  ackley$set_true_parameter(c(0,0), set_true_value = TRUE)
  expect_equal(ackley$true_value, f_ackley(c(0,0)))
  expect_snapshot(ackley)
  expect_error(ackley$set_true_parameter(c(1,1), set_true_value = FALSE),
               "also update the true optimum value.")
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$set_optimizer(), "Please specify argument 'optimizer'.")
  expect_error(ackley$set_optimizer("not_an_optimizer_object"),
               "must be an object of class 'optimizer'.")
  expect_error(ackley$set_optimizer(optimizer_nlm(), label = 1),
               "must be a single character.")
  ackley$set_optimizer(optimizer_nlm(), label = "nlm")
  expect_snapshot(ackley)
  expect_error(ackley$set_optimizer(optimizer_nlm(), label = "nlm"))
  ackley$set_optimizer(optimizer_optim())
  expect_snapshot(ackley)
})

test_that("optimizer can be removed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm(), label = "A")$
    set_optimizer(optimizer_nlm(), label = "B")$
    set_optimizer(optimizer_nlm(), label = "C")$
    set_optimizer(optimizer_nlm())
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
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  out <- ackley$optimize(runs = 5, return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  expect_length(out, 5)
  expect_length(out[[1]], 2)
})

test_that("Nop object can be tested", {
  # TODO
})

test_that("standardization works", {
  # TODO
})

test_that("reducing works", {
  # TODO
})






