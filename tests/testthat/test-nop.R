test_that("Nop object can be initialized", {
  f <- f_ackley
  npar <- 2L
  test <- Nop$new(f = f, npar = npar)
  expect_s3_class(test, c("Nop", "R6"), exact = TRUE)
  expect_error(Nop$new(), "Please specify argument 'f'.")
  expect_error(Nop$new(f = 1), "Argument 'f' is not a function.")
  expect_error(Nop$new(f = f), "Please specify argument 'npar'.")
  expect_error(Nop$new(f = f, npar = 0), "Argument 'npar' must be a positive integer.")
  expect_identical(test$f, f)
  expect_identical(test$npar, npar)
  expect_error({test$f <- function(x) x})
  expect_error({test$npar <- 1})
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("function arguments can be set", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 4)$
    set_argument("data", earthquakes)$
    set_argument("N", 2)$
    set_argument("neg", TRUE)
  expect_type(hmm$arguments, "list")
  expect_length(hmm$arguments, 3)
  expect_error({hmm$arguments$N <- 3})
  expect_snapshot(hmm)
  expect_error(hmm$set_argument("N", 4), "Argument 'N' already exists.")
})

test_that("function arguments can be removed", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 4)$
    set_argument("data", earthquakes)$
    set_argument("N", 2)$
    set_argument("neg", TRUE)
  expect_true("N" %in% names(hmm$arguments))
  hmm$remove_argument("N")
  expect_false("N" %in% names(hmm$arguments))
  expect_error(hmm$remove_argument("N"), "Argument 'N' does not exist.")
})

test_that("true value can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_value)
  ackley$true_value <- 2
  expect_equal(ackley$true_value, 2)
  ackley$set_true_value(3)
  expect_equal(ackley$true_value, 3)
  expect_snapshot(ackley)
})

test_that("true parameter can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_parameter)
  expect_error({ackley$true_parameter <- 2})
  ackley$true_parameter <- c(0,0)
  expect_equal(ackley$true_parameter, c(0,0))
  expect_error(ackley$set_true_parameter(3))
  ackley$set_true_parameter(c(1,0))
  expect_equal(ackley$true_parameter, c(1,0))
  expect_null(ackley$true_value)
  ackley$set_true_parameter(c(0,0), set_true_value = TRUE)
  expect_equal(ackley$true_value, f_ackley(c(0,0)))
  expect_snapshot(ackley)
})

test_that("optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
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

test_that("optimizer details can be printed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm(), label = "A")$
    set_optimizer(optimizer_nlm(), label = "B")$
    set_optimizer(optimizer_nlm(), label = "C")$
    set_optimizer(optimizer_nlm())
  expect_snapshot(ackley$optimizer_details("all"))
  expect_snapshot(ackley$optimizer_details(c(3,4,1)))
  expect_snapshot(ackley$optimizer_details(c("stats::nlm","B")))
})

test_that("function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(ackley$evaluate(1), "Argument 'at' must be a numeric vector of length 2.")
  expect_type(ackley$evaluate(c(1,2)), "double")
  hmm <- Nop$new(f = f_ll_hmm, npar = 4)$
    set_argument("data", earthquakes)$
    set_argument("N", 2)$
    set_argument("neg", TRUE)
  hmm$evaluate(c(0,2,1,4))
})

test_that("function can be optimized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$set_optimizer(optimizer_nlm())
})










