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
})

test_that("true value and true parameter can be set", {
  # TODO
  f <- f_ackley
  npar <- 2
  true_par <- c(0, 0)
  true_val <- 0
  ackley <- Nop$new(f = f_ackley, npar = 2)

})

test_that("Nop object can be printed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())

  ackley$set_true_par(true_par = c(0, 0))
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())

  ackley$set_true_par(true_par = c(0, 0), set_true_val = TRUE)
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())

  ackley$set_optimizer(optimizer = optimizer_nlm(), label = "nlm")
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())

  ackley$optimize(seed = 1)
  expect_snapshot(ackley)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})
