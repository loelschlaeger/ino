options("ino_progress" = FALSE)

test_that("runs object can be constructed and validated", {
  x <- new_runs()
  expect_s3_class(x, "runs")
  expect_type(x, "list")
  x <- validate_runs(x)
  expect_s3_class(x, "runs")
})

test_that("opts object can be constructed and validated", {
  x <- new_opts()
  expect_s3_class(x, "opts")
  expect_type(x, "list")
  expect_true(all(sapply(x, function(x) inherits(x, "optimizer"))))
  x <- validate_opts(x)
  expect_s3_class(x, "opts")
})

test_that("prob object can be constructed and validated", {
  x <- new_prob(f = function(x) x^2, npar = 1, f_name = "test", f_target = "x")
  expect_type(x, "list")
  expect_s3_class(x, "prob")
  x <- validate_prob(x)
  expect_s3_class(x, "prob")
})

test_that("ino setup works", {
  x <- setup_ino(f = f_ackley, npar = 2)
  expect_s3_class(x, "ino")
})

test_that("ino grid can be created", {
  x <- setup_ino(
    f = f_ll_hmm,
    npar = 4,
    data = list("data1" = earthquakes, "data2" = earthquakes),
    N = 2,
    neg = TRUE,
    mpvs = "data",
    opt = set_optimizer_nlm()
  )
  grid <- grid_ino(x)
  expect_s3_class(grid, "grid")
})
