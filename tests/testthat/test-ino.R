test_that("runs object can be constructed and validated", {
  x <- new_runs()
  expect_s3_class(x, "runs")
  expect_type(x, "list")
  expect_named(x, c("table", "pars"))
  expect_true(is.data.frame(x$table))
  expect_true(is.list(x$pars))
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
