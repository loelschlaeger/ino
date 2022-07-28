test_that("runs object can be constructed", {
  x <- new_runs()
  expect_s3_class(x, "runs")
  expect_type(x, "list")
  expect_named(x, c("table", "pars"))
  expect_true(is.data.frame(x$table))
  expect_true(is.list(x$pars))
  expect_equal(nrow(x$table), length(x$pars))
})

test_that("runs object can be validated", {
  x <- validate_runs(new_runs())
  expect_s3_class(x, "runs")
})

test_that("opti object can be constructed", {
  x <- new_opti()
  expect_s3_class(x, "opti")
  expect_type(x, "list")
  expect_true(all(sapply(x, function(x) inherits(x, "optimizer"))))
})

test_that("opti object can be validated", {
  x <- validate_opti(new_opti())
  expect_s3_class(x, "opti")
})

test_that("prob object can be constructed", {
  x <- new_prob()
  expect_type(x, "list")
  expect_s3_class(x, "prob")
})

test_that("prob object can be validated", {
  x <- validate_prob(new_prob())
  expect_s3_class(x, "prob")
})
