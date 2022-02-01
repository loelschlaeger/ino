test_that("set_f filters wrong arguments", {
  expect_error(set_f(f = "f", npar =  0))
  expect_error(set_f(f = function(x) x, npar = 0))
  expect_s3_class(set_f(f = function(x) x, npar = 1), "ino")
})
