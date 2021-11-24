test_that("set_f filters wrong arguments", {
  expect_error(set_f("f", 0))
  expect_error(set_f(function(x) x, 0))
  expect_s3_class(set_f(function(x) x, 1), "ino_f")
})
