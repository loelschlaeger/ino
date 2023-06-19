test_that("Runs object can be initialized", {
  object <- Runs$new()
  expect_s3_class(object, c("Runs", "R6"), exact = TRUE)
})
