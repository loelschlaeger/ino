test_that("Records object can be initialized", {
  object <- Records$new()
  expect_s3_class(object, c("Records", "R6"), exact = TRUE)
})

test_that("saving results works", {
  object <- Records$new()
  results <- list("value" = 1)
  object$save(
    results = results,
    results_depth = 1,
    optimizer_label = "nlmxx",
    optimization_label = "optimization_label",
    comparable = TRUE
  )
  results <- list(
    list("value" = 1),
    list("value" = 2)
  )
  object$save(
    results = results,
    results_depth = 2,
    optimizer_label = c("nlm", "optim"),
    optimization_label = "optimization_label",
    comparable = TRUE
  )
  results <- list(
    list(list("value" = 1), list("value" = 2)),
    list(list("value" = 2), list("value" = 3))
  )
  object$save(
    results = results,
    results_depth = 3,
    optimizer_label = c("nlm", "optim"),
    optimization_label = "optimization_label",
    comparable = TRUE
  )
})
