test_that("wrapper for ao optimizer works", {
  expect_s3_class(optimizer_ao(), "optimizer")
  out <- apply_optimizer(optimizer_ao(partition = as.list(1:2), optimizer = optimizer_nlm()), f_ackley, 1:2)
  expect_type(out, "list")
})
