test_that("ino setups work", {
  out <- setup_ino(
    f = f_ll_hmm,
    npar = 4,
    data = ino::earthquakes,
    N = 2,
    neg = TRUE,
    opt = set_optimizer_nlm(),
    verbose = FALSE,
    skip_test = FALSE
  )
  expect_s3_class(out, "ino")
  expect_type(grid_ino(out), "list")
  expect_s3_class(clear_ino(out), "ino")
  expect_s3_class(merge_ino(out, out), "ino")
})

test_that("set optimizer works", {
  opt <- set_optimizer(
    opt = pracma::nelder_mead,
    f = "fn",
    p = "x0",
    v = "fmin",
    z = "xmin",
    tol = 1e-6,
    crit = c("xmin", "fcount")
  )
  expect_s3_class(opt, "optimizer")
})

test_that("set optimizer nlm works", {
  expect_s3_class(set_optimizer_nlm(), "optimizer")
})

test_that("set optimizer optim works", {
  expect_s3_class(set_optimizer_optim(), "optimizer")
})
