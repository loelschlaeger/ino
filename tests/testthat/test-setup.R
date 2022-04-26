test_that("setup ino works", {
  out <- setup_ino(
    f = ino:::f_ll_hmm,
    npar = 4,
    data = ino::earthquakes,
    N = 2,
    neg = TRUE,
    opt = set_optimizer_nlm(),
    verbose = FALSE
  )
  expect_s3_class(out, "ino")
})
