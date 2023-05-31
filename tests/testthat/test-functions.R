test_that("ackley function can be evaluated", {
  expect_equal(f_ackley(c(0, 0)), 0)
})

test_that("beale function can be evaluated", {
  expect_equal(f_beale(c(3, 0.5)), 0)
})

test_that("matyas function can be evaluated", {
  expect_equal(f_matyas(c(0, 0)), 0)
})

test_that("easom function can be evaluated", {
  expect_equal(f_easom(c(pi, pi)), -1)
})

test_that("HMM data simulation and likelihood computation works", {
  tpm <- matrix(c(0.8,0.1,0.2,0.9), nrow = 2)
  mu <- c(-2,2)
  sigma <- c(0.5,1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  ll <- f_ll_hmm(theta = theta, data = data, N = 2)
  expect_type(ll, "double")
})

test_that("MNP data simulation and likelihood computation works", {
  data <- sim_mnp(N = 3, Tp = 2, J = 2, P = 2, b = c(1, -1), Omega = diag(2))
  expect_true(is.data.frame(data))
  theta <- attr(data, "true")
  ll <- f_ll_mnp(theta = theta, data = data)
  expect_true(is.numeric(ll))
})
