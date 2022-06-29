test_that("ackley function works", {
  expect_equal(f_ackley(c(0, 0)), 0)
})

test_that("beale function works", {
  expect_equal(f_beale(c(3, 0.5)), 0)
})

test_that("matyas function works", {
  expect_equal(f_matyas(c(0, 0)), 0)
})

test_that("easom function works", {
  expect_equal(f_easom(c(pi, pi)), -1)
})

test_that("HMM likelihood works", {
  ll <- f_ll_hmm(theta = c(-1, -1, 1, 2), data = ino::earthquakes, N = 2)
  expect_type(ll, "double")
})

test_that("MNP likelihood works", {
  data <- ino::probit_data[[1]]
  theta <- attr(data, "true")
  ll <- f_ll_mnp(theta = theta, data = data)
  expect_type(ll, "double")
})

test_that("MNP data simulation works", {
  data <- sim_mnp(N = 3, T = 2, b = c(1, -1), Omega = diag(2), Sigma = diag(2))
  expect_true(is.data.frame(data))
})

test_that("MNL likelihood works", {
  data <- ino::logit_data[[1]]
  theta <- attr(data, "true")
  ll <- f_ll_mnl(theta = theta, data = data)
  expect_type(ll, "double")
})

test_that("MNL data simulation works", {
  data <- sim_mnl(N = 3, T = 2, J = 3, b = c(-2, 0.5, 2), Omega = diag(3))
  expect_true(is.data.frame(data))
})
