# Example 1: Ackley function minimization ---------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(f = ackley, npar = 2)
nlm_opt <- optimizeR::Optimizer$new(which = "stats::nlm")
optim_opt <- optimizeR::Optimizer$new(which = "stats::optim")

test_that("Example 1: Defining the problem works", {
  checkmate::expect_r6(Nop_ackley, "Nop")
  expect_identical(Nop_ackley$npar, c("x" = 2))
  expect_equal(
    Nop_ackley$evaluate(c(0, 1)),
    ackley(c(0, 1))
  )
})

test_that("Example 1: Optimization works", {
  Nop_ackley$
    set_optimizer(nlm_opt, optimizer_label = "nlm")$
    set_optimizer(optim_opt)

  Nop_ackley$optimize()

  Nop_ackley$
    initialize_random(runs = 5, seed = 1)$optimize()$
    initialize_random(sampler = function() runif(2), seed = 1)$optimize()$
    initialize_fixed(0:1)$optimize()$
    initialize_fixed(list(1:2, 2:3, 3:4))$optimize()
  expect_snapshot(Nop_ackley)
})
