# Example 0: Polynomial ---------------------------------------------------

test_that("Example 0: Defining the problem works", {
  f <- function(x) -x^4 - x^3 - x^2 - x
  gradient <- function(x) -4*x^3 - 3*x^2 - 2*x - 1
  hessian <- function(x) -12*x^2 - 6*x - 2
  Nop_pol <- Nop$new(f = f, target = "x", npar = 1, gradient = gradient, hessian = hessian)

  checkmate::expect_r6(Nop_pol, "Nop")
  expect_identical(Nop_pol$npar, c("x" = 1))
  expect_error(Nop_pol$npar <- 2, "read-only")

  expect_equal(
    Nop_pol$evaluate(0, TRUE, TRUE),
    structure(0, gradient = -1, hessian = -2)
  )

  expect_snapshot(Nop_pol)

  checkmate::expect_tibble(Nop_pol$results, nrows = 0, ncols = 4)
})

# Example 1: Ackley function minimization ---------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(f = ackley, npar = 2)
Nop_ackley$verbose <- FALSE
nlm_opt <- optimizeR::Optimizer$new(which = "stats::nlm")
optim_opt <- optimizeR::Optimizer$new(which = "stats::optim")

self <- Nop_ackley
private <- self$.__enclos_env__$private

test_that("Example 1: Defining optimizers works", {
  expect_error(Nop_ackley$optimize(), "No optimizer specified yet.")

  Nop_ackley$
    set_optimizer(nlm_opt, optimizer_label = "nlm")$
    set_optimizer(optim_opt)
})

test_that("Example 1: Minimization works", {
  Nop_ackley$initialize_reset()
  expect_error(
    Nop_ackley$fresh_label <- "label",
    "read-only"
  )
  expect_warning(
    Nop_ackley$optimize(),
    "No initial values defined by user"
  )
  Nop_ackley$
    initialize_random(runs = 5)$optimize()$
    initialize_random(sampler = function() runif(2))$optimize()$
    initialize_fixed(0:1)$optimize()$
    initialize_fixed(list(1:2, 2:3, 3:4))$optimize()$
    initialize_grid()
  expect_warning(
    Nop_ackley$optimize(which_optimizer = 3)
  )
  expect_snapshot(Nop_ackley)
})

test_that("Example 1: Results can be accessed", {
  checkmate::expect_tibble(Nop_ackley$results)
  checkmate::expect_list(Nop_ackley$minimum, len = 2)
  expect_warning(Nop_ackley$maximum, "No results available.")
  #checkmate::expect_tibble(Nop_ackley$optima)
})

test_that("Example 1: Maximization works", {
  Nop_ackley$
    initialize_random(sampler = function() stats::runif(sum(self$npar)))$
    optimize(
      which_direction = "max", which_optimizer = "stats::optim", lower = 0.5, upper = 1
    )
  checkmate::expect_list(Nop_ackley$maximum, len = 2)
})

