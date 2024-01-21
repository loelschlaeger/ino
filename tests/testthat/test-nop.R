
# Example 1: Ackley function minimization ---------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(objective = ackley, npar = 2)
Nop_ackley$verbose <- FALSE

test_that("Example 1: Defining the problem works", {
  checkmate::expect_r6(Nop_ackley, "Nop")
  expect_snapshot(Nop_ackley$print())
  expect_snapshot(print(Nop_ackley))
  expect_identical(Nop_ackley$npar, c("x" = 2))
  nlm <- optimizeR::Optimizer$new(which = "stats::nlm")
  optim <- optimizeR::Optimizer$new(which = "stats::optim")
  Nop_ackley$
    set_optimizer(nlm, optimizer_label = "nlm")$
    set_optimizer(optim)
  Nop_ackley$evaluate(c(0, 0))
  expect_snapshot(Nop_ackley)
})

test_that("Example 1: Evaluation and optimization works", {
  expect_equal(
    Nop_ackley$evaluate(c(0, 1)),
    ackley(c(0, 1))
  )
  Nop_ackley$
    initialize_random(runs = 5, seed = 1)$optimize()$
    initialize_random(sampler = function() runif(2), seed = 1)$optimize()$
    initialize_fixed(0:1)$optimize()$
    initialize_fixed(list(1:2, 2:3, 3:4))$optimize()
  expect_snapshot(Nop_ackley)
})

# Example 2: HMM likelihood maximization ----------------------------------

hmm_data <- fHMM::simulate_hmm(seed = 1)$data

Nop_hmm <- Nop$new(
  objective = fHMM::ll_hmm,
  npar = 6,
  sdds = "normal",
  states = 2,
  negative = TRUE
)
Nop_hmm$verbose <- FALSE

test_that("Example 2: Defining the problem works", {
  checkmate::expect_r6(Nop_hmm, "Nop")
  expect_snapshot(Nop_hmm$print())
  expect_snapshot(print(Nop_hmm))
  expect_identical(Nop_hmm$npar, c("parUncon" = 6))
  Nop_hmm$set_optimizer(optimizeR::optimizer_nlm())
  expect_snapshot(Nop_hmm)
})

Nop_hmm$fixed_argument("set", "observations" = hmm_data)

test_that("Example 2: Additional arguments can be modified and reset", {
  expect_snapshot(print(Nop_hmm))
  expect_identical(
    Nop_hmm$fixed_argument("get", argument_name = "observations"),
    hmm_data
  )
  Nop_hmm$fixed_argument("remove", argument_name = "observations")
  expect_error(
    Nop_hmm$fixed_argument("get", argument_name = "observations"),
    "not specified"
  )
  Nop_hmm$set_argument("observations" = hmm_data)
  Nop_hmm$fixed_argument("modify", "observations" = 1:3)
  expect_identical(
    Nop_hmm$fixed_argument("get", argument_name = "observations"),
    1:3
  )
  expect_snapshot(print(Nop_hmm))
  Nop_hmm$fixed_argument("reset", argument_name = "observations")
  expect_identical(
    Nop_hmm$fixed_argument("get", argument_name = "observations"),
    hmm_data
  )
})

test_that("Example 2: Observations can be standardized", {
  Nop_hmm$standardize("observations")
  out <- Nop_hmm$fixed_argument("get", argument_name = "observations")
  expect_equal(
    mean(out), 0, tolerance = 1e-6
  )
  expect_equal(
    sd(out), 1, tolerance = 1e-6
  )
  Nop_hmm$fixed_argument("reset", argument_name = "observations")
  expect_identical(
    Nop_hmm$fixed_argument("get", argument_name = "observations"),
    hmm_data
  )
})

test_that("Example 2: Observations can be reduced", {
  Nop_hmm$reduce("observations")
  out <- Nop_hmm$fixed_argument("get", argument_name = "observations")
  expect_length(out, 50)
  Nop_hmm$fixed_argument("reset", argument_name = "observations")
  expect_length(
    Nop_hmm$fixed_argument("get", argument_name = "observations"), 100
  )
})

