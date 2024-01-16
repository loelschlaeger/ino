
# Example 1: Ackley function minimization ---------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(objective = ackley, npar = 2)

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

# TODO remove
self <- Nop_hmm
private <- self$.__enclos_env__$private

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


# test_that("Example 2: True value and parameter can be set", {
#   Nop_hmm$true(theta, which_direction = "max")
#   expect_snapshot(print(Nop_hmm))
# })
#
# test_that("Example 2: Evaluation and optimization works", {
#   Nop_hmm$argument("remove", name = "N")
#   expect_error(
#     Nop_hmm$evaluate(),
#     "is not specified"
#   )
#   Nop_hmm$argument("set", "N" = 2)
#   expect_equal(
#     Nop_hmm$evaluate(at = theta),
#     ll_hmm(theta = theta, data = hmm_data, N = 2)
#   )
#   Nop_hmm$
#     argument("set", "neg" = FALSE)$
#     initialize_fixed(at = theta)$
#     optimize(optimization_label = "maximize", which_direction = "max")
# })
#
# # General checks ----------------------------------------------------------
#
# test_that("Initialization checks work", {
#   expect_error(
#     Nop$new(),
#     "specify argument"
#   )
#   expect_error(
#     Nop$new(f = 1),
#     "Must be a function, not 'double'"
#   )
#   expect_error(
#     Nop$new(f = function(x) x),
#     "specify argument"
#   )
#   expect_error(
#     Nop$new(f = function(x) x, npar = 0),
#     "Assertion on 'npar' failed: Must be >= 1"
#   )
#   expect_error(
#     Nop$new(f = function() 1, npar = 2),
#     "must have at least one argument"
#   )
#   expect_warning(
#     Nop$new(f = function(x) {
#       1
#     }, npar = 2),
#     "is unnamed"
#   )
#   expect_error(
#     {Nop_ackley$npar <- 1},
#     "is read only"
#   )
# })
#
# test_that("Argument management checks work", {
#   expect_error(
#     Nop_hmm$argument("set", 1:10),
#     "All arguments to be set must be named."
#   )
#   expect_error(
#     Nop_hmm$argument("get"),
#     "Please specify"
#   )
#   expect_error(
#     Nop_hmm$argument("get", name = "does_not_exist"),
#     "is not specified"
#   )
#   expect_error(
#     Nop_hmm$argument("get", name = 1),
#     "Must be of type 'string', not 'double'"
#   )
#   expect_error(
#     Nop_hmm$argument("remove"),
#     "Please specify"
#   )
#   expect_error(
#     Nop_hmm$argument("remove", name = "does_not_exist"),
#     "is not specified"
#   )
#   expect_error(
#     Nop_hmm$argument("remove", name = 1),
#     "Must be of type 'string', not 'double'"
#   )
# })
#
# test_that("Optimizer definition checks work", {
#   expect_error(
#     Nop_ackley$set_optimizer(),
#     "Please specify argument"
#   )
#   expect_error(
#     Nop_ackley$set_optimizer("not_an_optimizer_object"),
#     "must be an"
#   )
#   expect_error(
#     Nop_ackley$set_optimizer(optimizer_nlm(), optimizer_label = 1),
#     "Must be of type 'string', not 'double'"
#   )
#   expect_error(
#     Nop_ackley$set_optimizer(optimizer_nlm(), optimizer_label = "nlm"),
#     "already exists, use another one"
#   )
# })
#
# test_that("Function evaluation checks work", {
#   expect_error(
#     Nop_ackley$evaluate(1),
#     "must be of length 2"
#   )
# })
#
# test_that("Warnings in function evaluation can be hidden", {
#   warning_f <- function(x) { warning("huhu"); x}
#   Nop_warning <- Nop$new(f = warning_f, npar = 1)
#   expect_warning(
#     Nop_warning$evaluate(at = 1),
#     "huhu"
#   )
#   expect_warning(
#     Nop_warning$evaluate(at = 1, hide_warnings = TRUE),
#     regexp = NA
#   )
# })
#
# test_that("Errors in function evaluation can be returned", {
#   error_f <- function(x) { stop("ups"); x}
#   Nop_error <- Nop$new(f = error_f, npar = 1)
#   expect_equal(
#     Nop_error$evaluate(at = 1),
#     "ups"
#   )
# })
#
# test_that("Long function evaluation can be interrupted", {
#   skip_if_not(.Platform$OS.type == "windows")
#   long_f <- function(x) {
#     for (i in 1:7) Sys.sleep(0.1)
#     x
#   }
#   Nop_long <- Nop$new(f = long_f, npar = 1)
#   expect_equal(
#     Nop_long$evaluate(at = 1, time_limit = 0.5),
#     "time limit reached"
#   )
#   expect_equal(
#     Nop_long$evaluate(at = 1, time_limit = 1),
#     1
#   )
# })
#
# test_that("Optimization checks work", {
#   expect_warning(
#     Nop_ackley$optimize(),
#     "No initial values defined"
#   )
#   expect_error(
#     Nop_ackley$optimize(verbose = "yes"),
#     "Must be of type 'logical flag', not 'character'"
#   )
#   expect_error(
#     Nop_ackley$optimize(hide_warnings = "bad"),
#     "Must be of type 'logical flag', not 'character'"
#   )
#   expect_error(
#     Nop_ackley$optimize(optimization_label = 1),
#     "Must be of type 'string', not 'double'"
#   )
# })

