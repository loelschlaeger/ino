
# Example 1: Ackley function minimization ---------------------------------

ackley <- TestFunctions::TF_ackley
Nop_ackley <- Nop$new(f = ackley, npar = 2)

test_that("Example 1: Defining the problem works", {
  checkmate::expect_r6(Nop_ackley, "Nop")
  expect_snapshot(Nop_ackley$print())
  expect_snapshot(print(Nop_ackley))
  expect_identical(Nop_ackley$npar, 2L)
  Nop_ackley$
    set_optimizer(optimizeR::optimizer_nlm(), optimizer_label = "nlm")$
    set_optimizer(optimizeR::optimizer_optim())
  Nop_ackley$true(c(0, 0))
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

sim_hmm <- function(Tp, N, theta, seed = NULL) {
  stopifnot(
    is.numeric(Tp), length(Tp) == 1, Tp > 0, Tp %% 1 == 0, is.numeric(N),
    length(N) == 1, N > 0, N %% 1 == 0, is.numeric(theta),
    length(theta) == N * (N - 1) + 2 * N
  )
  if (!is.null(seed)) set.seed(seed)
  tpm <- matrix(1, N, N)
  tpm[row(tpm) != col(tpm)] <- exp(theta[1:(N * (N - 1))])
  tpm <- tpm / rowSums(tpm)
  mu <- theta[(N * (N - 1) + 1):(N * (N - 1) + N)]
  sigma <- exp(theta[(N - 1) * N + (N + 1):(2 * N)])
  delta <- try(solve(t(diag(N) - tpm + 1), rep(1, N)), silent = TRUE)
  if (inherits(delta, "try-error")) delta <- rep(1, N) / N
  s <- numeric(Tp)
  s[1] <- sample(1:N, size = 1, prob = delta)
  x <- numeric(Tp)
  x[1] <- stats::rnorm(1, mean = mu[s[1]], sd = sigma[s[1]])
  for(t in 2:Tp){
    s[t] <- sample(1:N, size = 1, prob = tpm[s[t-1],])
    x[t] <- stats::rnorm(1, mean = mu[s[t-1]], sd = sigma[s[t-1]])
  }
  return(x)
}

ll_hmm <- function(theta, data, N, neg = FALSE) {
  stopifnot(
    is.numeric(theta), is.vector(data), is.numeric(data), is.numeric(N),
    length(N) == 1, N > 0, N %% 1 == 0, length(theta) == N * (N - 1) + 2 * N
  )
  Tp <- length(data)
  tpm <- matrix(1, N, N)
  tpm[row(tpm) != col(tpm)] <- exp(theta[1:(N * (N - 1))])
  tpm <- tpm / rowSums(tpm)
  mu <- theta[(N * (N - 1) + 1):(N * (N - 1) + N)]
  sigma <- exp(theta[(N - 1) * N + (N + 1):(2 * N)])
  delta <- try(solve(t(diag(N) - tpm + 1), rep(1, N)), silent = TRUE)
  if (inherits(delta, "try-error")) delta <- rep(1, N) / N
  allprobs <- matrix(1, Tp, N)
  for(n in 1:N){
    allprobs[, n] <- stats::dnorm(data, mean = mu[n], sd = sigma[n])
  }
  foo <- delta %*% diag(allprobs[1,])
  llk <- log(sum(foo))
  phi <- foo/sum(foo)
  for(t in 2:Tp){
    foo <- phi %*% tpm %*% diag(allprobs[t, ])
    llk <- llk + log(sum(foo))
    phi <- foo/sum(foo)
  }
  return(ifelse(neg, -llk, llk))
}

tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
mu <- c(-2, 2)
sigma <- c(0.5, 1)
theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
hmm_data <- sim_hmm(Tp = 100, N = 2, theta = theta, seed = 1)

Nop_hmm <- Nop$new(f = ll_hmm, npar = 6, N = 2)

test_that("Example 2: Defining the problem works", {
  checkmate::expect_r6(Nop_hmm, "Nop")
  expect_snapshot(Nop_hmm$print())
  expect_snapshot(print(Nop_hmm))
  expect_identical(Nop_hmm$npar, 6L)
  Nop_hmm$set_optimizer(optimizeR::optimizer_nlm())
  expect_snapshot(Nop_hmm)
})

test_that("Example 2: Additional arguments can be modified and reset", {
  Nop_hmm$argument("set", data = hmm_data)
  expect_snapshot(print(Nop_hmm))
  expect_identical(
    Nop_hmm$argument("get", name = "data"),
    hmm_data
  )
  Nop_hmm$argument("standardize", name = "data")
  Nop_hmm$argument("subset", name = "data")
  expect_snapshot(print(Nop_hmm))
  Nop_hmm$argument("reset", name = "data")
  expect_identical(
    Nop_hmm$argument("get", name = "data"),
    hmm_data
  )
  Nop_hmm$argument("modify", "N" = 3)
  expect_identical(
    Nop_hmm$argument("get", name = "N"),
    3
  )
  Nop_hmm$argument("reset", name = "N")
  expect_identical(
    Nop_hmm$argument("get", name = "N"),
    2
  )
})

test_that("Example 2: True value and parameter can be set", {
  Nop_hmm$true(theta, which_direction = "max")
  expect_snapshot(print(Nop_hmm))
})

test_that("Example 2: Evaluation and optimization works", {
  Nop_hmm$argument("remove", name = "N")
  expect_error(
    Nop_hmm$evaluate(),
    "is not specified"
  )
  Nop_hmm$argument("set", "N" = 2)
  expect_equal(
    Nop_hmm$evaluate(at = theta),
    ll_hmm(theta = theta, data = hmm_data, N = 2)
  )
})

# General checks ----------------------------------------------------------

test_that("Initialization checks work", {
  expect_error(
    Nop$new(),
    "specify argument"
  )
  expect_error(
    Nop$new(f = 1),
    "Must be a function, not 'double'"
  )
  expect_error(
    Nop$new(f = function(x) x),
    "specify argument"
  )
  expect_error(
    Nop$new(f = function(x) x, npar = 0),
    "Assertion on 'npar' failed: Must be >= 1"
  )
  expect_error(
    Nop$new(f = function() 1, npar = 2),
    "must have at least one argument"
  )
  expect_warning(
    Nop$new(f = function(x) {
      1
    }, npar = 2),
    "is unnamed"
  )
  expect_error(
    {Nop_ackley$npar <- 1},
    "is read only"
  )
})

test_that("Argument management checks work", {
  expect_error(
    Nop_hmm$argument("set", 1:10),
    "All arguments to be set must be named."
  )
  expect_error(
    Nop_hmm$argument("get"),
    "Please specify"
  )
  expect_error(
    Nop_hmm$argument("get", name = "does_not_exist"),
    "is not specified"
  )
  expect_error(
    Nop_hmm$argument("get", name = 1),
    "Must be of type 'string', not 'double'"
  )
  expect_error(
    Nop_hmm$argument("remove"),
    "Please specify"
  )
  expect_error(
    Nop_hmm$argument("remove", name = "does_not_exist"),
    "is not specified"
  )
  expect_error(
    Nop_hmm$argument("remove", name = 1),
    "Must be of type 'string', not 'double'"
  )
})

test_that("Optimizer definition checks work", {
  expect_error(
    Nop_ackley$set_optimizer(),
    "Please specify argument"
  )
  expect_error(
    Nop_ackley$set_optimizer("not_an_optimizer_object"),
    "must be an"
  )
  expect_error(
    Nop_ackley$set_optimizer(optimizer_nlm(), optimizer_label = 1),
    "Must be of type 'string', not 'double'"
  )
  expect_error(
    Nop_ackley$set_optimizer(optimizer_nlm(), optimizer_label = "nlm"),
    "already exists, use another one"
  )
})

test_that("Function evaluation checks work", {
  expect_error(
    Nop_ackley$evaluate(1),
    "must be of length 2"
  )
})

test_that("Warnings in function evaluation can be hidden", {
  warning_f <- function(x) { warning("huhu"); x}
  Nop_warning <- Nop$new(f = warning_f, npar = 1)
  expect_warning(
    Nop_warning$evaluate(at = 1),
    "huhu"
  )
  expect_warning(
    Nop_warning$evaluate(at = 1, hide_warnings = TRUE),
    regexp = NA
  )
})

test_that("Errors in function evaluation can be returned", {
  error_f <- function(x) { stop("ups"); x}
  Nop_error <- Nop$new(f = error_f, npar = 1)
  expect_equal(
    Nop_error$evaluate(at = 1),
    "ups"
  )
})

test_that("Long function evaluation can be interrupted", {
  skip_if_not(.Platform$OS.type == "windows")
  long_f <- function(x) {
    for (i in 1:7) Sys.sleep(0.1)
    x
  }
  Nop_long <- Nop$new(f = long_f, npar = 1)
  expect_equal(
    Nop_long$evaluate(at = 1, time_limit = 0.5),
    "time limit reached"
  )
  expect_equal(
    Nop_long$evaluate(at = 1, time_limit = 1),
    1
  )
})

test_that("Optimization checks work", {
  expect_warning(
    Nop_ackley$optimize(),
    "No initial values defined"
  )
  expect_error(
    Nop_ackley$optimize(verbose = "yes"),
    "Must be of type 'logical flag', not 'character'"
  )
  expect_error(
    Nop_ackley$optimize(hide_warnings = "bad"),
    "Must be of type 'logical flag', not 'character'"
  )
  expect_error(
    Nop_ackley$optimize(optimization_label = 1),
    "Must be of type 'string', not 'double'"
  )
})

#
#
# test_that("HMM likelihood function can be maximized", {
#   tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
#   mu <- c(-2, 2)
#   sigma <- c(0.5, 1)
#   theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
#   data <- sim_hmm(Tp = 100, N = 2, theta = theta)
#   hmm <- Nop$
#     new(f = f_ll_hmm, npar = 6, "data" = data, "N" = 2)$
#     set_optimizer(optimizer_nlm())
#   result1 <- hmm$
#     argument("set", neg = FALSE)$
#     initialize_fixed(theta)$
#     optimize(which_direction = "max", return_results = TRUE, simplify = TRUE)
#
#   result2 <- hmm$
#     argument("modify", neg = TRUE)$
#     initialize_fixed(theta)$
#     optimize(which_direction = "min", return_results = TRUE, simplify = TRUE)
#   expect_identical(result1$value, -result2$value)
#   expect_identical(result1$parameter, result2$parameter)
# })
#
# test_that("Nop object can be validated", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)
#   expect_error(
#     ackley$validate(at = 1),
#     "must be of length 2"
#   )
#   expect_warning(
#     ackley$validate(),
#     "No optimizer specified, testing optimizers is skipped."
#   )
#   ackley$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())
#   expect_true(ackley$validate(verbose = FALSE))
# })
#
# test_that("Bad functions and optimizers can be detected in validation", {
#   error_f <- Nop$new(f = function(x) stop("error message"), 1)
#   expect_error(
#     error_f$validate(),
#     "Function call threw an error"
#   )
#   lengthy_f <- Nop$new(f = function(x) 1:2, 1)
#   expect_error(
#     lengthy_f$validate(),
#     "Test function call is of length 2."
#   )
#   character_f <- Nop$new(f = function(x) "not_a_numeric", 1)
#   expect_error(
#     character_f$validate(),
#     "Function call threw an error"
#   )
#   list_f <- Nop$new(f = function(x) list(), 1)
#   expect_error(
#     list_f$validate(),
#     "Test function call did not return a"
#   )
#   error_optimizer_fun <- function(f, p) {
#     if (identical(p, 1:2)) stop("error message")
#     list(v = f(p), z = 1:2)
#   }
#   error_optimizer <- optimizeR::define_optimizer(
#     error_optimizer_fun,
#     .objective = "f", .initial = "p", .value = "v",
#     .parameter = "z", .direction = "min"
#   )
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(error_optimizer)
#   expect_error(
#     ackley$validate(at = 1:2),
#     "Optimization threw an error"
#   )
# })
#
# test_that("Validations can be interrupted", {
#   skip_if_not(.Platform$OS.type == "windows")
#   slow_f <- function(x) {
#     Sys.sleep(2)
#     1
#   }
#   slow_f <- Nop$new(slow_f, 1)$
#     set_optimizer(optimizer_nlm())
#   expect_warning(
#     expect_warning(
#       slow_f$validate(time_limit = 1),
#       "Time limit of 1s reached in the function call"
#     ),
#     "Time limit of 1s reached in the optimization"
#   )
#   slow_optimizer_fun <- function(f, p) {
#     Sys.sleep(2)
#     stats::nlm(f = f, p = p)
#   }
#   slow_optimizer <- optimizeR::define_optimizer(
#     slow_optimizer_fun,
#     .objective = "f", .initial = "p", .value = "minimum",
#     .parameter = "estimate", .direction = "min"
#   )
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(slow_optimizer)
#   expect_warning(
#     ackley$validate(at = 1:2, time_limit = 1),
#     "Time limit of 1s reached in the optimization"
#   )
# })
#
# test_that("Overview of available elements can be created", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())
#   expect_warning(
#     ackley$elements(),
#     "No optimization results saved yet"
#   )
#   ackley$
#     initialize_random(runs = 10)$
#     optimize()
#   expect_equal(
#     ackley$elements(),
#     list("stats::nlm" = c(
#       "value", "parameter", "seconds", "initial", "gradient", "code",
#       "iterations", "error", "error_message"
#     ))
#   )
# })
#
# test_that("Results can be accessed", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())$
#     initialize_random(runs = 10)$
#     optimize(save_results = TRUE, return_results = FALSE)
#   results <- ackley$results()
#   expect_type(results, "list")
#   expect_length(results, 20)
#   results <- ackley$results(
#     which_run = 1:10, which_optimizer = 2, which_element = "value"
#   )
#   expect_type(results, "list")
#   expect_length(results, 5)
# })
#
# test_that("Results can be summarized", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())
#   expect_warning(
#     ackley$summary(),
#     "No optimization results saved yet."
#   )
#   ackley$
#     initialize_random(runs = 10)$
#     optimize()
#   expect_warning(
#     out <- ackley$summary(
#       which_element = c("value", "iterations"), digits = 1
#     ),
#     "Elements not available"
#   )
#   expect_true(is.data.frame(out))
# })
#
# test_that("Results can be deleted", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())
#   expect_warning(
#     ackley$delete(which_run = 1),
#     "No optimization results saved yet"
#   )
#   expect_equal(
#     suppressWarnings(ackley$number()), 0
#   )
#   ackley$
#     initialize_random()$
#     optimize()
#   expect_equal(
#     suppressWarnings(ackley$number()), 1
#   )
#   ackley$delete(which_run = 1, prompt = FALSE)
#   expect_equal(
#     suppressWarnings(ackley$number()), 0
#   )
# })
#
# test_that("Overview of optima works", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())
#   expect_warning(
#     ackley$optima(),
#     "No optimization results saved yet."
#   )
#   ackley$
#     initialize_random(runs = 10)$
#     optimize()
#   expect_true(
#     is.data.frame(ackley$optima())
#   )
#   expect_error(
#     ackley$optima(sort_by = "bad_input"),
#     "must be one of"
#   )
# })
#
# test_that("Optimization times and values can be plotted", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())$
#     initialize_random(runs = 100)$
#     optimize(reset_initial = FALSE)$
#     optimize()
#   combinations <- expand.grid(
#     which_element = c("seconds", "value"),
#     group_by = list(".optimization_label", ".optimizer_label", NULL),
#     relative = c(TRUE, FALSE),
#     which_run = "all",
#     which_optimizer = "all",
#     only_comparable = c(TRUE, FALSE),
#     stringsAsFactors = FALSE
#   )
#   for (i in 1:nrow(combinations)) {
#     which_element <- combinations[i, "which_element"]
#     group_by <- combinations[[i, "group_by"]]
#     relative <- combinations[i, "relative"]
#     which_run <- combinations[i, "which_run"]
#     which_optimizer <- combinations[i, "which_optimizer"]
#     only_comparable <- combinations[i, "only_comparable"]
#     if (which_element == "value") {
#       relative <- FALSE
#     }
#     expect_s3_class(
#       ackley$plot(
#         which_element = which_element, group_by = group_by, relative = relative,
#         which_run = which_run, which_optimizer = which_optimizer,
#         only_comparable = only_comparable
#       ),
#       "ggplot"
#     )
#   }
# })
#
# test_that("Deviations can be calculated and ploted", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())$
#     true(c(0, 0), "parameter")$
#     initialize_random(runs = 10)$
#     optimize()
#   expect_true(
#     is.data.frame(ackley$deviation(plot = FALSE))
#   )
#   expect_s3_class(
#     ackley$deviation(),
#     "ggplot"
#   )
# })
#
# test_that("Optimization trace can be extracted", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)
#   expect_s3_class(ackley$trace(), "data.frame")
# })
#
# test_that("Best value and parameter can be extracted", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())$
#     set_optimizer(optimizer_optim())
#   expect_warning(
#     expect_null(ackley$best("value")),
#     "No optimization results saved yet."
#   )
#   expect_warning(
#     expect_null(ackley$best("parameter")),
#     "No optimization results saved yet."
#   )
#   ackley$
#     initialize_random(runs = 10)$
#     optimize()
#   expect_length(ackley$best("value"), 1)
#   expect_length(ackley$best("parameter"), 2)
# })
#
# test_that("Function name can be extracted and set", {
#   hmm <- Nop$new(f = f_ll_hmm, npar = 6)
#   expect_equal(hmm$f_name, "f_ll_hmm")
#   hmm$f_name <- "name"
#   expect_equal(hmm$f_name, "name")
#   expect_error(
#     {
#       hmm$f_name <- 1
#     },
#     "must be a single"
#   )
# })
#
# test_that("Length of target argument can be extracted", {
#   hmm <- Nop$new(f = f_ll_hmm, npar = 6)
#   expect_equal(hmm$npar, 6)
#   expect_error(
#     {
#       hmm$npar <- 5
#     },
#     "read only"
#   )
# })
#
# test_that("True value and parameter can be extracted and modified", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)
#   expect_warning(
#     ackley$true(),
#     "The true minimum parameter vector has not been specified yet."
#   )
# })
#
# test_that("New optimization label can be generated", {
#   ackley <- Nop$new(f = f_ackley, npar = 2)$
#     set_optimizer(optimizer_nlm())
#   label <- ackley$fresh_label
#   expect_true(is_name(label))
#   ackley$
#     initialize_random()$
#     optimize()
#   label_new <- ackley$fresh_label
#   expect_false(identical(label, label_new))
#   expect_error(
#     {
#       ackley$fresh_label <- "label"
#     },
#     "read only"
#   )
# })
