test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_snapshot(ackley$print())
  expect_snapshot(print(ackley))
  expect_error(
    Nop$new(),
    "specify argument"
  )
  expect_error(
    Nop$new(f = 1),
    "must be"
  )
  expect_error(
    Nop$new(f = f_ackley),
    "specify argument"
  )
  expect_error(
    Nop$new(f = f_ackley, npar = 0),
    "must be a single, positive"
  )
  expect_identical(ackley$npar, 2L)
  expect_error(
    {
      ackley$npar <- 1
    },
    "is read only"
  )
  expect_error(
    Nop$new(f = function() 1, npar = 2),
    "must have at least one argument"
  )
})

test_that("Additional function arguments can be set", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data, N = 2, neg = TRUE)
  expect_s3_class(hmm, c("Nop", "R6"), exact = TRUE)
  expect_error(
    hmm$argument("set", data),
    "All arguments to be set must be named."
  )
  expect_error(
    hmm$argument("set", data = data),
    "already exists"
  )
  expect_snapshot(print(hmm))
})

test_that("Additional function arguments can be extracted", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data, test_arg = 6)
  expect_error(
    hmm$argument("get"),
    "Please specify"
  )
  expect_equal(hmm$argument("get", name = "test_arg"), 6)
  expect_error(
    hmm$argument("get", name = "does_not_exist"),
    "is not yet specified"
  )
  expect_error(
    hmm$argument("get", name = 1),
    "must be a single"
  )
})

test_that("Additional function arguments can be removed", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data)
  expect_error(
    hmm$argument("remove", name = "arg_does_not_exist"),
    "is not yet specified"
  )
  expect_s3_class(hmm$argument("remove", name = "data"), "Nop")
  expect_error(
    hmm$argument("remove"),
    "Please specify"
  )
  expect_error(
    hmm$argument("remove", name = 1:2),
    "must be a"
  )
})

test_that("Additional function arguments can be modified and reset", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data)
  hmm$argument("standardize", name = "data")
  hmm$argument("subset", name = "data")
  hmm$argument("reset", name = "data")
  expect_identical(
    hmm$argument("get", name = "data"),
    data
  )
  data_tmp <- 1:10
  hmm$argument("modify", data = data_tmp)
  expect_identical(
    hmm$argument("get", name = "data"),
    data_tmp
  )
})

test_that("Optimizer can be set", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$set_optimizer(),
    "Please specify argument"
  )
  expect_error(
    ackley$set_optimizer(
      "not_an_optimizer_object"
    ),
    "must be an"
  )
  expect_error(
    ackley$set_optimizer(optimizer_nlm(), optimizer_label = 1),
    "must be a"
  )
  ackley$set_optimizer(optimizer_nlm(), optimizer_label = "nlm")
  expect_snapshot(ackley)
  expect_error(
    ackley$set_optimizer(optimizer_nlm(), optimizer_label = "nlm"),
    "already exists, use another one"
  )
  ackley$set_optimizer(optimizer_optim())
  expect_snapshot(ackley)
})

test_that("Ackley function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$evaluate(1),
    "must be of length 2"
  )
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
})

test_that("Warnings in function evaluation can be hidden", {
  warning_f <- function(x) {
    warning("huhu")
    x
  }
  warning_nop <- Nop$new(f = warning_f, npar = 1)
  expect_warning(
    warning_nop$evaluate(at = 1),
    "huhu"
  )
  expect_warning(
    warning_nop$evaluate(at = 1, hide_warnings = TRUE),
    regexp = NA
  )
})

test_that("Errors in function evaluation can be returned", {
  error_f <- function(x) {
    stop("shit")
    x
  }
  error_nop <- Nop$new(f = error_f, npar = 1)
  expect_equal(
    error_nop$evaluate(at = 1),
    "shit"
  )
})

test_that("HMM likelihood function can be evaluated", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, "data" = data)
  at <- rnorm(6)
  expect_error(
    hmm$evaluate(),
    "is not yet specified"
  )
  hmm$argument("set", "N" = 2, "neg" = TRUE)
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = data, N = 2, neg = TRUE)
  )
  hmm$argument("remove", name = "neg")
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = data, N = 2)
  )
})

test_that("Long function evaluation can be interrupted", {
  skip_if_not(.Platform$OS.type == "windows")
  expect_warning(
    long_f <- Nop$new(f = function(x) {
      for (i in 1:7) Sys.sleep(0.1)
      x
    }, npar = 1),
    "is unnamed"
  )
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 0.5),
    "time limit reached"
  )
  expect_equal(
    long_f$evaluate(at = 1, time_limit = 1),
    1
  )
})

test_that("Input checks for optimization method work", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(),
    "No initial values set"
  )
  ackley$initialize_random()
  expect_error(
    ackley$optimize(save_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(ncores = 1.4),
    "must be a single, positive"
  )
  expect_error(
    ackley$optimize(verbose = "yes"),
    "must be"
  )
  expect_error(
    ackley$optimize(hide_warnings = "bad"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = TRUE, simplify = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(optimization_label = 1),
    "must be"
  )
})

test_that("Optimization via random or fixed initialization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    initialize_random(runs = 5)$optimize()$
    initialize_random(sampler = function() runif(2), seed = 1)$optimize()$
    initialize_fixed(runif(2))$optimize()$
    initialize_fixed(list(1:2, 2:3, 3:4))$optimize()
  expect_snapshot(ackley)
  out <- ackley$
    initialize_random(runs = 5)$
    optimize(return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  expect_length(out, 5)
  expect_true(all(sapply(out, length) == 2))
  out <- ackley$initialize_random(runs = 3)$
    optimize(which_optimizer = 1, return_results = TRUE, save_results = FALSE
  )
  expect_type(out, "list")
  expect_length(out, 3)
  expect_true(all(sapply(out, length) == 1))
  skip_on_cran()
  ackley$initialize_random(runs = 50)$optimize(ncores = 2)
})

test_that("Nop object can be validated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$validate(at = 1),
    "must be of length 2"
  )
  expect_warning(
    ackley$validate(),
    "No optimizer specified, testing optimizers is skipped."
  )
  ackley$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_true(ackley$validate(verbose = FALSE))
})

test_that("Bad functions and optimizers can be detected in validation", {
  error_f <- Nop$new(f = function(x) stop("error message"), 1)
  expect_error(
    error_f$validate(),
    "Function call threw an error"
  )
  lengthy_f <- Nop$new(f = function(x) 1:2, 1)
  expect_error(
    lengthy_f$validate(),
    "Test function call is of length 2."
  )
  character_f <- Nop$new(f = function(x) "not_a_numeric", 1)
  expect_error(
    character_f$validate(),
    "Function call threw an error"
  )
  list_f <- Nop$new(f = function(x) list(), 1)
  expect_error(
    list_f$validate(),
    "Test function call did not return a"
  )
  error_optimizer_fun <- function(f, p) {
    if (identical(p, 1:2)) stop("error message")
    list(v = f(p), z = 1:2)
  }
  error_optimizer <- optimizeR::define_optimizer(
    error_optimizer_fun,
    objective = "f", initial = "p", value = "v",
    parameter = "z"
  )
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(error_optimizer)
  expect_error(
    ackley$validate(at = 1:2),
    "Optimization threw an error"
  )
})

test_that("Validations can be interrupted", {
  skip_if_not(.Platform$OS.type == "windows")
  slow_f <- function(x) {
    Sys.sleep(2)
    1
  }
  slow_f <- Nop$new(slow_f, 1)$
    set_optimizer(optimizer_nlm())
  expect_warning(
    expect_warning(
      slow_f$validate(time_limit = 1),
      "Time limit of 1s reached in the function call"
    ),
    "Time limit of 1s reached in the optimization"
  )
  slow_optimizer_fun <- function(f, p) {
    Sys.sleep(2)
    stats::nlm(f = f, p = p)
  }
  slow_optimizer <- optimizeR::define_optimizer(
    slow_optimizer_fun,
    objective = "f", initial = "p", value = "minimum",
    parameter = "estimate"
  )
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(slow_optimizer)
  expect_warning(
    ackley$validate(at = 1:2, time_limit = 1),
    "Time limit of 1s reached in the optimization"
  )
})

test_that("Overview of available elements can be created", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())
  expect_warning(
    ackley$elements(),
    "No optimization results saved yet"
  )
  ackley$
    initialize_random(runs = 10)$
    optimize()
  expect_equal(
    ackley$elements(),
    list("stats::nlm" = c(
      "value", "parameter", "seconds", "initial", "gradient", "code",
      "iterations", "error", "error_message"
    ))
  )
})

test_that("Results can be accessed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    initialize_random(runs = 10)$
    optimize(save_results = TRUE, return_results = FALSE)
  results <- ackley$results()
  expect_type(results, "list")
  expect_length(results, 20)
  results <- ackley$results(
    which_run = 1:10, which_optimizer = 2, which_element = "value"
  )
  expect_type(results, "list")
  expect_length(results, 5)
})

test_that("Results can be summarized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$summary(),
    "No optimization results saved yet."
  )
  ackley$
    initialize_random(runs = 10)$
    optimize()
  out <- ackley$summary(
    which_element = c("value", "iterations"), digits = 1
  )
})

test_that("Results can be deleted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())
  expect_warning(
    ackley$delete(which_run = 1),
    "No optimization results saved yet"
  )
  expect_equal(
    suppressWarnings(ackley$number()), 0
  )
  ackley$
    initialize_random()$
    optimize()
  expect_equal(
    suppressWarnings(ackley$number()), 1
  )
  ackley$delete(which_run = 1, prompt = FALSE)
  expect_equal(
    suppressWarnings(ackley$number()), 0
  )
})

test_that("Overview of optima works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$optima(),
    "No optimization results saved yet."
  )
  ackley$
    initialize_random(runs = 10)$
    optimize()
  expect_true(
    is.data.frame(ackley$optima())
  )
  expect_error(
    ackley$optima(sort_by = "bad_input"),
    "can be one of"
  )
})

test_that("Optimization times and values can be plotted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    initialize_random(runs = 100)$
    optimize(reset_initial = FALSE)$
    optimize()
  combinations <- expand.grid(
    which_element = c("seconds", "value"),
    group_by = list("optimization_label", "optimizer_label", NULL),
    relative = c(TRUE, FALSE),
    which_run = "all",
    which_optimizer = "all",
    only_comparable = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    which_element <- combinations[i, "which_element"]
    group_by <- combinations[[i, "group_by"]]
    relative <- combinations[i, "relative"]
    which_run <- combinations[i, "which_run"]
    which_optimizer <- combinations[i, "which_optimizer"]
    only_comparable <- combinations[i, "only_comparable"]
    expect_s3_class(
      ackley$plot(
        which_element = which_element, group_by = group_by, relative = relative,
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      ),
      "ggplot"
    )
  }
})

# TODO

self <- ackley
private <- self$.__enclos_env__$private



test_that("Optimization trace can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley$trace(), "data.frame")
})

test_that("Best value and parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    expect_null(ackley$best_value()),
    "No optimization results saved yet."
  )
  ackley$optimize(runs = 10)
  expect_length(
    ackley$best_value(), 1
  )
  expect_warning(
    expect_null(ackley$best_parameter()),
    "No optimization results saved yet."
  )
  ackley$optimize(runs = 10)
  expect_length(
    ackley$best_parameter(), 2
  )
})

test_that("Existence of additional argument can be checked", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  private <- hmm$.__enclos_env__$private
  expect_error(
    private$.check_additional_argument_exists("data"),
    "is not yet specified"
  )
  hmm$set_argument("data" = data)
})

test_that("f can be extracted", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  expect_equal(hmm$f, f_ll_hmm)
  expect_error(
    {
      hmm$f <- "function"
    },
    "read only"
  )
})

test_that("f_name can be extracted and set", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  expect_equal(hmm$f_name, "f_ll_hmm")
  hmm$f_name <- "name"
  expect_equal(hmm$f_name, "name")
  expect_error(
    {
      hmm$f_name <- 1
    },
    "must be a single"
  )
})

test_that("f_target can be extracted", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  expect_equal(hmm$f_target, "theta")
  expect_error(
    {
      hmm$f_target <- "par"
    },
    "read only"
  )
})

test_that("npar can be extracted", {
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  expect_equal(hmm$npar, 6)
  expect_error(
    {
      hmm$npar <- 5
    },
    "read only"
  )
})

test_that("arguments can be extracted", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6)
  expect_warning(
    hmm$arguments,
    "No function arguments have been specified yet"
  )
  hmm$set_argument("data" = data)
  expect_equal(
    hmm$arguments,
    list(data = data)
  )
  expect_error(
    {
      hmm$arguments <- "argument"
    },
    "read only"
  )
})

test_that("true value can be extracted and modified", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_value)
  expect_error(
    {
      ackley$true_value <- 1:2
    },
    "must be a single"
  )
  ackley$true_value <- 0
  expect_equal(ackley$true_value, 0)
  ackley$true_value <- NULL
  expect_null(ackley$true_value)
})

test_that("true parameter can be extracted and modified", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_null(ackley$true_parameter)
  expect_error(
    {
      ackley$true_parameter <- 1:4
    },
    "must be of length 2"
  )
  ackley$true_parameter <- c(0, 0)
  expect_equal(ackley$true_value, 0)
  expect_equal(ackley$true_value, 0)
  expect_error(
    {
      ackley$true_value <- 2
    },
    "Please update"
  )
  ackley$true_parameter <- NULL
  expect_null(ackley$true_parameter)
})

test_that("new label can be generated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())
  label <- ackley$new_label
  expect_true(is_name(label))
  ackley$optimize()
  label_new <- ackley$new_label
  expect_false(identical(label, label_new))
  expect_error(
    {
      ackley$new_label <- "label"
    },
    "read only"
  )
})
