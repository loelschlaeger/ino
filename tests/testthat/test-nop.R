test_that("Nop object can be initialized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley, c("Nop", "R6"), exact = TRUE)
  expect_error(
    Nop$new(),
    "specify argument"
  )
  expect_error(
    Nop$new(f = 1),
    "is not a"
  )
  expect_error(
    Nop$new(f = f_ackley),
    "specify argument"
  )
  expect_error(
    Nop$new(f = f_ackley, npar = 0),
    "must be a single, positive"
  )
  expect_identical(ackley$f, f_ackley)
  expect_identical(ackley$npar, 2L)
  expect_error(
    {
      ackley$f <- function(x) x
    },
    "is read only"
  )
  expect_error(
    {
      ackley$npar <- 1
    },
    "is read only"
  )
  expect_error(
    Nop$new(f = function() 1, npar = 0),
    "should have at least one argument"
  )
})

test_that("Nop object can be printed", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_snapshot(print(ackley))
  expect_snapshot(ackley$print())
})

test_that("Parameters for Nop object can be set", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data)
  expect_s3_class(hmm, c("Nop", "R6"), exact = TRUE)
  expect_error(
    hmm$set_argument(data),
    "Please name argument 1."
  )
  expect_error(
    hmm$set_argument("data" = data),
    "already exists"
  )
  expect_snapshot(print(hmm))
})

test_that("Parameters for Nop object can be get", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data, test_arg = 6)
  expect_error(
    hmm$get_argument(),
    "Please specify"
  )
  expect_equal(hmm$get_argument("test_arg"), 6)
  expect_error(
    hmm$get_argument("does_not_exist"),
    "is not yet specified"
  )
  expect_error(
    hmm$get_argument(1),
    "must be a single"
  )
})

test_that("Parameters for Nop object can be removed", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(f = f_ll_hmm, npar = 6, data = data)
  expect_error(
    hmm$remove_argument("arg_does_not_exist"),
    "is not yet specified"
  )
  expect_s3_class(hmm$remove_argument("data"), "Nop")
  expect_error(
    hmm$remove_argument(),
    "Please specify"
  )
  expect_error(
    hmm$remove_argument(argument_name = 1:2),
    "must be a"
  )
})

test_that("optimizer can be set", {
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
    ackley$set_optimizer(optimizer_nlm(), label = 1),
    "must be a"
  )
  ackley$set_optimizer(optimizer_nlm(), label = "nlm")
  expect_snapshot(ackley)
  expect_error(
    ackley$set_optimizer(optimizer_nlm(), label = "nlm"),
    "already exists, please choose another one"
  )
  ackley$set_optimizer(optimizer_optim())
  expect_snapshot(ackley)
})

test_that("ackley function can be evaluated", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$evaluate(1),
    "must be of length 2"
  )
  expect_equal(ackley$evaluate(c(0, 1)), f_ackley(c(0, 1)))
})

test_that("long function evaluations can be interrupted", {
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

test_that("warnings in function evaluation can be hidden", {
  expect_warning(
    warning_f <- Nop$new(f = function(x) {
      warning("huhu")
      x
    }, npar = 1),
    "is unnamed"
  )
  expect_warning(
    warning_f$evaluate(at = 1),
    "huhu"
  )
  expect_warning(
    warning_f$evaluate(at = 1, hide_warnings = TRUE),
    regexp = NA
  )
})

test_that("errors in function evaluation can be returned", {
  expect_warning(
    error_f <- Nop$new(f = function(x) {
      stop("shit")
      x
    }, npar = 1),
    "is unnamed"
  )
  expect_equal(
    error_f$evaluate(at = 1),
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
  hmm$set_argument("N" = 2, "neg" = TRUE)
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = data, N = 2, neg = TRUE)
  )
  hmm$remove_argument("neg")
  expect_equal(
    hmm$evaluate(at = at),
    f_ll_hmm(theta = at, data = data, N = 2)
  )
})

test_that("ackley function can be optimized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(runs = -1),
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
  ackley$optimize(runs = 5)
  ackley$optimize(runs = 1, initial = runif(2))
  ackley$optimize(runs = 3, initial = function() runif(2), seed = 1)
  ackley$optimize(initial = c(0, 0))
  ackley$optimize(initial = list(1:2, 2:3, 3:4))
  expect_snapshot(ackley)
  expect_error(
    ackley$optimize(save_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = "TRUE"),
    "must be"
  )
  expect_error(
    ackley$optimize(return_results = TRUE, simplify = "TRUE"),
    "must be"
  )
  out <- ackley$optimize(runs = 5, return_results = TRUE, save_results = FALSE)
  expect_type(out, "list")
  expect_length(out, 5)
  expect_true(all(sapply(out, length) == 2))
  out <- ackley$optimize(
    runs = 1, which_optimizer = 1, return_results = TRUE, save_results = FALSE
  )
  expect_type(out, "list")
  out <- ackley$optimize(
    runs = 1, which_optimizer = 1, return_results = TRUE, save_results = FALSE,
    simplify = FALSE
  )
  expect_type(out, "list")
})

test_that("parallel optimization works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$optimize(ncores = 1.4),
    "must be a single, positive"
  )
  skip_on_cran()
  ackley$optimize(
    runs = 40, ncores = 2, save_results = FALSE
  )
})

test_that("Nop object can be tested", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_error(
    ackley$test(at = 1),
    "must be of length 2"
  )
  expect_warning(
    ackley$test(),
    "No optimizer specified, testing optimizer is skipped."
  )
  ackley$set_optimizer(optimizer_nlm())
  expect_true(ackley$test(verbose = FALSE))
})

test_that("standardization works", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = data, "N" = 2, "neg" = TRUE
  )
  expect_error(
    hmm$standardize(),
    "Please specify"
  )
  expect_error(
    hmm$standardize(1),
    "must be a single"
  )
  expect_s3_class(hmm$standardize("data"), c("Nop", "R6"), exact = TRUE)
})

test_that("reduction works", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = data, "N" = 2, "neg" = TRUE
  )
  expect_error(
    hmm$reduce(),
    "Please specify"
  )
  expect_error(
    hmm$reduce(1),
    "must be a single"
  )
  expect_s3_class(hmm$reduce("data"), c("Nop", "R6"), exact = TRUE)
})

test_that("argument can be reset", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = data, "N" = 2, "neg" = TRUE
  )
  hmm$standardize("data")
  expect_error(
    hmm$reset_argument(),
    "Please specify"
  )
  hmm$reset_argument("data")
  expect_equal(data, hmm$get_argument("data"))
  hmm$reduce("data")
  hmm$reset_argument("data")
  expect_equal(data, hmm$get_argument("data"))
})

test_that("continue optimization works", {
  tpm <- matrix(c(0.8, 0.1, 0.2, 0.9), nrow = 2)
  mu <- c(-2, 2)
  sigma <- c(0.5, 1)
  theta <- c(log(tpm[row(tpm) != col(tpm)]), mu, log(sigma))
  data <- sim_hmm(Tp = 100, N = 2, theta = theta)
  hmm <- Nop$new(
    f = f_ll_hmm, npar = 6, "data" = data, "N" = 2, "neg" = TRUE
  )$set_optimizer(optimizer_nlm())$
    standardize("data")$
    optimize(runs = 2)$
    reset_argument("data")$
    continue()
  expect_s3_class(hmm, "Nop")
})

test_that("results can be accessed", {
  runs <- 10
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = runs, save_results = TRUE, return_results = FALSE)
  results <- ackley$results()
  expect_type(results, "list")
  expect_length(results, runs)
})

test_that("number of results can be accessed", {
  runs <- 10
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = runs, save_results = TRUE, return_results = FALSE)
  expect_equal(ackley$number_runs(), runs)
})

test_that("overview of available elements can be created", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())
  expect_warning(
    ackley$elements_available(),
    "No optimization results saved yet"
  )
  ackley$optimize(runs = 10)
  expect_equal(
    ackley$elements_available(),
    list("stats::nlm" = c(
      "value", "parameter", "seconds", "initial", "gradient", "code",
      "iterations", "label", "run", "optimizer", "comparable"
    ))
  )
})

test_that("results can be cleared", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())
  expect_warning(
    ackley$clear(which_run = 1),
    "No optimization results saved yet"
  )
  ackley$optimize(runs = 10)
  ackley$clear(which_run = 1)
})

test_that("results can be summarized", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$summary(),
    "No optimization results saved yet."
  )
})

test_that("overview of optima works", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    ackley$optima(),
    "No optimization results saved yet."
  )
  ackley$optimize(runs = 10)
  expect_true(is.data.frame(ackley$optima()))
  expect_error(
    ackley$optima(sort_by = "bad_input"),
    "must be"
  )
})

test_that("optimization times and values can be plotted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 100, label = "1")$
    optimize(runs = 100, label = "2")
  combinations <- expand.grid(
    which_element = c("seconds", "value"),
    by = list("label", "optimizer", NULL),
    relative = c(TRUE, FALSE),
    which_run = "all",
    which_optimizer = "all",
    only_comparable = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    which_element <- combinations[i, "which_element"]
    by <- combinations[[i, "by"]]
    relative <- combinations[i, "relative"]
    which_run <- combinations[i, "which_run"]
    which_optimizer <- combinations[i, "which_optimizer"]
    only_comparable <- combinations[i, "only_comparable"]
    expect_s3_class(
      ackley$plot(
        which_element = which_element, by = by, relative = relative,
        which_run = which_run, which_optimizer = which_optimizer,
        only_comparable = only_comparable
      ),
      "ggplot"
    )
  }
})

test_that("optimization trace can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_s3_class(ackley$trace(), "data.frame")
})

test_that("best value can be extracted", {
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
})

test_that("best parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_warning(
    expect_null(ackley$best_parameter()),
    "No optimization results saved yet."
  )
  ackley$optimize(runs = 10)
  expect_length(
    ackley$best_parameter(), 2
  )
})

test_that("closest parameter can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())$
    optimize(runs = 10)
  expect_length(
    ackley$closest_parameter(0), 2
  )
})

test_that("existence of additional argument can be checked", {
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

test_that("run ids can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)$
    set_optimizer(optimizer_nlm(), "nlm")$
    optimize(runs = 10, label = "label")
  private <- ackley$.__enclos_env__$private
  expect_equal(private$.get_run_ids(which_run = "label"), 1:10)
  expect_warning(
    private$.get_run_ids(which_run = "label_does_not_exist"),
    "Please check argument"
  )
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

test_that("show minimum can be extracted and modified", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_true(ackley$minimized)
  ackley$minimized <- FALSE
  expect_false(ackley$minimized)
  expect_error(
    {
      ackley$minimized <- "TRUE"
    },
    "must be"
  )
})

test_that("optimizer can be extracted", {
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_warning(
    ackley$optimizer,
    "No optimizer specified"
  )
  ackley$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_type(ackley$optimizer, "list")
  expect_length(ackley$optimizer, 2)
  expect_error(
    {
      ackley$optimizer <- "optimizer"
    },
    "must be"
  )
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
