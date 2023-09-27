options("ino_verbose" = FALSE)

test_that("initial random parameter", {
  expect_length(build_initial("random", 2)(1, 1), 2)
})

test_that("initial parameter as numeric", {
  expect_equal(build_initial(1:3, 3)(1, 2), 1:3)
  expect_error(
    build_initial(1:4, 3),
    "It should be of length 3."
  )
})

test_that("initial parameter as list", {
  expect_equal(build_initial(list(1:3, 2:4), 3)(2, 1), 2:4)
  expect_error(
    build_initial(list(1:3, 2:4), 2),
    "Each of them should be of length 2."
  )
  expect_error(
    build_initial(list(LETTERS[1:2], LETTERS[3:4]), 2),
    "should only contain"
  )
})

test_that("initial parameter as function", {
  expect_equal(build_initial(function(a, b) c(a, b), 2)(1, 2), 1:2)
  expect_equal(build_initial(function() rep(3, 4), 4)(3, 4), rep(3, 4))
  expect_error(
    build_initial(function(run) rep(run, 4), 4),
    "It can have 0 or 2 arguments, but not 1."
  )
  expect_error(
    build_initial(function() 1:3, 4),
    "It should return initial values of length 4."
  )
})

test_that("initial parameter as anything else", {
  expect_error(
    build_initial(diag(2), 4),
    "Please see the documentation for possible inputs."
  )
})

test_that("input checks for result filtering work", {
  expect_error(
    filter_results(
      results = "not_a_list",
      run_ids = 1,
      optimizer_ids = 1,
      which_element = "character",
      only_comparable = FALSE
    )
  )
  expect_error(
    filter_results(
      results = list(),
      run_ids = "not_a_number",
      optimizer_ids = 1,
      which_element = "character",
      only_comparable = FALSE
    )
  )
  expect_error(
    filter_results(
      results = list(),
      run_ids = 1,
      optimizer_ids = "not_a_number",
      which_element = "character",
      only_comparable = FALSE
    )
  )
  expect_error(
    filter_results(
      results = list(),
      run_ids = 1,
      optimizer_ids = 1,
      which_element = "",
      only_comparable = FALSE
    )
  )
  expect_error(
    filter_results(
      results = list(),
      run_ids = 1,
      optimizer_ids = 1,
      which_element = "character",
      only_comparable = "not_a_boolean"
    ),
    "must be"
  )
  expect_error(
    simplify_results(
      results = list(),
      simplify = "not_a_boolean"
    ),
    "must be"
  )
})

test_that("results can be filtered by run", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11
      ),
      list( # optimizer 2
        "value" = 12
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21
      ),
      list( # optimizer 2
        "value" = 22
      )
    )
  )
  out <- filter_results(
    results = results,
    run_ids = 2,
    optimizer_ids = 1:2,
    which_element = "value",
    only_comparable = FALSE
  )
  expect_identical(
    out, list(list(list(value = 21), list(value = 22)))
  )
})

test_that("results can be filtered by optimizer", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11
      ),
      list( # optimizer 2
        "value" = 12
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21
      ),
      list( # optimizer 2
        "value" = 22
      )
    )
  )
  out <- filter_results(
    results = results,
    run_ids = 1:2,
    optimizer_ids = 2,
    which_element = "value",
    only_comparable = FALSE
  )
  expect_identical(
    out, list(list(list(value = 12)), list(list(value = 22)))
  )
})

test_that("results can be filtered by element", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11, "comparable" = TRUE
      ),
      list( # optimizer 2
        "value" = 12, "comparable" = FALSE
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21, "comparable" = TRUE
      ),
      list( # optimizer 2
        "value" = 22, "comparable" = FALSE
      )
    )
  )
  out <- filter_results(
    results = results,
    run_ids = 1:2,
    optimizer_ids = 1:2,
    which_element = "value",
    only_comparable = FALSE
  )
  expect_identical(
    out, list(
      list(list(value = 11), list(value = 12)), list(
        list(value = 21),
        list(value = 22)
      )
    )
  )
})

test_that("results can be filtered by comparable", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11, "comparable" = TRUE
      ),
      list( # optimizer 2
        "value" = 12, "comparable" = FALSE
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21, "comparable" = TRUE
      ),
      list( # optimizer 2
        "value" = 22, "comparable" = FALSE
      )
    )
  )
  out <- filter_results(
    results = results,
    run_ids = 1:2,
    optimizer_ids = 1:2,
    which_element = c("value", "comparable"),
    only_comparable = TRUE
  )
  expect_identical(
    out, list(
      list(list(value = 11, comparable = TRUE)),
      list(list(value = 21, comparable = TRUE))
    )
  )
})

test_that("results with one run can be simplified", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11, "message" = "a"
      ),
      list( # optimizer 2
        "value" = 12, "message" = "b"
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, list(list(value = 11, message = "a"), list(value = 12, message = "b"))
  )
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11, "message" = "a"
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, list(value = 11, message = "a")
  )
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, 11
  )
})

test_that("results with one optimizer can be simplified", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11, "message" = "a"
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21, "message" = "b"
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, list(list(value = 11, message = "a"), list(value = 21, message = "b"))
  )
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, list(c(value = 11), c(value = 21))
  )
})

test_that("results with one element can be simplified", {
  results <- list(
    list( # run 1
      list( # optimizer 1
        "value" = 11
      ),
      list( # optimizer 2
        "value" = 12
      )
    ),
    list( # run 2
      list( # optimizer 1
        "value" = 21
      ),
      list( # optimizer 2
        "value" = 22
      )
    )
  )
  out <- simplify_results(
    results = results,
    simplify = TRUE
  )
  expect_identical(
    out, list(
      list(c(value = 11), c(value = 12)),
      list(c(value = 21), c(value = 22))
    )
  )
})

test_that("Nop object can be tested", {
  options("ino_verbose" = FALSE)
  ackley <- Nop$new(f = f_ackley, npar = 2)
  expect_warning(
    ackley$test(),
    "No optimizer specified, testing optimizer is skipped."
  )
  ackley$
    set_optimizer(optimizer_nlm())$
    set_optimizer(optimizer_optim())
  expect_error(
    ackley$test(time_limit = -1),
    "is not a positive"
  )
  expect_error(
    ackley$test(verbose = "FALSE"),
    "must be"
  )
  expect_true(ackley$test())
})

test_that("Bad function specifications can be detected in tests", {
  error_f <- Nop$new(f = function(x) stop("error message"), 1)
  expect_error(
    error_f$test(),
    "Function call threw an error"
  )
  lengthy_f <- Nop$new(f = function(x) 1:2, 1)
  expect_error(
    lengthy_f$test(),
    "Test function call is of length 2."
  )
  character_f <- Nop$new(f = function(x) "not_a_numeric", 1)
  expect_error(
    character_f$test(),
    "Function call threw an error"
  )
  list_f <- Nop$new(f = function(x) list(), 1)
  expect_error(
    list_f$test(),
    "Test function call did not return a"
  )
})

test_that("Bad optimizer specifications can be detected in tests", {
  error_optimizer_fun <- function(f, p) {
    if (identical(p, 1:2)) stop("error message")
    list(v = f(p), z = 1:2)
  }
  error_optimizer <- optimizeR::define_optimizer(
    .optimizer = error_optimizer_fun,
    .objective = "f", .initial = "p", .value = "v",
    .parameter = "z", .direction = "min"
  )
  ackley <- Nop$new(f = f_ackley, npar = 2)$set_optimizer(error_optimizer)
  expect_error(
    ackley$test(at = 1:2),
    "Optimization threw an error"
  )
})

test_that("Nop tests can be interrupted", {
  skip_if_not(.Platform$OS.type == "windows")
  slow_f <- function(x) {
    Sys.sleep(2)
    1
  }
  slow_f <- Nop$new(slow_f, 1)
  expect_warning(
    expect_warning(
      slow_f$test(time_limit = 1),
      "Time limit of 1s was reached"
    ),
    "No optimizer specified, testing optimizer is skipped."
  )
  slow_optimizer_fun <- function(f, p) {
    Sys.sleep(2)
    stats::nlm(f = f, p = p)
  }
  slow_optimizer <- optimizeR::define_optimizer(
    .optimizer = slow_optimizer_fun,
    .objective = "f", .initial = "p", .value = "minimum",
    .parameter = "estimate", .direction = "min"
  )
  ackley <- Nop$new(f = f_ackley, npar = 2)$set_optimizer(slow_optimizer)
  expect_warning(
    ackley$test(at = 1:2, time_limit = 1),
    "Time limit of 1s was reached in the optimization"
  )
})

test_that("input checks for standardization work", {
  expect_error(
    standardize_argument(
      argument = diag(3), by_column = "not_a_boolean",
      center = TRUE, scale = TRUE, ignore = integer()
    ),
    "must be"
  )
  expect_error(
    standardize_argument(
      argument = diag(3), by_column = FALSE,
      center = TRUE, scale = TRUE, ignore = integer()
    ),
    "Currently, only"
  )
  expect_error(
    standardize_argument(
      argument = diag(3), by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = pi
    ),
    "must be an"
  )
  expect_error(
    standardize_argument(
      argument = list(), by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = integer()
    ),
    "Argument is not suited for standardization."
  )
  expect_warning(
    standardize_argument(
      argument = 1, by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = integer()
    ),
    "Standardization produced NAs."
  )
})

test_that("standardization of vector works", {
  argument <- rnorm(10)
  combinations <- expand.grid(
    by_column = TRUE,
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 2:3),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    if (length(ignore) == 0) {
      expected <- as.vector(
        scale(argument, center = center, scale = scale)
      )
    } else {
      expected <- argument
      expected[-ignore] <- as.numeric(
        scale(argument[-ignore], center = center, scale = scale)
      )
    }
    out <- standardize_argument(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore
    )
    expect_equal(out, expected)
  }
})

test_that("standardization of data.frame works", {
  argument <- data.frame("a" = rnorm(10), "b" = rnorm(10))
  combinations <- expand.grid(
    by_column = TRUE,
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 1),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    if (length(ignore) == 0) {
      expected <- as.data.frame(
        scale(argument, center = center, scale = scale)
      )
    } else {
      expected <- argument
      expected[, -ignore] <- as.data.frame(
        scale(argument[, -ignore, drop = FALSE], center = center, scale = scale)
      )
    }
    out <- standardize_argument(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore
    )
    expect_equal(out, expected, ignore_attr = TRUE)
  }
})

test_that("standardization of matrix works", {
  argument <- matrix(rnorm(9), 3, 3)
  combinations <- expand.grid(
    by_column = TRUE,
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 2:3),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    if (length(ignore) == 0) {
      expected <- as.matrix(
        scale(argument, center = center, scale = scale)
      )
    } else {
      expected <- argument
      expected[, -ignore] <- as.matrix(
        scale(argument[, -ignore, drop = FALSE], center = center, scale = scale)
      )
    }
    out <- standardize_argument(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore
    )
    expect_equal(out, expected, ignore_attr = TRUE)
  }
})

test_that("input checks for subsetting work", {
  expect_error(
    subset_argument(
      argument = diag(3), by_row = TRUE,
      how = TRUE, proportion = 0.5, centers = 2, ignore = integer()
    ),
    "must be a single"
  )
  expect_error(
    subset_argument(
      argument = diag(3), by_row = TRUE, how = "bad_specification",
      proportion = 0.5, centers = 2, ignore = integer()
    ),
    "is misspecified"
  )
  expect_error(
    subset_argument(
      argument = diag(3), by_row = "not_a_boolean",
      how = "random", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "must be"
  )
  expect_error(
    subset_argument(
      argument = diag(3), by_row = FALSE,
      how = "random", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "Currently, only"
  )
  expect_error(
    subset_argument(
      argument = diag(3), by_row = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = pi
    ),
    "must be an index"
  )
  expect_error(
    subset_argument(
      argument = list(), by_row = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "Argument is not suited for reduction."
  )
  expect_error(
    subset_argument(
      argument = diag(3), by_row = TRUE,
      how = "similar", proportion = -1, centers = 2, ignore = integer()
    ),
    "between 0 and 1"
  )
})

test_that("subsetting of vector works (without clusters)", {
  argument <- rnorm(10)
  combinations <- expand.grid(
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion, ignore = ignore
    )
    expect_true(is.vector(out))
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
})

test_that("subsetting of vector works (with clusters)", {
  argument <- rep(1:4, each = 5)
  combinations <- expand.grid(
    how = c("similar", "dissimilar"),
    proportion = round(runif(2, min = 0.1), 2),
    centers = 2,
    ignore = list(integer(), 1:2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    ignore <- combinations[[i, "ignore"]]
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expect_true(is.vector(out))
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
})

test_that("subsetting of data.frame works (without clusters)", {
  argument <- data.frame("a" = 1:5, "b" = LETTERS[1:5])
  combinations <- expand.grid(
    by_row = TRUE,
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- subset_argument(
      argument = argument, by_row = by_row, how = how, proportion = proportion
    )
    expected_dim <- c(ceiling(nrow(argument) * proportion), ncol(argument))
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    for (j in seq_len(ncol(argument))) {
      expect_true(all(out[, j] %in% argument[, j]))
    }
  }
})

test_that("subsetting of data.frame works (with clusters)", {
  argument <- data.frame("a" = 1:5, "b" = 1:5)
  combinations <- expand.grid(
    by_row = TRUE,
    how = c("similar", "dissimilar"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    centers = 1:2,
    ignore = list(integer(), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    ignore <- combinations[[i, "ignore"]]
    out <- subset_argument(
      argument = argument, by_row = by_row, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- c(ceiling(nrow(argument) * proportion), ncol(argument))
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    for (j in seq_len(ncol(argument))) {
      expect_true(all(out[, j] %in% argument[, j]))
    }
  }
})

test_that("subsetting of matrix works (without clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    by_row = TRUE,
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- subset_argument(
      argument = argument, by_row = by_row, how = how, proportion = proportion
    )
    expected_dim <- c(ceiling(nrow(argument) * proportion), ncol(argument))
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    for (j in seq_len(ncol(argument))) {
      expect_true(all(out[, j] %in% argument[, j]))
    }
  }
})

test_that("subsetting of matrix works (with clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    by_row = TRUE,
    how = c("similar", "dissimilar"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    centers = 1:2,
    ignore = list(integer(), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    ignore <- combinations[[i, "ignore"]]
    out <- subset_argument(
      argument = argument, by_row = by_row, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- c(ceiling(nrow(argument) * proportion), ncol(argument))
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    for (j in seq_len(ncol(argument))) {
      expect_true(all(out[, j] %in% argument[, j]))
    }
  }
})
