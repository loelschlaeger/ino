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

test_that("input checks for result transformation work", {
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
      argument = diag(3), by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = pi
    ),
    "must be an index vector"
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
  combinations <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE))
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, 1]
    center <- combinations[i, 2]
    scale <- combinations[i, 3]
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = integer()
      ),
      as.numeric(
        scale(argument, center = center, scale = scale)
      )
    )
    ignore <- 1:5
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = ignore
      ),
      c(argument[ignore], as.numeric(
        scale(argument[-ignore], center = center, scale = scale)
      ))
    )
  }
})

test_that("standardization of data.frame works", {
  argument <- data.frame("a" = rnorm(10), "b" = rnorm(10))
  combinations <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE))
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, 1]
    center <- combinations[i, 2]
    scale <- combinations[i, 3]
    if (by_column) {
      expected <- as.data.frame(
        scale(argument, center = center, scale = scale)
      )
    } else {
      expected <- as.data.frame(
        t(scale(t(argument), center = center, scale = scale))
      )
    }
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = integer()
      ),
      expected
    )
    if (by_column) {
      ignore <- 1
      expected <- as.data.frame(cbind(
        argument[, ignore, drop = FALSE],
        scale(argument[, -ignore, drop = FALSE], center = center, scale = scale)
      ))
    } else {
      ignore <- 1:5
      expected <- as.data.frame(rbind(
        argument[ignore, , drop = FALSE],
        t(scale(t(argument[-ignore, , drop = FALSE]),
          center = center,
          scale = scale
        ))
      ))
    }
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = ignore
      ),
      expected,
      ignore_attr = TRUE
    )
  }
})

test_that("standardization of matrix works", {
  argument <- matrix(rnorm(9), 3, 3)
  combinations <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE))
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, 1]
    center <- combinations[i, 2]
    scale <- combinations[i, 3]
    if (by_column) {
      expected <- scale(argument, center = center, scale = scale)
    } else {
      expected <- t(scale(t(argument), center = center, scale = scale))
    }
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = integer()
      ),
      expected,
      ignore_attr = TRUE
    )
    if (by_column) {
      ignore <- 1
      expected <- cbind(
        argument[, ignore, drop = FALSE],
        scale(argument[, -ignore, drop = FALSE], center = center, scale = scale)
      )
    } else {
      ignore <- 1:2
      expected <- rbind(
        argument[ignore, , drop = FALSE],
        t(scale(t(argument[-ignore, , drop = FALSE]),
          center = center,
          scale = scale
        ))
      )
    }
    expect_equal(
      standardize_argument(
        argument = argument, by_column = by_column, center = center,
        scale = scale, ignore = ignore
      ),
      expected,
      ignore_attr = TRUE
    )
  }
})

test_that("input checks for subsetting work", {

})

test_that("subsetting of vector works", {
  argument <- rnorm(10)
  combinations <- expand.grid(
    how = c("random", "first", "last"),
    proportion = round(runif(2), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion, seed = NULL
    )
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
  argument <- rep(1:2, each = 5)
  combinations <- expand.grid(
    how = c("similar", "dissimilar"),
    proportion = round(runif(2), 2),
    centers = 1:2,
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion,
      centers = centers, ignore = 2, seed = NULL
    )
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
})

test_that("subsetting of data.frame works", {
  argument <- data.frame("a" = rnorm(10), "b" = rnorm(10), "c" = LETTERS[1:10])
  combinations <- expand.grid(
    by_row = c(TRUE, FALSE),
    how = c("random", "first", "last"),
    proportion = round(runif(2), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- subset_argument(
      argument = argument, by_row = by_row, how = how, proportion = proportion,
      seed = NULL
    )
    if (by_row) {
      expected_nrow <- ceiling(nrow(argument) * proportion)
      expect_equal(nrow(out), expected_nrow)
    } else {

    }
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion, seed = NULL
    )
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
  argument <- rep(1:2, each = 5)
  combinations <- expand.grid(
    how = c("similar", "dissimilar"),
    proportion = round(runif(2), 2),
    centers = 1:2,
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    expected_length <- ceiling(length(argument) * proportion)
    out <- subset_argument(
      argument = argument, how = how, proportion = proportion,
      centers = centers, ignore = 2, seed = NULL
    )
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
})


