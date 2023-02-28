test_that("initial parameter as random", {
  expect_length(build_initial("random", 2)(1), 2)
})

test_that("initial parameter as numeric", {
  expect_equal(build_initial(1:3, 3)(1), 1:3)
  expect_error(
    build_initial(1:4, 3),
    "It should be of length 3."
  )
})

test_that("initial parameter as list", {
  expect_equal(build_initial(list(1:3, 2:4), 3)(2), 2:4)
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
  expect_equal(build_initial(function(run) rep(run, 4), 4)(3), rep(3, 4))
  expect_equal(build_initial(function() rep(3, 4), 4)(3), rep(3, 4))
  expect_error(
    build_initial(function(run, bad_arg) rep(run, 4), 4),
    "It can have 0 or 1 arguments, but not 2."
  )
  expect_error(
    build_initial(function(run) 1:3, 4),
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
      list(list(value = 11), list(value = 12)), list(list(value = 21),
      list(value = 22))
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
