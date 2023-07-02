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

