test_that("initial values can be created", {
  initial_options <- list(
    "random",
    1:2,
    list(1:2, 2:3, 4:5),
    function() runif(2),
    function(a, b) c(a, b)
  )
  for (initial_option in initial_options) {
    initial_values <- initial_values_helper(
      initial = initial_option,
      npar = 2,
      check_initial = TRUE,
      runs = 3,
      optimizer_ids = 2
    )
    expect_type(initial_values, "list")
    expect_length(initial_values, 3)
    expect_true(all(sapply(initial_values, length) == 2))
    expect_true(all(sapply(initial_values, function(x) is.null(x[[1]]))))
  }
  expect_error(
    initial_values_helper(
      initial = list(1, 1:2, "bad"),
      npar = 2,
      check_initial = TRUE,
      runs = 3,
      optimizer_ids = 2
    ),
    "Each of them should be of length 2."
  )
  expect_error(
    initial_values_helper(
      initial = 1:3,
      npar = 2,
      check_initial = TRUE,
      runs = 3,
      optimizer_ids = 2
    ),
    "It should be of length 2."
  )
  expect_error(
    initial_values_helper(
      initial = function(run) rep(run, 4),
      npar = 2,
      check_initial = TRUE,
      runs = 3,
      optimizer_ids = 2
    ),
    "It can have 0 or 2 arguments, but not 1."
  )
  expect_error(
    initial_values_helper(
      initial = diag(2),
      npar = 2,
      check_initial = TRUE,
      runs = 3,
      optimizer_ids = 2
    ),
    "Please see the documentation for possible inputs."
  )
})

test_that("input checks for standardization work", {
  expect_error(
    standardize_helper(
      argument = diag(3), by_column = "not_a_boolean",
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "must be"
  )
  expect_error(
    standardize_helper(
      argument = diag(3), by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = pi, jointly = list()
    ),
    "must be an"
  )
  expect_error(
    standardize_helper(
      argument = list(), by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "Argument cannot be standardized."
  )
  expect_warning(
    standardize_helper(
      argument = 1, by_column = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "Argument has NAs after standardization."
  )
})

test_that("standardization of vector works", {
  argument <- rnorm(10)
  combinations <- expand.grid(
    by_column = TRUE,
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(integer()),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    scale_input <- if (!center & scale) {
      sd(argument, na.rm = TRUE)
    } else {
      scale
    }
    scale_out <- scale(argument, center = center, scale = scale_input)
    expected <- as.vector(scale_out)
    if (center) {
      attr(expected, "standardized:center") <- attr(scale_out, "scaled:center")
    }
    if (scale) {
      attr(expected, "standardized:scale") <- attr(scale_out, "scaled:scale")
    }
    out <- standardize_helper(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore
    )
    expect_equal(out, expected)
  }
})

test_that("standardization of data.frame works", {
  argument <- data.frame("a" = rnorm(10), "b" = rnorm(10), "c" = rnorm(10))
  combinations <- expand.grid(
    by_column = c(TRUE, FALSE),
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 1),
    jointly = list(list(), list(2:3)),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    jointly <- combinations[[i, "jointly"]]
    if (length(jointly) > 0) {
      ### no unit tests implemented yet
      next
    }
    if (length(ignore) == 0) {
      scale_input <- if (!center & scale) {
        apply(argument, ifelse(by_column, 2, 1), sd, na.rm = TRUE)
      } else {
        scale
      }
      if (by_column) {
        scale_out <- scale(argument, center = center, scale = scale_input)
      } else {
        scale_out <- scale(t(argument), center = center, scale = scale_input)
        scale_out <- t(scale_out)
      }
      expected <- as.data.frame(scale_out)
    } else {
      scale_input <- if (!center & scale) {
        if (by_column) {
          apply(argument[, -ignore, drop = FALSE], 2, sd, na.rm = TRUE)
        } else {
          apply(argument[-ignore, , drop = FALSE], 1, sd, na.rm = TRUE)
        }
      } else {
        scale
      }
      expected <- argument
      if (by_column) {
        scale_out <- scale(
          argument[, -ignore, drop = FALSE], center = center,
          scale = scale_input
        )
        expected[, -ignore] <- scale_out
      } else {
        scale_out <- scale(
          t(argument[-ignore, , drop = FALSE]), center = center,
          scale = scale_input
        )
        expected[-ignore, ] <- t(scale_out)
      }
      expected <- as.data.frame(expected)
    }
    if (center) {
      if (length(ignore) == 0) {
        attr(expected, "standardized:center") <- as.numeric(
          attr(scale_out, "scaled:center")
        )
      } else {
        centering <- rep(0, dim(argument)[ifelse(by_column, 2, 1)])
        centering[-ignore] <- as.numeric(attr(scale_out, "scaled:center"))
        attr(expected, "standardized:center") <- centering
      }
    }
    if (scale) {
      if (length(ignore) == 0) {
        attr(expected, "standardized:scale") <- as.numeric(
          attr(scale_out, "scaled:scale")
        )
      } else {
        scalings <- rep(1, dim(argument)[ifelse(by_column, 2, 1)])
        scalings[-ignore] <- as.numeric(attr(scale_out, "scaled:scale"))
        attr(expected, "standardized:scale") <- scalings
      }
    }
    out <- standardize_helper(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore, jointly = jointly
    )
    expect_equal(out, expected)
  }
})

test_that("standardization of matrix works", {
  argument <- matrix(rnorm(9), 3, 3)
  combinations <- expand.grid(
    by_column = c(TRUE, FALSE),
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 1),
    jointly = list(list(), list(2:3)),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_column <- combinations[i, "by_column"]
    center <- combinations[i, "center"]
    scale <- combinations[i, "scale"]
    ignore <- combinations[[i, "ignore"]]
    jointly <- combinations[[i, "jointly"]]
    if (length(jointly) > 0) {
      ### no unit tests implemented yet
      next
    }
    if (length(ignore) == 0) {
      scale_input <- if (!center & scale) {
        apply(argument, ifelse(by_column, 2, 1), sd, na.rm = TRUE)
      } else {
        scale
      }
      if (by_column) {
        scale_out <- scale(argument, center = center, scale = scale_input)
      } else {
        scale_out <- scale(t(argument), center = center, scale = scale_input)
        scale_out <- t(scale_out)
      }
      expected <- as.matrix(scale_out)
    } else {
      scale_input <- if (!center & scale) {
        if (by_column) {
          apply(argument[, -ignore, drop = FALSE], 2, sd, na.rm = TRUE)
        } else {
          apply(argument[-ignore, , drop = FALSE], 1, sd, na.rm = TRUE)
        }
      } else {
        scale
      }
      expected <- argument
      if (by_column) {
        scale_out <- scale(
          argument[, -ignore, drop = FALSE], center = center,
          scale = scale_input
        )
        expected[, -ignore] <- scale_out
      } else {
        scale_out <- scale(
          t(argument[-ignore, , drop = FALSE]), center = center,
          scale = scale_input
        )
        expected[-ignore, ] <- t(scale_out)
      }
      expected <- as.matrix(expected)
    }
    attr(expected, "scaled:center") <- NULL
    attr(expected, "scaled:scale") <- NULL
    if (center) {
      if (length(ignore) == 0) {
        attr(expected, "standardized:center") <- as.numeric(
          attr(scale_out, "scaled:center")
        )
      } else {
        centering <- rep(0, dim(argument)[ifelse(by_column, 2, 1)])
        centering[-ignore] <- as.numeric(attr(scale_out, "scaled:center"))
        attr(expected, "standardized:center") <- centering
      }
    }
    if (scale) {
      if (length(ignore) == 0) {
        attr(expected, "standardized:scale") <- as.numeric(
          attr(scale_out, "scaled:scale")
        )
      } else {
        scalings <- rep(1, dim(argument)[ifelse(by_column, 2, 1)])
        scalings[-ignore] <- as.numeric(attr(scale_out, "scaled:scale"))
        attr(expected, "standardized:scale") <- scalings
      }
    }
    out <- standardize_helper(
      argument = argument, by_column = by_column, center = center,
      scale = scale, ignore = ignore, jointly = jointly
    )
    expect_equal(out, expected)
  }
})

test_that("input checks for subsetting work", {
  expect_error(
    subset_helper(
      argument = diag(3), by_row = TRUE,
      how = TRUE, proportion = 0.5, centers = 2, ignore = integer()
    ),
    "must be a single"
  )
  expect_error(
    subset_helper(
      argument = diag(3), by_row = TRUE, how = "bad_specification",
      proportion = 0.5, centers = 2, ignore = integer()
    ),
    "is misspecified"
  )
  expect_error(
    subset_helper(
      argument = diag(3), by_row = "not_a_boolean",
      how = "random", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "must be"
  )
  expect_error(
    subset_helper(
      argument = diag(3), by_row = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = pi
    ),
    "must be an index"
  )
  expect_error(
    subset_helper(
      argument = list(), by_row = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "Argument cannot be subsetted."
  )
  expect_error(
    subset_helper(
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
    out <- subset_helper(
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
    out <- subset_helper(
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
    by_row = c(TRUE, FALSE),
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- subset_helper(
      argument = argument, by_row = by_row, how = how, proportion = proportion
    )
    expected_dim <- if (by_row) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    if (by_row) {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[, j] %in% argument[, j]))
      }
    } else {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[j, ] %in% argument[j, ]))
      }
    }
  }
})

test_that("subsetting of data.frame works (with clusters)", {
  argument <- data.frame(
    "a" = c(1, 1, 6, 6, 6), "b" = c(5, 5, 2, 2, 2), "c" = c(0, 0, 0, 3, 3)
  )
  combinations <- expand.grid(
    by_row = c(TRUE, FALSE),
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
    out <- subset_helper(
      argument = argument, by_row = by_row, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- if (by_row) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    if (by_row) {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[, j] %in% argument[, j]))
      }
    } else {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[j, ] %in% argument[j, ]))
      }
    }
  }
})

test_that("subsetting of matrix works (without clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    by_row = c(TRUE, FALSE),
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    by_row <- combinations[i, "by_row"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- subset_helper(
      argument = argument, by_row = by_row, how = how, proportion = proportion
    )
    expected_dim <- if (by_row) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    if (by_row) {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[, j] %in% argument[, j]))
      }
    } else {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[j, ] %in% argument[j, ]))
      }
    }
  }
})

test_that("subsetting of matrix works (with clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    by_row = c(TRUE, FALSE),
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
    out <- subset_helper(
      argument = argument, by_row = by_row, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- if (by_row) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    if (by_row) {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[, j] %in% argument[, j]))
      }
    } else {
      for (j in seq_len(ncol(argument))) {
        expect_true(all(out[j, ] %in% argument[j, ]))
      }
    }
  }
})

# TODO

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
  out <- helper_flatten_list(results)
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
  out <- helper_flatten_list(results)
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
  out <- helper_flatten_list(results)
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
  out <- helper_flatten_list(
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
  out <- helper_flatten_list(
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
  out <- helper_flatten_list(
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




# TODO



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
    helper_flatten_list(
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


