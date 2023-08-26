test_that("Input checks for standardization work", {
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = "not_a_boolean",
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "Must be of type 'logical', not 'character'"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = pi, jointly = list()
    ),
    "Must be of type 'integerish'"
  )
  expect_error(
    helper_standardize(
      argument = list(), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "Argument cannot be standardized"
  )
  expect_warning(
    helper_standardize(
      argument = 1, byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(), jointly = list()
    ),
    "Argument has NAs after standardization"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = 4, jointly = list()
    ),
    "is out of bound"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(), jointly = "not_a_list"
    ),
    "Must be of type 'list', not 'character'"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(),
      jointly = list("not_an_index_vector")
    ),
    "must contain index vectors"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(),
      jointly = list(3:4)
    ),
    "out of bound"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = integer(),
      jointly = list(1:2, 2:3)
    ),
    "must be exclusive"
  )
  expect_error(
    helper_standardize(
      argument = diag(3), byrow = TRUE,
      center = TRUE, scale = TRUE, ignore = 3,
      jointly = list(2:3)
    ),
    "same elements"
  )
})

test_that("Standardization of vector works", {
  argument <- rnorm(10)
  combinations <- expand.grid(
    byrow = TRUE,
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(integer()),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
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
    out <- helper_standardize(
      argument = argument, byrow = byrow, center = center,
      scale = scale, ignore = ignore
    )
    expect_equal(out, expected)
  }
})

test_that("Standardization of data.frame works", {
  argument <- data.frame("a" = rnorm(10), "b" = rnorm(10), "c" = rnorm(10))
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 1),
    jointly = list(list(), list(2:3)),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
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
        apply(argument, ifelse(byrow, 1, 2), sd, na.rm = TRUE)
      } else {
        scale
      }
      if (byrow) {
        scale_out <- scale(t(argument), center = center, scale = scale_input)
        scale_out <- t(scale_out)
      } else {
        scale_out <- scale(argument, center = center, scale = scale_input)
      }
      expected <- as.data.frame(scale_out)
    } else {
      scale_input <- if (!center & scale) {
        if (byrow) {
          apply(argument[-ignore, , drop = FALSE], 1, sd, na.rm = TRUE)
        } else {
          apply(argument[, -ignore, drop = FALSE], 2, sd, na.rm = TRUE)
        }
      } else {
        scale
      }
      expected <- argument
      if (byrow) {
        scale_out <- scale(
          t(argument[-ignore, , drop = FALSE]), center = center,
          scale = scale_input
        )
        expected[-ignore, ] <- t(scale_out)
      } else {
        scale_out <- scale(
          argument[, -ignore, drop = FALSE], center = center,
          scale = scale_input
        )
        expected[, -ignore] <- scale_out
      }
      expected <- as.data.frame(expected)
    }
    if (center) {
      if (length(ignore) == 0) {
        attr(expected, "standardized:center") <- as.numeric(
          attr(scale_out, "scaled:center")
        )
      } else {
        centering <- rep(0, dim(argument)[ifelse(byrow, 1, 2)])
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
        scalings <- rep(1, dim(argument)[ifelse(byrow, 1, 2)])
        scalings[-ignore] <- as.numeric(attr(scale_out, "scaled:scale"))
        attr(expected, "standardized:scale") <- scalings
      }
    }
    out <- helper_standardize(
      argument = argument, byrow = byrow, center = center,
      scale = scale, ignore = ignore, jointly = jointly
    )
    expect_equal(out, expected)
  }
})

test_that("Standardization of matrix works", {
  argument <- matrix(rnorm(9), 3, 3)
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    center = c(TRUE, FALSE),
    scale = c(TRUE, FALSE),
    ignore = list(numeric(), 1),
    jointly = list(list(), list(2:3)),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
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
        apply(argument, ifelse(byrow, 1, 2), sd, na.rm = TRUE)
      } else {
        scale
      }
      if (byrow) {
        scale_out <- scale(t(argument), center = center, scale = scale_input)
        scale_out <- t(scale_out)
      } else {
        scale_out <- scale(argument, center = center, scale = scale_input)
      }
      expected <- as.matrix(scale_out)
    } else {
      scale_input <- if (!center & scale) {
        if (byrow) {
          apply(argument[-ignore, , drop = FALSE], 1, sd, na.rm = TRUE)
        } else {
          apply(argument[, -ignore, drop = FALSE], 2, sd, na.rm = TRUE)
        }
      } else {
        scale
      }
      expected <- argument
      if (byrow) {
        scale_out <- scale(
          t(argument[-ignore, , drop = FALSE]), center = center,
          scale = scale_input
        )
        expected[-ignore, ] <- t(scale_out)
      } else {
        scale_out <- scale(
          argument[, -ignore, drop = FALSE], center = center,
          scale = scale_input
        )
        expected[, -ignore] <- scale_out
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
        centering <- rep(0, dim(argument)[ifelse(byrow, 1, 2)])
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
        scalings <- rep(1, dim(argument)[ifelse(byrow, 1, 2)])
        scalings[-ignore] <- as.numeric(attr(scale_out, "scaled:scale"))
        attr(expected, "standardized:scale") <- scalings
      }
    }
    out <- helper_standardize(
      argument = argument, byrow = byrow, center = center,
      scale = scale, ignore = ignore, jointly = jointly
    )
    expect_equal(out, expected)
  }
})

test_that("Standardization jointly works", {
  argument <- matrix(1:3, ncol = 3, nrow = 3)
  expect_equal(
    helper_standardize(
      argument = argument, jointly = as.list(1:3)
    ),
    helper_standardize(
      argument = argument, jointly = list()
    )
  )
  argument <- matrix(1:3, ncol = 3, nrow = 3, byrow = TRUE)
  expect_equal(
    helper_standardize(
      argument = argument, jointly = as.list(1:3), byrow = TRUE
    ),
    helper_standardize(
      argument = argument, jointly = list(), byrow = TRUE
    )
  )
})

test_that("Input checks for subsetting work", {
  expect_error(
    helper_subset(
      argument = diag(3), byrow = TRUE, how = "bad_specification",
      proportion = 0.5, centers = 2, ignore = integer()
    ),
    "must be one of"
  )
  expect_error(
    helper_subset(
      argument = diag(3), byrow = "not_a_boolean",
      how = "random", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "Must be of type 'logical', not 'character'"
  )
  expect_error(
    helper_subset(
      argument = diag(3), byrow = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = pi
    ),
    "Must be of type 'integerish', but element 1 is not close to an integer"
  )
  expect_error(
    helper_subset(
      argument = list(), byrow = TRUE,
      how = "similar", proportion = 0.5, centers = 2, ignore = integer()
    ),
    "Subsetting can only be applied to"
  )
  expect_error(
    helper_subset(
      argument = diag(3), byrow = TRUE,
      how = "similar", proportion = -1, centers = 2, ignore = integer()
    ),
    "Element 1 is not >= 0"
  )
})

test_that("Subsetting of vector works (without clusters)", {
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
    out <- helper_subset(
      argument = argument, how = how, proportion = proportion, ignore = ignore
    )
    expect_true(is.vector(out))
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
})

test_that("Subsetting of vector works (with clusters)", {
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
    out <- helper_subset(
      argument = argument, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expect_true(is.vector(out))
    expect_length(out, expected_length)
    expect_true(all(out %in% argument))
  }
  expect_error(
    helper_subset(
      argument = rep(1, 10), how = "similar", centers = 2
    ),
    "more cluster centers than distinct data points"
  )
})

test_that("Subsetting of data.frame works (without clusters)", {
  argument <- data.frame("a" = 1:5, "b" = LETTERS[1:5])
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- helper_subset(
      argument = argument, byrow = byrow, how = how, proportion = proportion
    )
    expected_dim <- if (byrow) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    if (byrow) {
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

test_that("Subsetting of data.frame works (with clusters)", {
  argument <- data.frame(
    "a" = c(1, 1, 6, 6, 6), "b" = c(5, 5, 2, 2, 2), "c" = c(0, 0, 0, 3, 3)
  )
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    how = c("similar", "dissimilar"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    centers = 1:2,
    ignore = list(integer(), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    ignore <- combinations[[i, "ignore"]]
    out <- helper_subset(
      argument = argument, byrow = byrow, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- if (byrow) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.data.frame(out))
    expect_equal(dim(out), expected_dim)
    if (byrow) {
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

test_that("Subsetting of matrix works (without clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    how = c("random", "first", "last"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    out <- helper_subset(
      argument = argument, byrow = byrow, how = how, proportion = proportion
    )
    expected_dim <- if (byrow) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    if (byrow) {
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

test_that("Subsetting of matrix works (with clusters)", {
  argument <- matrix(1:15, nrow = 5, ncol = 3)
  combinations <- expand.grid(
    byrow = c(TRUE, FALSE),
    how = c("similar", "dissimilar"),
    proportion = round(runif(2, min = 0.1, max = 0.9), 2),
    centers = 1:2,
    ignore = list(integer(), 2),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    byrow <- combinations[i, "byrow"]
    how <- combinations[i, "how"]
    proportion <- combinations[i, "proportion"]
    centers <- combinations[i, "centers"]
    ignore <- combinations[[i, "ignore"]]
    out <- helper_subset(
      argument = argument, byrow = byrow, how = how, proportion = proportion,
      centers = centers, ignore = ignore
    )
    expected_dim <- if (byrow) {
      c(ceiling(nrow(argument) * proportion), ncol(argument))
    } else {
      c(nrow(argument), ceiling(ncol(argument) * proportion))
    }
    expect_true(is.matrix(out))
    expect_equal(dim(out), expected_dim)
    if (byrow) {
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

test_that("Argument matching works", {
  expect_equal(
    match_arg("A", "A"),
    "A"
  )
  expect_equal(
    match_arg("A", LETTERS),
    "A"
  )
  expect_error(
    match_arg(c("A", "B"), LETTERS),
    "must be of length 1"
  )
  expect_equal(
    match_arg("lo", c("loooong", "else")),
    "loooong"
  )
  expect_equal(
    match_arg(c("A", "B"), LETTERS, several.ok = TRUE),
    c("A", "B")
  )
  expect_error(
    match_arg(character(), LETTERS),
    "greater or equal 1"
  )
  expect_equal(
    match_arg(character(), LETTERS, none.ok = TRUE),
    character()
  )
  expect_error(
    match_arg("bad", LETTERS),
    "must be one of"
  )
})

test_that("Removing index works", {
  expect_equal(remove_index(1:10, 1), 2:10)
  expect_equal(remove_index(1:10, 1, replace = TRUE), 1:9)
})

test_that("Confirmation by user works", {
  f <- file()
  options("ino_connection" = f)
  ans <- paste(c("n", "y", "", "bad", "y"), collapse = "\n")
  write(ans, f)
  suppressMessages({
    expect_false(user_confirm())
    expect_true(user_confirm())
    expect_false(user_confirm())
    expect_true(user_confirm())
  })
  options("ino_connection" = stdin())
  close(f)
  if (!interactive()) {
    expect_false(user_confirm())
  }
})
