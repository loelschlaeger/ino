test_that("checks for proper number vector work", {
  expect_true(is_number_vector(1))
  expect_true(is_number_vector(c(-pi, 0, pi)))
  expect_error(is_number_vector("1"), "must be")
  expect_error(is_number_vector(LETTERS), "must be")
  expect_true(is_number_vector(c(NA, 1, 2)))
  expect_error(is_number_vector(c(NA, 1, 2), allow_na = FALSE), "must be")
})

test_that("checks for proper number work", {
  expect_true(is_number(1))
  expect_true(is_number(pi))
  expect_true(is_number(NA_real_))
  expect_error(is_number("1"), "must be")
  expect_error(is_number(1:2), "must be")
  expect_error(is_number(NA), "must be")
  expect_error(is_number(NA_real_, allow_na = FALSE), "must be")
})

test_that("checks for proper proportion work", {
  expect_true(is_proportion(0.5))
  expect_error(is_proportion(pi), "between 0 and 1")
  expect_error(is_proportion("1"), "must be a")
})

test_that("checks for proper count work", {
  expect_true(is_count(1))
  expect_error(is_count(pi), "must be a single, positive")
  expect_error(is_count("1"), "must be")
  expect_error(is_count(-1), "must be a single, positive")
})

test_that("checks for proper name work", {
  expect_true(is_name("one"))
  expect_error(is_name(1), "must be a single")
  expect_error(is_name(LETTERS[1:2]), "must be a single")
  expect_error(is_name(""), "must be a single")
})

test_that("checks for proper name vector work", {
  expect_true(is_name_vector("one"))
  expect_error(is_name_vector(1), "must be a")
  expect_true(is_name_vector(LETTERS[1:2]))
  expect_error(is_name_vector(""), "must be a")
})

test_that("checks for proper time work", {
  expect_true(is_time(1))
  expect_true(is_time(0))
  expect_error(is_time(-1), "is not a non-negative")
  expect_error(is_time_limit("1"), "must be a")
})

test_that("checks for proper time limit work", {
  expect_true(is_time_limit(1))
  expect_true(is_time_limit(pi))
  expect_error(is_time_limit(0), "is not a positive")
  expect_error(is_time_limit("1"), "must be a")
})

test_that("checks for proper boolean work", {
  expect_true(is_TRUE_FALSE(TRUE))
  expect_error(is_TRUE_FALSE(1), "must be")
  expect_error(is_TRUE_FALSE("TRUE"), "must be")
})

test_that("checks for proper index vector work", {
  expect_true(is_index_vector(1:10))
  expect_error(is_index_vector(-3:3), "must be")
  expect_error(is_index_vector(LETTERS), "must be")
  expect_error(is_index_vector(c(1, 2, pi)), "must be")
})
