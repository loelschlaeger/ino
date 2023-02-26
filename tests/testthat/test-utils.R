test_that("checks for numbers work", {
  expect_true(is_number(1))
  expect_false(is_number(pi))
  expect_false(is_number("1"))
})

test_that("checks for names work", {
  expect_true(is_name("one"))
  expect_false(is_name(""))
  expect_false(is_name(1))
  expect_false(is_name(LETTERS[1:2]))
})
