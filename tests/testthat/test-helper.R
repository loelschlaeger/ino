test_that("initial parameter specifications are unified", {
  expect_length(build_initial("random", 2)(1), 2)
  expect_equal(build_initial(list(1:3, 2:4), 3)(2), 2:4)
  expect_error(
    build_initial(list(1:3, 2:4), 2),
    "Each of them should be of length 2."
  )
  expect_error(
    build_initial(list(LETTERS[1:2], LETTERS[3:4]), 2),
    "should only contain"
  )
  expect_equal(build_initial(1:3, 3)(1), 1:3)
  expect_error(
    build_initial(1:4, 3),
    "It should be of length 3."
  )
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
  expect_error(
    build_initial(diag(2), 4),
    "Please see the documentation for possible inputs."
  )
})

test_that("checks for active optimizers work", {

})
