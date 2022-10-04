options("ino_progress" = FALSE)

test_that("get_vars works", {
  x <- setup_ino(
    f = f_ackley,
    npar = 2,
    global = c(0, 0),
    opt = list(
      "nlm" = set_optimizer_nlm(),
      "optim" = set_optimizer_optim()
    )
  )
  x <- random_initialization(x)
  expect_type(get_vars(x), "list")
  expect_length(get_vars(x), 2)
  expect_error(get_vars(x, run_ids = "not_a_number"))
  expect_error(get_vars(x, vars = 1))
  expect_error(get_vars(x = 1))
  expect_error(get_vars(x = x, run_ids = 0))
  expect_type(get_vars(x, run_ids = 1, vars = ".estimate"), "double")
  expect_type(get_vars(x, run_ids = 1, vars = ".estimate", simplify = FALSE), "list")
  expect_type(get_vars(x, run_ids = 1, vars = c(".init", ".estimate")), "list")
  expect_warning(get_vars(x, vars = "this_var_does_not_exist"))
})
