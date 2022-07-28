test_par = list(
  validate = TRUE,
  init_rest = list("lower" = -1, "upper" = 1),
  init_digits = 2,
  f_checks = 10,
  f_checks_time = 1,
  opt_checks = 10,
  opt_checks_time = 1
)

logit_data <- list()
for(i in 1:10){
  b <- rnorm(3, sd = 3)
  Omega <- RprobitB::rwishart(3,diag(3))$W
  name <- paste0("data",i)
  logit_data[[name]] <- sim_mnl(N = 300, J = 3, b = b, Omega = Omega, seed = i)
}

x <- setup_ino(
  f = f_ll_mnl,
  npar = 9,
  data = logit_data,
  R = list("R1" = 10, "R2" = 100),
  neg = TRUE,
  mpvs = c("data", "R"),
  opt = set_optimizer_nlm(),
  test_par = test_par
)

x <- new_ino(
  f = f_ll_mnl,
  npar = 9,
  add = list(data = logit_data, R = list("R1" = 10, "R2" = 100), neg = TRUE),
  mpvs = c("data", "R"),
  f_name = "f_ll_mnl",
  f_target = "theta",
  opt = set_optimizer_nlm()
)

grid <- grid_ino(x)

x$prob <- validate_prob(x = x$prob, test_par = test_par)


validate_ino(x)
