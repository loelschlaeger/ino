sim_probit <- function(N, T, J, P) {
  form <- as.formula(paste("choice ~",paste0(LETTERS[1:P],collapse=" + "),"| 0"))
  re <- LETTERS[1:P]
  allvars <- Rprobit::read_formula(form)$allvars
  mod <- Rprobit::check_mod(list(N = N, Tp = T, alt = J, lthb = P, lRE = P))
  mod <- Rprobit::make_mappings(mod = mod, corr_re = TRUE, error_struc = "full")
  data <- Rprobit::sim_data_raw(
    ASC = FALSE, allvars = allvars, re = re, choice = all.vars(form)[1],
    theta = NULL, mod = mod, seed = 1
  )
  structure(
    data$data_raw,
    "allvars" = allvars,
    "mod" = mod,
    "true" = data$theta_0
  )
}

ll_probit <- function(theta, data) {
  data_full <- Rprobit::data_raw_to_data(
    data_raw = data,
    allvars = attr(data, "allvars"),
    choice = "choice",
    re = LETTERS[1:attr(data, "mod")$lRE],
    norm_alt = 1,
    alt = attr(data, "mod")$alt,
    ASC = FALSE
  )$data
  data_tr <- Rprobit::substract_choice_regressor_from_data(data_full)
  control <- list(probit = TRUE,
                  approx_method = "SJ",
                  bayes = FALSE,
                  bayes_specs = NULL,
                  hessian = FALSE)
  as.numeric(Rprobit::ll_macml(theta = theta, data = data_tr, mod = attr(data, "mod"),
                               control = control))
}

probit_data <- sim_probit(N = 30, T = 5, J = 2, P = 2)

Rprobit_ino <- setup_ino(
  f = ll_probit,
  npar = 5,
  global = attr(probit_data, "true"),
  data = probit_data,
  opt = list(
    "nlm" = set_optimizer_nlm(),
    "ao" = set_optimizer_ao(partition = list(1:2, 3:5))
  )
)

Rprobit_ino <- random_initialization(Rprobit_ino, runs = 10)

overview_optima(Rprobit_ino, digits = 2)
