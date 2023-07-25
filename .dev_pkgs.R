
if (!require("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!require("covr", quietly = TRUE)) {
  renv::install("covr", prompt = FALSE)
}

if (!require("devtools", quietly = TRUE)) {
  renv::install("devtools", prompt = FALSE)
}

if (!require("DT", quietly = TRUE)) {
  renv::install("DT", prompt = FALSE)
}

if (!require("testthat", quietly = TRUE)) {
  renv::install("testthat", prompt = FALSE)
}
