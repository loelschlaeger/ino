library("testthat")
library("ino")

tmp_ino_verbose <- getOption("ino_verbose")
options("ino_verbose" = FALSE)

test_check("ino")

options("ino_verbose" = tmp_ino_verbose)
rm(tmp_ino_verbose)
