library(testthat)
library(ino)

ino_verbose <- getOption("ino_verbose")
ino_ncores <- getOption("ino_ncores")
options("ino_verbose" = FALSE)
options("ino_ncores" = 1)

test_check("ino")

options("ino_verbose" = ino_verbose)
options("ino_ncores" = ino_ncores)
