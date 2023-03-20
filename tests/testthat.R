library(testthat)
library(ino)

ino_verbose <- getOption("ino_verbose")
options("ino_verbose" = FALSE)
data("earthquakes")

test_check("ino")

rm("earthquakes")
options("ino_verbose" = ino_verbose)
