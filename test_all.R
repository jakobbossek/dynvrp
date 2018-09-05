library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(dynvrp)
}

test_dir("tests/testthat")
