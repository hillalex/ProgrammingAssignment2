#!/usr/bin/env Rscript
source("cachematrix.R")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE, stop_on_warning = TRUE)
