rm(list = ls())

if(!require("devtools")) install.packages("devtools")
if(!require("testthat")) install.packages("testthat")
library(testthat)

test_dir("__tests__/data")
test_dir("__tests__/R/SBART")
test_dir("__tests__/R/SBART/MCMC")