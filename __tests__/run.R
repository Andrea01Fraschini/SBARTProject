rm(list = ls())
library(testthat)

GLOBAL_test_environment_wd <- getwd()

test_dir("__tests__/data")
test_dir("__tests__/R/SBART")
test_dir("__tests__/R/SBART/MCMC")

# --------------------------------------------------

print("Do you want to run the tests automatically? (y/n)")
answer <- readline(prompt = "")

if(answer == "y") {
    auto_test("data","__tests__/data")
    auto_test("R/SBART","__tests__/R/SBART")
    auto_test("R/SBART/MCMC","__tests__/R/SBART/MCMC")
} 

