rm(list = ls())

# CHANGE THIS PATH TO YOUR LOCAL PATH
setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject") 

if(!require("devtools")) install.packages("devtools", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require("testthat")) install.packages("testthat", repos = "http://cran.us.r-project.org", dependencies = TRUE)
library(testthat)

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

