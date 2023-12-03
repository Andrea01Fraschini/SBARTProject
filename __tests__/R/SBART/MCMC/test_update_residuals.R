rm(list = ls())
setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject")
library(testthat)
source("R/library_imports.R")

describe("Test sample data function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("R/SBART/MCMC/update_residuals.R")
    source("__tests__/R/SBART/MCMC/kim_output/output_update_residuals.R")

    set.seed(1)
    data <- sample_data()
    
    set.seed(1)
    params <- init_model_parameters(
        X = data$Xpred,
        Y = data$Y,
        SIAM = data$wind_mat,
        n.trees = 50L
    )

    set.seed(1)
    vars <- init_chain(
        n.iterations = 10000L,
        n.locations.all = params$n.locations.all,
        p = params$p,
        n = params$n,
        n.trees = 50L,
        X = data$Xpred,
        missing_indexes = data$mis.ind,
        SIAM = data$wind_mat,
        W = data$Ws,
        Y = params$Y,
        rho = params$rho
    )

    residuals <- update_residuals(
        Y = vars$Y, 
        vars$trees, 
        1,
        vars$spatial_theta,
        vars$missing_indexes
    )

    it("should return a vector",{
        expect_is(residuals, "numeric")
    })

    it("should return a vector with the correct length",{
        expect_equal(length(residuals), 30)
    })

    it("should return kim's result for first iteration and first tree",{
        expect_equal(residuals, output_update_residuals$residuals)
    })


})