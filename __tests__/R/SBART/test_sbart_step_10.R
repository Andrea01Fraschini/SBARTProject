# source('../../env_setup_tests.R')
  setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject") 

library(testthat)
source("R/library_imports.R")

describe("Test SBART functions",{
    source("R/SBART/sbart.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_sample_data_2.R")
    source("output/KIM/output_sbart_step_10.R")

    set.seed(1)
    model <- sbart_fit(
        x = output_sample_data_1$x_predictors,
        y = output_sample_data_1$y, 
        ws = output_sample_data_2$ws, 
        siam = output_sample_data_1$wind_matrix,
        missing_indexes = output_sample_data_1$missing_indexes,
        n_trees = 50L,
        n_iterations = 10L,
        warmup = 1000L
    )

    it("should return a list",{
        expect_is(model, "list")
    })

    it("should return a list with 5 elements",{
        expect_equal(length(model), 5)
    })

    it("should return a list with the correct names",{
        expect_equal(names(model), c("covariates_selection_chain", "spatial_theta_chain", "sigma2_chain", "trees_chain", "w_selection_chain"))
    })

    it("Should return Kim's results",{
        expect_equal(model$covariates_selection_chain, output_sbart_step_10$covariates_selection_chain)
        expect_equal(model$spatial_theta_chain, output_sbart_step_10$spatial_theta_chain)
        expect_equal(model$sigma2_chain, output_sbart_step_10$sigma2_chain)
        expect_equal(model$trees_chain, output_sbart_step_10$trees_chain)
        expect_equal(model$w_selection_chain, output_sbart_step_10$w_selection_chain)
    })
})