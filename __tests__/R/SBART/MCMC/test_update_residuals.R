source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample data function",{
    source("R/SBART/MCMC/update_residuals.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_residuals.R")

    residuals <- update_residuals(
        y = output_init_chain$y, 
        trees = output_init_chain$trees, 
        t = 1,
        spatial_theta = output_init_chain$spatial_theta,
        missing_indexes = output_init_chain$missing_indexes
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