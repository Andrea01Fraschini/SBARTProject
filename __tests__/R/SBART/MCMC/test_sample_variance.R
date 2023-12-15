source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("R/SBART/MCMC/sample_variance.R")
    source("output/KIM/input_sample_variance.R")
    source("output/KIM/output_sample_variance.R")


    set.seed(1)
    sigma2_samples <- sample_variance(
        y = input_sample_variance$y,
        trees = input_sample_variance$trees,
        spatial_theta = input_sample_variance$spatial_theta,
        missing_indexes = input_sample_variance$missing_indexes,
        sigma2_alpha = input_sample_variance$sigma2_alpha,
        sigma2_beta = input_sample_variance$sigma2_beta,
        n_locations_all = input_sample_variance$n_locations_all,
        sigma2_samples = input_sample_variance$sigma2_samples,
        j = 2
    )

    it("should return a numeric value representing a sample from the posterior distribution of σ^2",{
        expect_is(sigma2_samples, "numeric")
    })

    it("should return a value equal to the one in the kim_output file",{  
        expect_equal(sigma2_samples, output_sample_variance$sigma2_samples)
    })

})