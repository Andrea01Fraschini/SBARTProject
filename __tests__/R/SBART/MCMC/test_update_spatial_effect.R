source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{

    source("R/SBART/MCMC/update_spatial_effect.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_sample_variance.R")
    source("output/KIM/output_update_spatial_effect.R")

    
    set.seed(1)
    spatial_theta <- update_spatial_effect(
        y = output_init_chain$y,
        trees_pred = output_init_chain$trees_pred,
        w_post_full = output_init_chain$w_post_full,
        n_locations_all = output_init_model_parameters$n_locations_all,
        spatial_theta = output_init_chain$spatial_theta,
        tau2 = output_init_model_parameters$tau2,
        rho = output_init_model_parameters$rho,
        sigma2_samples = output_sample_variance$sigma2_samples,
        j = 2
    ) 

    it("should return a numeric value",{
        expect_is(spatial_theta, "numeric")
    })

    it("should return kim's output",{
        expect_equal(spatial_theta, output_update_spatial_effect$spatial_theta)
    })
    

})