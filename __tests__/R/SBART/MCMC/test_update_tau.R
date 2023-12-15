source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("R/SBART/MCMC/update_tau.R")
    source("output/KIM/output_update_tau.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_spatial_effect.R")

    #  TODO: Change temp name
    set.seed(1)
    update_tau2_results <- update_tau2(
        w_post_full = output_init_chain$w_post_full,
        n_locations_all = output_init_model_parameters$n_locations_all,
        spatial_theta = output_update_spatial_effect$spatial_theta,
        rho = output_init_model_parameters$rho,
        tau2_b = output_init_model_parameters$tau2_b,
        tau2_posterior_shape = output_init_model_parameters$tau2_posterior_shape,
        tau2_samples = output_init_chain$tau2_samples,
        j = 2
    ) # tau2_samples, temp

    it("should return a list",{
        expect_is(update_tau2_results, "list")
    })

    it("should resutrn a list with two elements",{
        expect_equal(length(update_tau2_results), 2)
    })
    
    it("should return a the correct classes",{
        expect_is(update_tau2_results$tau2_samples, "numeric")
        expect_is(update_tau2_results$temp, "numeric")
    })

    it("should return kim's output",{
        expect_equal(update_tau2_results$tau2_samples, output_update_tau$tau2_samples)
        expect_equal(update_tau2_results$temp, output_update_tau$temp)
    })
})