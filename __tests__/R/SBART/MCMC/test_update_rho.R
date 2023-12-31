source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("R/SBART/MCMC/update_rho.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_spatial_effect.R")
    source("output/KIM/output_update_tau.R")
    source("output/KIM/output_update_rho.R")

    set.seed(1)
    rho_update_results <- update_rho(
        rho = output_init_model_parameters$rho,
        proposal_sd_rho = output_init_model_parameters$proposal_sd_rho,
        w_post_full = output_init_chain$w_post_full,
        n_locations_all = output_init_model_parameters$n_locations_all,
        spatial_theta = output_update_spatial_effect$spatial_theta,
        tau2 = output_init_model_parameters$tau2,
        w_star_eigen_vals = output_init_chain$w_star_eigen_vals,
        det_q = output_init_chain$det_q,
        temp = output_update_tau$temp,
        rho_samples = output_init_chain$rho_samples,
        j = 2
    ) # rho_samples, temp, det_q

    it("Should return a list",{
        expect_is(rho_update_results, "list")
    })
    
    it("Should return a list with 3 elements",{
        expect_equal(length(rho_update_results), 3)
    })

    it("Should return a list with the correct names",{
        expect_equal(names(rho_update_results), c("rho_samples", "det_q", "temp"))
    })

    it("should return kim's output",{
        expect_equal(rho_update_results$rho_samples, output_update_rho$rho_samples)
        expect_equal(rho_update_results$temp, output_update_rho$temp)
        expect_equal(rho_update_results$det_q, output_update_rho$det_q)
    })
    

})