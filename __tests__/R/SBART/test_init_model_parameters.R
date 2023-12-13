source('../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test init_model_parameters function",{
    source("R/SBART/init_model_parameters.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_init_model_parameters.R")
    
    set.seed(1)
    results <- init_model_parameters(
        x = output_sample_data_1$x_predictors,
        y = output_sample_data_1$y,
        siam = output_sample_data_1$wind_matrix,
        n_trees = 50L
    )

    it("should return a list",{
        expect_is(results, "list")
    })

    it("should return a list with 28 elements",{
        expect_equal(length(results), 28)
    })

    it("should return a list with the correct names",{
        expect_equal(names(results), c("p", "n", "n_locations_all", "prob_grow", "prob_prune", "prob_change", "alpha", "beta", 
                      "dirichlet_alpha", "posterior_dirichlet_alpha", "cov_sel_prob", "tau2_alpha", "tau2_beta", 
                      "tau2_posterior_shape", "tau2", "proposal_sd_rho", "rho", "a0", "b0", 
                      "y", "residuals", "sigma2", "nu", "lambda", "sigma2_a", "sigma2_b", 
                       "sigma_mu", "missing_indexes"))
    })

    it("should return a list with the correct class",{
        expect_is(results$p, "integer")
        expect_is(results$n, "integer")
        expect_is(results$n_locations_all, "integer")
        expect_is(results$prob_grow, "numeric")
        expect_is(results$prob_prune, "numeric")
        expect_is(results$prob_change, "numeric")
        expect_is(results$alpha, "numeric")
        expect_is(results$beta, "numeric")
        expect_is(results$dirichlet_alpha, "numeric")
        expect_is(results$posterior_dirichlet_alpha, "numeric")
        expect_is(results$cov_sel_prob, "array")
        expect_is(results$tau2_alpha, "numeric")
        expect_is(results$tau2_beta, "numeric")
        expect_is(results$tau2_posterior_shape, "numeric")
        expect_is(results$tau2, "numeric")
        expect_is(results$proposal_sd_rho, "numeric")
        expect_is(results$rho, "numeric")
        expect_is(results$a0, "numeric")
        expect_is(results$b0, "numeric")
        expect_is(results$y, "numeric")
        expect_is(results$residuals, "numeric")
        expect_is(results$sigma2, "numeric")
        expect_is(results$nu, "numeric")
        expect_is(results$lambda, "numeric")
        expect_is(results$sigma2_a, "numeric")
        expect_is(results$sigma2_b, "numeric")
        expect_is(results$sigma_mu, "numeric")
        expect_is(results$missing_indexes, "integer")
    })

    it("Should return Kim's results",{
        expect_equal(results$p, output_init_model_parameters$p)
        expect_equal(results$n, output_init_model_parameters$n)
        expect_equal(results$n_locations_all, output_init_model_parameters$n_locations_all)
        expect_equal(results$prob_grow, output_init_model_parameters$prob_grow)
        expect_equal(results$prob_prune, output_init_model_parameters$prob_prune)
        expect_equal(results$prob_change, output_init_model_parameters$prob_change)
        expect_equal(results$alpha, output_init_model_parameters$alpha)
        expect_equal(results$beta, output_init_model_parameters$beta)
        expect_equal(results$dirichlet_alpha, output_init_model_parameters$dirichlet_alpha)
        expect_equal(results$posterior_dirichlet_alpha, output_init_model_parameters$posterior_dirichlet_alpha)
        expect_equal(results$cov_sel_prob, output_init_model_parameters$cov_sel_prob)
        expect_equal(results$tau2_alpha, output_init_model_parameters$tau2_alpha)
        expect_equal(results$tau2_beta, output_init_model_parameters$tau2_beta)
        expect_equal(results$tau2_posterior_shape, output_init_model_parameters$tau2_posterior_shape)
        expect_equal(results$tau2, output_init_model_parameters$tau2)
        expect_equal(results$proposal_sd_rho, output_init_model_parameters$proposal_sd_rho)
        expect_equal(results$rho, output_init_model_parameters$rho)
        expect_equal(results$a0, output_init_model_parameters$a0)
        expect_equal(results$b0, output_init_model_parameters$b0)
        expect_equal(results$y, output_init_model_parameters$y)
        expect_equal(results$residuals, output_init_model_parameters$residuals)
        expect_equal(results$sigma2, output_init_model_parameters$sigma2)
        expect_equal(results$nu, output_init_model_parameters$nu)
        expect_equal(results$lambda, output_init_model_parameters$lambda)
        expect_equal(results$sigma2_a, output_init_model_parameters$sigma2_a)
        expect_equal(results$sigma2_b, output_init_model_parameters$sigma2_b)
        expect_equal(results$sigma_mu, output_init_model_parameters$sigma_mu)
        expect_equal(results$missing_indexes, output_init_model_parameters$missing_indexes)
    })

})