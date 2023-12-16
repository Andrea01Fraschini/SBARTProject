source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("R/SBART/MCMC/update_dirichlet_alpha.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/input_sample_variance.R")
    source("output/KIM/output_update_dirichlet_alpha.R")
    source("output/KIM/input_update_dirichlet_alpha.R")
    source("__tests__/scripts/transform_dt_list.R")

    transformed_dt_list <- transform_dt_list(input_update_dirichlet_alpha$dt_list)

    set.seed(1)
    result <- update_dirichlet_alpha(
        dt_list = transformed_dt_list,
        p = output_init_model_parameters$p,
        j = 2,
        warmup = 1000L,
        dirichlet_alpha = output_init_model_parameters$dirichlet_alpha,
        a0 = output_init_model_parameters$a0,
        b0 = output_init_model_parameters$b0,
        cov_sel_prob = output_init_model_parameters$cov_sel_prob
    ) # cov_sel_prob, rules_count

    it("should get kim's result",{
        expect_equal(result$cov_sel_prob, output_update_dirichlet_alpha$cov_sel_prob)
        expect_equal(result$rules_count, output_update_dirichlet_alpha$rules_count)
    })
})