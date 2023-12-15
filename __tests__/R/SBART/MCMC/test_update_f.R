source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("R/SBART/MCMC/update_f.R")
    source("output/KIM/output_sample_data_2.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_spatial_effect.R")
    source("output/KIM/output_update_rho.R")
    source("output/KIM/output_update_tau.R")
    source("output/KIM/output_update_f.R")

    set.seed(1)
    f_update_results <- update_f(
        ws = output_sample_data_2$ws,
        w_count = output_init_chain$w_count,
        siam = output_sample_data_1$wind_matrix,
        n_locations_all = output_init_model_parameters$n_locations_all,
        spatial_theta = output_update_spatial_effect$spatial_theta,
        rho = output_update_rho$rho_samples[2],
        tau2 = output_update_tau$tau2_samples[2],
        det_q = output_update_rho$det_q,
        temp = output_update_tau$temp,
        w_sel = output_init_chain$w_sel,
        w_siam_full = output_init_chain$w_siam_full,
        w_post_full = output_init_chain$w_post_full,
        w_star = output_init_chain$w_star,
        w_star_eigen = output_init_chain$w_star_eigen,
        w_star_eigen_vals = output_init_chain$w_star_eigen_vals,
        w_sel_samples = output_init_chain$w_sel_samples,
        j = 2
    ) # w_sel_samples, det_q, w_siam_full, w_post_full, w_star, w_star_eigen, w_star_eigen_vals
    
    it("Should return a list with the correct length",{
        expect_equal(length(f_update_results), 7)
    })

    it("Should return a list with the correct names",{
        expect_equal(names(f_update_results), c("w_sel_samples", "det_q", "w_siam_full", "w_post_full", "w_star", "w_star_eigen", "w_star_eigen_vals"))
    })

    it("Should return a list with the correct classes",{
        expect_is(f_update_results$w_sel_samples, "integer")
        expect_is(f_update_results$det_q, "numeric")
        expect_is(f_update_results$w_siam_full, "matrix")
        expect_is(f_update_results$w_post_full, "list")
        expect_is(f_update_results$w_star, "matrix")
        expect_is(f_update_results$w_star_eigen, "eigen")
        expect_is(f_update_results$w_star_eigen_vals, "numeric")
    })

    it("Should return kim's output",{
        expect_equal(f_update_results$w_sel_samples, output_update_f$w_sel_samples)
        expect_equal(f_update_results$det_q, output_update_f$det_q)
        expect_equal(f_update_results$w_siam_full, output_update_f$w_siam_full)

        W.post.full2 <- output_update_f$w_post_full
        W.post.full2$n.rows <- output_update_f$w_post_full$n
        W.post.full2$n <- NULL

        expect_equal(f_update_results$w_post_full, W.post.full2)
        expect_equal(f_update_results$w_star, output_update_f$w_star)
        expect_equal(f_update_results$w_star_eigen$values, output_update_f$w_star_eigen$values)
        # expect_equal(f_update_results$w_star_eigen$vectors, output_update_f$w_star_eigen$vectors)
        expect_equal(f_update_results$w_star_eigen_vals, output_update_f$w_star_eigen_vals)
    })

})