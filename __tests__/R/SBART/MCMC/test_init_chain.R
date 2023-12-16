source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test init_chain function",{
    source("R/SBART/MCMC/init_chain.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_sample_data_2.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")

    set.seed(1)
    vars <- init_chain(
        n_iterations = 10000L,
        n_locations_all = output_init_model_parameters$n_locations_all,
        p = output_init_model_parameters$p,
        n_trees = 50L,
        n = output_init_model_parameters$n,
        x = output_sample_data_1$x_pred,
        y = output_init_model_parameters$y,
        missing_indexes = output_sample_data_1$missing_indexes,
        siam = output_sample_data_1$wind_matrix,
        ws = output_sample_data_2$ws,
        rho = output_init_model_parameters$rho
    )

    it("should return a list",{
        expect_is(vars, "list")
    })

    it("should return a list with 26 elements",{
        expect_equal(length(vars), 26)
    })

    it("should return a list with the correct names",{
        expect_equal(names(vars), c(
            "sigma2_samples",
            "rho_samples",
            "tau2_samples",
            "spatial_theta",
            "cov_sel",
            "obs_list_ind",
            "dt_list",
            "trees",
            "trees_pred",
            "x_list",
            "x_mult",
            "x_unique",
            "w_sel",
            "w_sel_samples",
            "w_count",
            "w_siam",
            "w_siam_full",
            "w_post",
            "w_post_full",
            "w_star",
            "w_star_eigen",
            "w_star_eigen_vals",
            "det_q",
            "y",
            "missing_indexes",
            "y_da"
        ))
    })

    it("should return a list with the correct class",{
        expect_equal(class(vars), "list")
    })

    it("should return kim's results",{
        expect_equal(vars$sigma2_samples, output_init_chain$sigma2_samples)
        expect_equal(vars$rho_samples, output_init_chain$rho_samples)
        expect_equal(vars$tau2_samples, output_init_chain$tau2_samples)
        expect_equal(vars$spatial_theta, output_init_chain$spatial_theta)
        expect_equal(vars$cov_sel, output_init_chain$cov_sel)
        expect_equal(vars$obs_list_ind, output_init_chain$obs_list_ind)

        for(i in seq_along(vars$dt_list)) {
            dt_list1 <- vars$dt_list[[i]]
            dt_list2 <- output_init_chain$dt_list[[i]]
        
            dt_list2$terminal <- ifelse(dt_list2$Terminal == 1, TRUE, FALSE)
            dt_list2$split <- dt_list2$Split
            dt_list2$value <- dt_list2$Value
            dt_list2$mu <- dt_list2$MU

            expect_equal(dt_list1$position, dt_list2$position)
            expect_equal(dt_list1$parent, dt_list2$parent)
            expect_equal(dt_list1$terminal, dt_list2$terminal)
            expect_equal(dt_list1$split, dt_list2$split)
            expect_equal(dt_list1$value, dt_list2$value)
            expect_equal(dt_list1$mu, dt_list2$mu)
            expect_equal(dt_list1$begin, dt_list2$begin)
            expect_equal(dt_list1$end, dt_list2$end)
        }
        
        expect_equal(vars$trees, output_init_chain$trees)
        expect_equal(vars$trees_pred, output_init_chain$trees_pred)
        expect_equal(vars$x_list, output_init_chain$x_list)
        expect_equal(vars$x_mult, output_init_chain$x_mult)
        expect_equal(vars$x_unique, output_init_chain$x_unique)
        expect_equal(vars$w_sel, output_init_chain$w_sel)
        expect_equal(vars$w_sel_samples, output_init_chain$w_sel_samples)
        expect_equal(vars$w_count, output_init_chain$w_count)
        expect_equal(vars$w_siam, output_init_chain$w_siam)
        expect_equal(vars$w_siam_full, output_init_chain$w_siam_full)

        expect_equal(vars$w_post$n.rows, output_init_chain$w_post$n)
        W_post1 <- vars$w_post
        W_post2 <- output_init_chain$w_post
        W_post1$n <- NULL
        W_post2$n <- NULL
        W_post1$n.rows <- NULL
        W_post2$n.rows <- NULL
        expect_equal(W_post1, W_post2)

        expect_equal(vars$w_post_full$n.rows, output_init_chain$w_post_full$n)
        W_post_full1 <- vars$w_post_full
        W_post_full2 <- output_init_chain$w_post_full
        W_post_full1$n <- NULL
        W_post_full2$n <- NULL
        W_post_full1$n.rows <- NULL
        W_post_full2$n.rows <- NULL
        expect_equal(W_post_full1, W_post_full2)

        expect_equal(vars$w_star, output_init_chain$w_star)
        expect_equal(vars$w_star_eigen$values, output_init_chain$w_star_eigen$values)
        # expect_equal(vars$w_star_eigen$vectors, output_init_chain$w_star_eigen$vectors)
        expect_equal(vars$w_star_eigen_vals, output_init_chain$w_star_eigen_vals)
        expect_equal(vars$det_q, output_init_chain$det_q)
        expect_equal(vars$y, output_init_chain$y)
        expect_equal(vars$missing_indexes, output_init_chain$missing_indexes)
        expect_equal(vars$y_da, output_init_chain$y_da)
    })

})