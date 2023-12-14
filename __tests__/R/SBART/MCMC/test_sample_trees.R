source('../../../env_setup_tests.R')
  setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject") 

source("R/library_imports.R")
library(testthat)

describe("Test sample_trees function",{
    source("R/SBART/MCMC/sample_trees.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_residuals.R")
    source("output/KIM/output_sample_trees.R")

    dt_list_transformed <- lapply(output_init_chain$dt_list, function(dt) {
        dt$terminal <- ifelse(dt$Terminal == 1, TRUE, FALSE)
        dt$split <- dt$Split
        dt$value <- dt$Value
        dt$mu <- dt$MU

        dt$Terminal <- NULL
        dt$Split <- NULL
        dt$Value <- NULL
        dt$MU <- NULL

        temp <- dt$begin
        temp2 <- dt$end

        dt$begin <- NULL
        dt$end <- NULL

        dt$begin <- temp
        dt$end <- temp2
        
        return(dt)
    })

    set.seed(1)
    result <- sample_trees(
        dt_list = dt_list_transformed,
        prob_grow = output_init_model_parameters$prob_grow,
        prob_change = output_init_model_parameters$prob_change,
        prob_prune = output_init_model_parameters$prob_prune,
        sigma2_samples = output_init_chain$sigma2_samples,
        j = 2,
        sigma_mu = output_init_model_parameters$sigma_mu,
        t = 1,
        residuals = output_update_residuals$residuals,
        cov_sel_prob = output_init_model_parameters$cov_sel_prob,
        obs_list_ind = output_init_chain$obs_list_ind,
        x_list = output_init_chain$x_list,
        x_unique = output_init_chain$x_unique,
        n = output_init_model_parameters$n,
        alpha = output_init_model_parameters$alpha,
        beta = output_init_model_parameters$beta
    )

    it("should return a list",{
        expect_is(result, "list")
    })

    it("should return a list with 2 elements",{
        expect_equal(length(result), 2)
    })

    it("should return a list with the correct names",{
        expect_equal(names(result), c("dt_list", "obs_list_ind"))
    })

    it("should return a list with the correct class",{
        expect_is(result$dt_list, "list")
        expect_is(result$obs_list_ind, "list")
    })

    it("should return Kim's results, for firt iteration, first tree",{
        for(i in seq_along(result$dt_list)) {
            dt_list1 <- result$dt_list[[i]]
            dt_list2 <- output_sample_trees$dt_list[[i]]
        
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
        expect_equal(result$obs_list.ind, output_sample_trees$obs_list.ind)
    })
})