source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test PRUNE function",{
    source("__tests__/scripts/transform_dt_list.R")
    source("R/common/log_likelihood_ratio.R")
    source("R/SBART/MCMC/perturbations/PRUNE.R")
    source("output/KIM/input_prune.R")
    source("output/KIM/output_prune.R")

    dt_list_transformed <- transform_dt_list(input_prune$dt_list)

    set.seed(1)
    result <- PRUNE(
            sigma2 = input_prune$sigma2_samples[input_prune$j-1],
            sigma_mu = input_prune$sigma_mu,
            dt = dt_list_transformed[[input_prune$t]],
            residuals = input_prune$residuals,
            prop.prob = input_prune$cov_sel_prob,
            obs = input_prune$obs_list_ind[[input_prune$t]],
            x.list = input_prune$x_list,
            xcut = input_prune$x_unique,
            n.available = input_prune$n,
            prob.grow = input_prune$prob_grow,
            prob.change = input_prune$prob_change,
            prob.prune = input_prune$prob_prune,
            alpha = input_prune$alpha,
            beta = input_prune$beta
        )
    

    it("should return a list",{
        expect_is(result, "list")
    })

    it("should return a list with 2 elements",{
        expect_equal(length(result), 2)
    })

    it("should return a list with the correct names",{
        expect_equal(names(result), c("dt", "obs"))
    })

    it("should return a list with the correct class",{
        expect_is(result$dt, "list")
        expect_is(result$obs, "integer")
    })

    it("should return Kim's results, for firt iteration, first tree",{

        dt_1 <- result$dt
        dt_2 <- output_prune$dt_list[[input_prune$t]]
    
        dt_2$terminal <- ifelse(dt_2$Terminal == 1, TRUE, FALSE)
        dt_2$split <- dt_2$Split
        dt_2$value <- dt_2$Value
        dt_2$mu <- dt_2$MU

        expect_equal(dt_1$position, dt_2$position)
        expect_equal(dt_1$parent, dt_2$parent)
        expect_equal(dt_1$terminal, dt_2$terminal)
        expect_equal(dt_1$split, dt_2$split)
        expect_equal(dt_1$value, dt_2$value)
        expect_equal(dt_1$mu, dt_2$mu)
        expect_equal(dt_1$begin, dt_2$begin)
        expect_equal(dt_1$end, dt_2$end)

        expect_equal(result$obs, output_prune$obs_list_ind[[input_prune$t]])
    })
})