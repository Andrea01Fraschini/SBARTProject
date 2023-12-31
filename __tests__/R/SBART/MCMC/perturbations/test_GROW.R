# source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test GROW function",{
    source("__tests__/scripts/transform_dt_list.R")
    source("R/common/log_likelihood_ratio.R")
    source("R/SBART/MCMC/perturbations/GROW.R")
    source("output/KIM/input_grow.R")
    source("output/KIM/output_grow.R")

    dt_list_transformed <- transform_dt_list(input_grow$dt_list)

    set.seed(1)
    result <- GROW(
            sigma2 = input_grow$sigma2_samples[input_grow$j-1],
            sigma_mu = input_grow$sigma_mu,
            dt = dt_list_transformed[[input_grow$t]],
            residuals = input_grow$residuals,
            prop.prob = input_grow$cov_sel_prob,
            obs = input_grow$obs_list_ind[[input_grow$t]],
            x.list = input_grow$x_list,
            xcut = input_grow$x_unique,
            n.available = input_grow$n,
            prob.grow = input_grow$prob_grow,
            prob.change = input_grow$prob_change,
            prob.prune = input_grow$prob_prune,
            alpha = input_grow$alpha,
            beta = input_grow$beta
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
        dt_2 <- output_grow$dt_list[[input_grow$t]]
    
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

        expect_equal(result$obs, output_grow$obs_list_ind[[input_grow$t]])
    })
})