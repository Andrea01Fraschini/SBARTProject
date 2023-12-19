source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test CHANGE function",{
    source("__tests__/scripts/transform_dt_list.R")
    source("R/SBART/MCMC/perturbations/CHANGE.R")
    source("output/KIM/input_change.R")
    source("output/KIM/output_change.R")

    dt_list_transformed <- transform_dt_list(input_change$dt_list)

    set.seed(1)
    result <- CHANGE(
            sigma2 = input_change$sigma2_samples[input_change$j-1],
            sigma_mu = input_change$sigma_mu,
            dt = dt_list_transformed[[input_change$t]],
            residuals = input_change$residuals,
            prop.prob = input_change$cov_sel_prob,
            obs = input_change$obs_list_ind[[input_change$t]],
            x.list = input_change$x_list,
            xcut = input_change$x_unique,
            n.available = input_change$n,
            prob.grow = input_change$prob_grow,
            prob.change = input_change$prob_change,
            prob.prune = input_change$prob_prune,
            alpha = input_change$alpha,
            beta = input_change$beta
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
        dt_2 <- output_change$dt_list[[input_change$t]]
    
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

        expect_equal(result$obs, output_change$obs_list_ind[[input_change$t]])
    })
})