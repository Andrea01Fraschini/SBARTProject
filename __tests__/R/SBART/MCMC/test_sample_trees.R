source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_trees function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("R/SBART/MCMC/update_residuals.R")
    source("R/SBART/MCMC/sample_trees.R")
    source("output/KIM/output_sample_trees.R")

    set.seed(1)
    data <- sample_data()
    
    set.seed(1)
    params <- init_model_parameters(
        X = data$Xpred,
        Y = data$Y,
        SIAM = data$wind_mat,
        n.trees = 50L
    )

    set.seed(1)
    vars <- init_chain(
        n.iterations = 10000L,
        n.locations.all = params$n.locations.all,
        p = params$p,
        n = params$n,
        n.trees = 50L,
        X = data$Xpred,
        missing_indexes = data$mis.ind,
        SIAM = data$wind_mat,
        W = data$Ws,
        Y = params$Y,
        rho = params$rho
    )

    residuals <- update_residuals(
        Y = vars$Y, 
        vars$trees, 
        1,
        vars$spatial_theta,
        vars$missing_indexes
    )

    set.seed(1)
    result <- sample_trees(
        dt_list = vars$dt_list,
        prob.grow = params$prob.grow,
        prob.change = params$prob.change,
        prob.prune = params$prob.prune,
        sigma2.samples = vars$sigma2.samples,
        j = 2,
        sigma_mu = params$sigma_mu,
        t = 1,
        residuals = residuals,
        cov.sel_prob = params$cov.sel_prob,
        obs_list.ind = vars$obs_list.ind,
        Xlist = vars$Xlist,
        X.unique = vars$X.unique,
        n = params$n,
        alpha = params$alpha,
        beta = params$beta
    )

    it("should return a list",{
        expect_is(result, "list")
    })

    it("should return a list with 2 elements",{
        expect_equal(length(result), 2)
    })

    it("should return a list with the correct names",{
        expect_equal(names(result), c("dt_list", "obs_list.ind"))
    })

    it("should return a list with the correct class",{
        expect_is(result$dt_list, "list")
        expect_is(result$obs_list.ind, "list")
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