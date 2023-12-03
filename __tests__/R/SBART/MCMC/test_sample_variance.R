rm(list = ls())
setwd(".")
library(testthat)
source("R/library_imports.R")

describe("Test sample_variance function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("R/SBART/MCMC/update_residuals.R")
    source("R/SBART/MCMC/sample_trees.R")
    source("R/SBART/MCMC/sample_means.R")
    source("R/SBART/MCMC/sample_variance.R")
    source("__tests__/R/SBART/MCMC/kim_output/output_sample_variance.R")

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

    trees <- vars$trees
    dt_list <- vars$dt_list
    obs_list.ind <- vars$obs_list.ind

    for(t in 1:50){

        residuals <- update_residuals(
            Y = vars$Y, 
            trees = trees, 
            t = t,
            spatial_theta = vars$spatial_theta,
            missing_indexes = vars$missing_indexes
        ) # residuals

        set.seed(1)
        trees_output <- sample_trees(
            dt_list = dt_list,
            prob.grow = params$prob.grow,
            prob.change = params$prob.change,
            prob.prune = params$prob.prune,
            sigma2.samples = vars$sigma2.samples,
            j = 2,
            sigma_mu = params$sigma_mu,
            t = t,
            residuals = residuals,
            cov.sel_prob = params$cov.sel_prob,
            obs_list.ind = obs_list.ind,
            Xlist = vars$Xlist,
            X.unique = vars$X.unique,
            n = params$n,
            alpha = params$alpha,
            beta = params$beta
        ) # list(dt_list = dt_list, obs_list.ind = obs_list.ind)

        dt_list <- trees_output$dt_list
        obs_list.ind <- trees_output$obs_list.ind

        set.seed(1)
        means <- sample_means(
            sigma2 = vars$sigma2.samples,
            sigma_mu = params$sigma_mu,
            obs = obs_list.ind,
            residuals = residuals,
            xcut = vars$X.unique,
            n.available = params$n,
            trees = trees,
            dt_list = dt_list,
            t = t,
            j = 2
        ) # list(trees = trees, dt_list = dt_list)

        trees <- means$trees
        dt_list <- means$dt_list

    }

    set.seed(1)
    sigma2.samples <- sample_variance(
        Y = vars$Y,
        trees = means$trees,
        spatial_theta = vars$spatial_theta,
        missing_indexes = vars$missing_indexes,
        sigma2.a = params$sigma2.a,
        sigma2.b = params$sigma2.b,
        n = params$n,
        sigma2.samples = vars$sigma2.samples,
        j = 2
    )

    it("should return a numeric value representing a sample from the posterior distribution of σ^2",{
        expect_is(sigma2.samples, "numeric")
    })

    it("should return a value equal to the one in the kim_output file",{
        expect_equal(sigma2.samples, output_sample_variance$sigma2.samples)
    })

})