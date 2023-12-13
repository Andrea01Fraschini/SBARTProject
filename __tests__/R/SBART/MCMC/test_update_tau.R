source('../../../env_setup_tests.R')
# setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject") 

source("R/library_imports.R")
library(testthat)

describe("Test sample_variance function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("R/SBART/MCMC/update_residuals.R")
    source("R/SBART/MCMC/sample_trees.R")
    source("R/SBART/MCMC/sample_means.R")
    source("R/SBART/MCMC/sample_variance.R")
    source("R/SBART/MCMC/update_spatial_effect.R")
    source("R/SBART/MCMC/update_tau.R")
    source("output/KIM/output_update_tau.R")

    set.seed(1)
    data <- sample_data() # Xpred, Y, mis.ind, Ws, wind_mat
    
    set.seed(1)
    params <- init_model_parameters(
        X = data$Xpred,
        Y = data$Y,
        SIAM = data$wind_mat,
        n.trees = 50L
    ) # p, n, n.locations.all, prob.grow, prob.prune, prob.change, alpha, beta, dirichlet.alpha, posterior.dirichlet.alpha, cov.sel_prob, tau2.a, tau2.b, tau2.posterior.shape, tau2, proposal.sd.rho, rho, a0, b0, Y, residuals, sigm2, nu, lambda, sigma2.a, sigma2.b, sigma_mu, missing_indexes

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
    ) # sigma2.samples, rho.samples, tau2.samples, spatial_theta, cov_sel, obs_list.ind, dt_list, trees, trees.pred, Xlist, Xmult, X.unique, W_sel, W_sel.samples, W.count, W.siam, W.siam.full, W.post, W.post.full, Wstar, Wstar.eigen, Wstar.eigen_vals, det.Q, Y, missing_indexes, Y.da

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
        ) # dt_list, obs_list.ind

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
        ) # trees, dt_list

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
    ) # sigma2.samples

    spatial_theta <- update_spatial_effect(
        Y = vars$Y,
        trees_pred = vars$trees.pred,
        W.post.full = vars$W.post.full,
        n_locations_all = params$n.locations.all,
        spatial_theta = vars$spatial_theta,
        tau2 = params$tau2,
        rho = params$rho,
        sigma2_samples = sigma2.samples,
        j = 2
    ) # spatial_theta

    update_tau2_results <- update_tau2(
        W.post.full = vars$W.post.full,
        n_locations_all = params$n.locations.all,
        spatial_theta = spatial_theta,
        rho = params$rho,
        tau2_b = params$tau2.b,
        tau2_posterior_shape = params$tau2.posterior.shape,
        tau2.samples = vars$tau2.samples,
        j = 2
    ) # tau2.samples, temp

    it("should return a list",{
        expect_is(update_tau2_results, "list")
    })

    it("should resutrn a list with two elements",{
        expect_equal(length(update_tau2_results), 2)
    })
    
    it("should return a the correct classes",{
        expect_is(update_tau2_results$tau2.samples, "numeric")
        expect_is(update_tau2_results$temp, "numeric")
    })

    it("should return kim's output",{
        expect_equal(update_tau2_results$tau2.samples, output_update_tau$tau2.samples)
        expect_equal(update_tau2_results$temp, output_update_tau$temp)
    })
    

})