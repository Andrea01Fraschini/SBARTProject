setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject")
library(testthat)
source("R/library_imports.R")

describe("Test sample data function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("__tests__/R/SBART/kim_output/output_init_model_parameters.R")

    set.seed(1)
    data <- sample_data()
    
    set.seed(1)
    results <- init_model_parameters(
        X = data$Xpred,
        Y = data$Y,
        SIAM = data$wind_mat,
        n.trees = 50L
    )

    it("should return a list",{
        expect_is(results, "list")
    })

    it("should return a list with 28 elements",{
        expect_equal(length(results), 28)
    })

    it("should return a list with the correct names",{
        expect_equal(names(results), c("p", "n", "n.locations.all", "prob.grow", "prob.prune", "prob.change", "alpha", "beta", 
                      "dirichlet.alpha", "posterior.dirichlet.alpha", "cov.sel_prob", "tau2.a", "tau2.b", 
                      "tau2.posterior.shape", "tau2", "proposal.sd.rho", "rho", "a0", "b0", 
                      "Y", "residuals", "sigma2", "nu", "lambda", "sigma2.a", "sigma2.b", 
                       "sigma_mu", "missing_indexes"))
    })

    it("should return a list with the correct class",{
        expect_is(results$p, "integer")
        expect_is(results$n, "integer")
        expect_is(results$n.locations.all, "integer")
        expect_is(results$prob.grow, "numeric")
        expect_is(results$prob.prune, "numeric")
        expect_is(results$prob.change, "numeric")
        expect_is(results$alpha, "numeric")
        expect_is(results$beta, "numeric")
        expect_is(results$dirichlet.alpha, "numeric")
        expect_is(results$posterior.dirichlet.alpha, "numeric")
        expect_is(results$cov.sel_prob, "array")
        expect_is(results$tau2.a, "numeric")
        expect_is(results$tau2.b, "numeric")
        expect_is(results$tau2.posterior.shape, "numeric")
        expect_is(results$tau2, "numeric")
        expect_is(results$proposal.sd.rho, "numeric")
        expect_is(results$rho, "numeric")
        expect_is(results$a0, "numeric")
        expect_is(results$b0, "numeric")
        expect_is(results$Y, "numeric")
        expect_is(results$residuals, "numeric")
        expect_is(results$sigma2, "numeric")
        expect_is(results$nu, "numeric")
        expect_is(results$lambda, "numeric")
        expect_is(results$sigma2.a, "numeric")
        expect_is(results$sigma2.b, "numeric")
        expect_is(results$sigma_mu, "numeric")
        expect_is(results$missing_indexes, "integer")
    })

    it("Should return Kim's results",{
        expect_equal(results$p, output_init_model_parameters$p)
        expect_equal(results$n, output_init_model_parameters$n)
        expect_equal(results$n.locations.all, output_init_model_parameters$n.locations.all)
        expect_equal(results$prob.grow, output_init_model_parameters$prob.grow)
        expect_equal(results$prob.prune, output_init_model_parameters$prob.prune)
        expect_equal(results$prob.change, output_init_model_parameters$prob.change)
        expect_equal(results$alpha, output_init_model_parameters$alpha)
        expect_equal(results$beta, output_init_model_parameters$beta)
        expect_equal(results$dirichlet.alpha, output_init_model_parameters$dirichlet.alpha)
        expect_equal(results$posterior.dirichlet.alpha, output_init_model_parameters$posterior.dirichlet.alpha)
        expect_equal(results$cov.sel_prob, output_init_model_parameters$cov.sel_prob)
        expect_equal(results$tau2.a, output_init_model_parameters$tau2.a)
        expect_equal(results$tau2.b, output_init_model_parameters$tau2.b)
        expect_equal(results$tau2.posterior.shape, output_init_model_parameters$tau2.posterior.shape)
        expect_equal(results$tau2, output_init_model_parameters$tau2)
        expect_equal(results$proposal.sd.rho, output_init_model_parameters$proposal.sd.rho)
        expect_equal(results$rho, output_init_model_parameters$rho)
        expect_equal(results$a0, output_init_model_parameters$a0)
        expect_equal(results$b0, output_init_model_parameters$b0)
        expect_equal(results$Y, output_init_model_parameters$Y)
        expect_equal(results$residuals, output_init_model_parameters$residuals)
        expect_equal(results$sigma2, output_init_model_parameters$sigma2)
        expect_equal(results$nu, output_init_model_parameters$nu)
        expect_equal(results$lambda, output_init_model_parameters$lambda)
        expect_equal(results$sigma2.a, output_init_model_parameters$sigma2.a)
        expect_equal(results$sigma2.b, output_init_model_parameters$sigma2.b)
        expect_equal(results$sigma_mu, output_init_model_parameters$sigma_mu)
        expect_equal(results$missing_indexes, output_init_model_parameters$missing_indexes)
    })

})