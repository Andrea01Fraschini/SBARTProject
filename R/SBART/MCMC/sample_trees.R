#' Sample tree
#'
#' This function performs a Metropolis Hastings step to sample T_t.
#' The formula used is: T_t ∼ [T_t | R_{i,(-t)},...,R_{n,(-t)}, σ^2]
#'

sample_trees <- function(dt_list, prob.grow, prob.change, prob.prune, sigma2.samples, j, sigma_mu, t, residuals, cov.sel_prob, obs_list.ind, Xlist, X.unique, n, alpha, beta) {
  source("R/SBART/MCMC/perturbations/GROW.R")
  source("R/SBART/MCMC/perturbations/PRUNE.R")
  source("R/SBART/MCMC/perturbations/CHANGE.R")

  # Find depth of the tree 
  tree.depth <- length(dt_list[[t]]$position)
  step <- ifelse(tree.depth == 1, # check if root node, if so, only grow perturbation is possible
            1, # GROW.root 
            sample(2:4, 1, prob = c(prob.grow, prob.prune, prob.change)) # Pick a perturbation 
        )

  perturbation <- switch(
            step, # selection var 
            GROW.root,
            GROW,
            PRUNE,
            CHANGE,
        )

  # Apply perturbation to trees 
  result <- perturbation(
            sigma2 = sigma2.samples[j-1],
            sigma_mu = sigma_mu,
            dt = dt_list[[t]],
            residuals = residuals,
            prop.prob = cov.sel_prob,
            obs = obs_list.ind[[t]],
            x.list = Xlist,
            xcut = X.unique,
            n.available = n,
            prob.grow = prob.grow,
            prob.change = prob.change,
            prob.prune = prob.prune,
            alpha = alpha,
            beta = beta
        )

  dt_list[[t]] <- result$dt # update tree structure
  obs_list.ind[[t]] <- result$obs # update observations indexes
  
  return(
    list(
      dt_list = dt_list, 
      obs_list.ind = obs_list.ind
    )
  )
}