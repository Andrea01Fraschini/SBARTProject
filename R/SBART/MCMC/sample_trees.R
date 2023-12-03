#' Sample Decision Trees
#'
#' This function samples decision trees in the Markov chain. It applies a perturbation (grow, prune, or change) to each tree in the list of decision trees.
#'
#' @param dt_list A list of decision trees.
#' @param prob.grow The probability of growing the tree.
#' @param prob.change The probability of changing the tree.
#' @param prob.prune The probability of pruning the tree.
#' @param sigma2.samples A vector of samples for the error variance.
#' @param j The current iteration of the Markov chain.
#' @param sigma_mu The variance of the leaf parameters (mu_ij).
#' @param t The index of the current tree.
#' @param residuals A vector of residuals (R).
#' @param cov.sel_prob A vector of proposal probabilities for covariates (from Dirichlet).
#' @param obs_list.ind A list of vectors, each containing the indices of observations for each tree.
#' @param Xlist A list of vectors, each containing the values of a covariate for non-missing observations.
#' @param X.unique A list of vectors, each containing the unique values of a covariate for non-missing observations.
#' @param n The number of available observations.
#' @param alpha The depth regularization parameter.
#' @param beta The regularization parameter.
#'
#' @return A list containing the following elements:
#'   - dt_list: The updated list of decision trees.
#'   - obs_list.ind: The updated list of observation indices for each tree.
#'
#' @examples
#' # Assuming all the required parameters are defined...
#' result = sample_trees(dt_list, prob.grow, prob.change, prob.prune, sigma2.samples, j, sigma_mu, t, residuals, cov.sel_prob, obs_list.ind, Xlist, X.unique, n, alpha, beta)
#' @export
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