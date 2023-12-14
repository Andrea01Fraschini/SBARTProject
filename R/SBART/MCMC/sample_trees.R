#' Sample Decision Trees
#'
#' This function samples decision trees in the Markov chain. It applies a perturbation (grow, prune, or change) to each tree in the list of decision trees.
#'
#' @param dt_list A list of decision trees.
#' @param prob_grow The probability of growing the tree.
#' @param prob_change The probability of changing the tree.
#' @param prob_prune The probability of pruning the tree.
#' @param sigma2_samples A vector of samples for the error variance.
#' @param j The current iteration of the Markov chain.
#' @param sigma_mu The variance of the leaf parameters (mu_ij).
#' @param t The index of the current tree.
#' @param residuals A vector of residuals (R).
#' @param cov_sel_prob A vector of proposal probabilities for covariates (from Dirichlet).
#' @param obs_list_ind A list of vectors, each containing the indices of observations for each tree.
#' @param x_list A list of vectors, each containing the values of a covariate for non-missing observations.
#' @param x_unique A list of vectors, each containing the unique values of a covariate for non-missing observations.
#' @param n The number of available observations.
#' @param alpha The depth regularization parameter.
#' @param beta The regularization parameter.
#'
#' @return A list containing the following elements:
#'   - dt_list: The updated list of decision trees.
#'   - obs_list_ind: The updated list of observation indices for each tree.
#'
#' @examples
#' # Assuming all the required parameters are defined...
#' result = sample_trees(dt_list, prob_grow, prob_change, prob_prune, sigma2_samples, j, sigma_mu, t, residuals, cov_sel_prob, obs_list_ind, x_list, x_unique, n, alpha, beta)
#' @export
#'
sample_trees <- function(dt_list, prob_grow, prob_change, prob_prune, sigma2_samples, j, sigma_mu, t, residuals, cov_sel_prob, obs_list_ind, x_list, x_unique, n, alpha, beta) {
  source("R/SBART/MCMC/perturbations/GROW.R")
  source("R/SBART/MCMC/perturbations/PRUNE.R")
  source("R/SBART/MCMC/perturbations/CHANGE.R")

  # Find depth of the tree 
  tree_depth <- length(dt_list[[t]]$position)

  step <- ifelse(tree_depth == 1, # check if root node, if so, only grow perturbation is possible
            1, # GROW.root 
            sample(2:4, 1, prob = c(prob_grow, prob_prune, prob_change)) # Pick a perturbation 
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
            sigma2 = sigma2_samples[j-1],
            sigma_mu = sigma_mu,
            dt = dt_list[[t]],
            residuals = residuals,
            prop.prob = cov_sel_prob,
            obs = obs_list_ind[[t]],
            x.list = x_list,
            xcut = x_unique,
            n.available = n,
            prob.grow = prob_grow,
            prob.change = prob_change,
            prob.prune = prob_prune,
            alpha = alpha,
            beta = beta
        )

  dt_list[[t]] <- result$dt # update tree structure
  obs_list_ind[[t]] <- result$obs # update observations indexes
  
  return(
    list(
      dt_list = dt_list, 
      obs_list_ind = obs_list_ind
    )
  )
}