#' Sample tree
#'
#' This function performs a Metropolis Hastings step to sample T_t.
#' The formula used is: T_t ∼ [T_t | R_{i,(-t)},...,R_{n,(-t)}, σ^2]
#'
#' @param sigma2 A numeric value representing the variance of the residuals.
#' @param sigma_mu A numeric value representing the variance of the prior on the leaf parameters.
#' @param dt A list representing the current tree.
#' @param residuals A numeric vector of residuals.
#' @param prop.prob A numeric value representing the proposal probability.
#' @param obs A numeric vector of observations.
#' @param x.list A list of predictor variables.
#' @param xcut A numeric vector of cutpoints for the predictor variables.
#' @param n.available A numeric value representing the number of available observations.
#' @param prob.grow A numeric value representing the probability of a grow step.
#' @param prob.change A numeric value representing the probability of a change step.
#' @param prob.prune A numeric value representing the probability of a prune step.
#' @param alpha A numeric value representing the alpha parameter of the prior on the tree depth.
#' @param beta A numeric value representing the beta parameter of the prior on the tree depth.
#'
#' @return A list with two elements: 'dt' representing the updated tree structure, and 'obs' representing the updated observations indexes.
#' @export
#'
sample_trees <- function(sigma2, sigma_mu, dt, residuals, prop.prob, obs, x.list, xcut, n.available, prob.grow, prob.change, prob.prune, alpha, beta) {

  # Find depth of the tree 
  tree.depth <- length(dt$position)
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
            sigma2 = sigma2,
            sigma_mu = sigma_mu,
            dt = dt,
            residuals = residuals,
            prop.prob = prop.prob,
            obs = obs,
            x.list = x.list,
            xcut = xcut,
            n.available = n.available,
            prob.grow = prob.grow,
            prob.change = prob.change,
            prob.prune = prob.prune,
            alpha = alpha,
            beta = beta
        )

  dt <- result$dt # update tree structure
  obs <- result$obs # update observations indexes
  
  return(list(dt = dt, obs = obs))
}