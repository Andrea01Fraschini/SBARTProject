#' Sample Means for Leaf Nodes
#'
#' This function samples the means for the leaf nodes of a decision tree. 
#' It uses the residuals and the current tree structure to sample a new mean for each leaf node.
#'
#' @param sigma2_samples A vector of samples for the error variance.
#' @param sigma_mu The variance of the leaf parameters (mu_ij).
#' @param obs_list_ind A list of vectors, each containing the indices of observations for each tree.
#' @param residuals A vector of residuals (R).
#' @param x_cut A partition of the predictor space.
#' @param n_available The number of available observations.
#' @param trees A matrix of tree values.
#' @param dt_list A list of decision trees.
#' @param t The index of the current tree.
#' @param j The current iteration of the Markov chain.
#'
#' @return A list containing the following elements:
#'   - trees: The updated matrix of tree values.
#'   - dt_list: The updated list of decision trees.
#'
#' @examples
#' # Assuming all the required parameters are defined...
#' result = sample_means(sigma2_samples, sigma_mu, obs_list_ind, residuals, x_cut, n_available, trees, dt_list, t, j)
#' @export
#'
sample_means <- function(sigma2_samples, sigma_mu, obs_list_ind, residuals, x_cut, n_available, trees, dt_list, t, j) {
    sigma2 <- sigma2_samples[j-1] # sigma2 is a list of length 1
    obs <- obs_list_ind[[t]] # obs is a list of length 1
    n <- n_available
    T <- rep(0, n) # assignment of obs through trees  
    dt <- dt_list[[t]] # current tree

    terminal_nodes <- which(dt$terminal)
    
    # sample mean for leaf nodes 
    for (i in 1:length(terminal_nodes)) {
        obs.ind <- obs[(dt$begin[terminal_nodes[i]]):(dt$end[terminal_nodes[i]])]
        var <- 1 / (1 / sigma_mu + length(obs.ind) / sigma2)
        mean <- var * (sum(residuals[obs.ind]) / sigma2)

        T[obs.ind] <- dt$mu[terminal_nodes[i]] <- rnorm(1, mean, sqrt(var))
    }

    trees[,t] <- T
    dt_list[[t]] <- dt

    return(list(trees = trees, dt_list = dt_list))
}