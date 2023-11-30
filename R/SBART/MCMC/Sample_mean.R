#' Sample mean
#'
#' This function performs a Gibbs step to sample M_t.
#' The formula used is: M_t ∼ [M_t | T_t, R_{i,(-t)},...,R_{n,(-t)}, σ^2]
#'
#' @param sigma2 A numeric value representing the variance of the residuals.
#' @param sigma_mu A numeric value representing the variance of the prior on the leaf parameters.
#' @param dt A list representing the current tree.
#' @param obs A numeric vector of observations.
#' @param residuals A numeric vector of residuals.
#' @param xcut A numeric vector of cutpoints for the predictor variables.
#' @param n.available A numeric value representing the number of available observations.
#'
#' @return A list with two elements: 'T' representing the updated tree structure, and 'dt' representing the updated observations indexes.
#' @export
#'
sample_mean <- function(sigma2, sigma_mu, dt, obs, residuals, xcut, n.available) {
    n <- n.available
    T <- rep(0, n) # assignment of obs through trees  

    terminal_nodes <- which(dt$terminal)
    
    # sample mean for leaf nodes 
    for (i in 1:length(terminal_nodes)) {
        obs.ind <- obs[(dt$begin[terminal_nodes[i]]):(dt$end[terminal_nodes[i]])]
        var <- 1 / (1 / sigma_mu + length(obs.ind) / sigma2)
        mean <- var * (sum(residuals[obs.ind]) / sigma2)

        T[obs.ind] <- dt$mu[terminal_nodes[i]] <- rnorm(1, mean, sqrt(var))
    }

    return(list(T = T, dt = dt))
}