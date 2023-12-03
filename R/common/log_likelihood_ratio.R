#' Compute Log-Likelihood Ratio
#'
#' This function computes the log-likelihood ratio for a given set of parameters and residuals. 
#' It is used in the context of a spatial analysis model.
#'
#' @param sigma2 A numeric value representing the variance of the error term.
#' @param sigma_mu A numeric value representing the variance of the spatial random effect.
#' @param residuals A numeric vector representing the residuals of the model.
#' @param obs.left A numeric vector representing the indices of the observations in the left partition.
#' @param obs.right A numeric vector representing the indices of the observations in the right partition.
#'
#' @return A numeric value representing the log-likelihood ratio.
#'
#' @examples
#' residuals <- rnorm(100)
#' obs.left <- 1:50
#' obs.right <- 51:100
#' sigma2 <- 1
#' sigma_mu <- 0.5
#' log_lh_ratio = log_likelihood_ratio(sigma2, sigma_mu, residuals, obs.left, obs.right)
#' @export
#'
log_likelihood_ratio <- function(sigma2, sigma_mu, residuals, obs.left, obs.right) {
    
    n.left <- length(obs.left)
    n.right <- length(obs.right)

    # temp variables for computing likelihood
    term.all <- sigma2 + (n.left + n.right) * sigma_mu
    term.left <- sigma2 + n.left * sigma_mu
    term.right <- sigma2 + n.right * sigma_mu
    res_sum.all <- sum(residuals[union(obs.left, obs.right)])
    res_sum.left <- sum(residuals[obs.left])
    res_sum.right <- sum(residuals[obs.right])

    log_lh_ratio <- log(sqrt(sigma2 * term.all) / sqrt(term.left * term.right)) + (sigma_mu / (2 * sigma2) * (res_sum.left^2 / term.left + res_sum.right^2 / term.right) - res_sum.all^2 / term.all)
    return(log_lh_ratio)
}