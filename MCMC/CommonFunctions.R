# likelihood ratio in log scale used for GROW and PRUNE
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