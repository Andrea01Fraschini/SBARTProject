#' Sample variance
#'
#' This function performs a Gibbs step to sample σ^2.
#' The formula used is: σ^2 ∼ IG(a_σ + n/2, b_σ + 1/2(∑_{i=1}^n R_{final,i})) where R_{final,i} = y_i - ∑^T_{t=1} g(x_i; T_t, M_t) - θ_i
#'
#' @param y A numeric vector of target values.
#' @param trees A matrix where each column represents a tree.
#' @param spatial_theta A numeric vector of spatial parameters.
#' @param missing_indexes A numeric vector of indexes where data is missing.
#' @param sigma2_a A numeric value representing the shape parameter of the inverse gamma prior on σ^2.
#' @param sigma2_b A numeric value representing the scale parameter of the inverse gamma prior on σ^2.
#' @param n A numeric value representing the number of observations.
#' @param sigma2_samples A numeric vector of samples for σ^2.
#' @param j The current iteration of the Markov chain.
#'
#' @return A numeric value representing a sample from the posterior distribution of σ^2.
#' @export
#'
sample_variance <- function(y, trees, spatial_theta, missing_indexes, sigma2_a, sigma2_b, n, sigma2_samples, j) {
  
  r_final <- y[-missing_indexes] - rowSums(trees) - spatial_theta[-missing_indexes]
  sigma2_samples[j] <- MCMCpack::rinvgamma(1, sigma2_a + n / 2, scale = sigma2_b + sum((r_final) ^ 2) / 2)

  return(sigma2_samples)
}