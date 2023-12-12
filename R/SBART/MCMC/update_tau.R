#'  TODO: Update documentation
#'  Update tau
#'
#' This function performs a Gibbs step to update the variance of the spatial effect τ^2.
#' The formula used is: τ^2 ∼ IG( α_τ + n/2, β_τ + 1/2((∑^n_{i=1} θ^2_i ( ρ ∑_{k=1}^n w_ik + 1 - ρ)) ρ ( (∑^n_{i=1} (∑^n_{k=1} θ_i θ_k w_ik )))))
#'
#' @param W_triplet A list containing the structure of the spatial weights matrix.
#' @param W_triplet_sum A numeric value representing the sum of the spatial weights.
#' @param n_triplet A numeric value representing the number of triplets in the spatial weights matrix.
#' @param n_locations_all A numeric value representing the total number of locations.
#' @param spatial_theta A numeric vector of spatial parameters.
#' @param rho A numeric value representing the spatial correlation parameter.
#' @param tau2_b A numeric value representing the scale parameter of the inverse gamma prior on τ^2.
#' @param tau2_posterior_shape A numeric value representing the shape parameter of the posterior distribution of τ^2.
#'
#' @return A numeric value representing a sample from the posterior distribution of τ^2.
#' @export
#'
update_tau2 <- function(W.post.full, n_locations_all, spatial_theta, rho, tau2_b, tau2_posterior_shape, tau2.samples, j) {

  temp <- quadform(
            as.matrix(W.post.full$W.triplet),
            W.post.full$W.triplet.sum,
            W.post.full$n.triplet,
            n_locations_all,
            spatial_theta,
            spatial_theta,
            rho
        )
  tau2_posterior_scale <- temp + tau2_b 
  tau2 <- 1 / rgamma(1, tau2_posterior_shape, scale = (1 / tau2_posterior_scale))
  
  tau2.samples[j] <- tau2

  return(list(tau2.samples = tau2.samples, temp = temp))
}