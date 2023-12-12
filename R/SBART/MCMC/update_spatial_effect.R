#' Update spatial effect
#'
#' This function performs a Gibbs step to update the spatial effect θ.
#' The formula used is: 
#' Short version:
#' θ_i ∼ N(...,...)
#'
#' Long version:
#' θ_i ∼  N( ((ρ (∑^n_(k=1) w_ikθ_k) / τ^2) + e_i/ σ^2) / (( 1 / τ^2 ( ρ (∑_^n{k=1} w_ik) + 1 - ρ )^-1) + 1/σ^2),  1/(( 1 / τ^2 ( ρ (∑_^n{k=1} w_ik) + 1 - ρ )^-1) + 1/σ^2))
#'
#' @param y A numeric vector of target values.
#' @param trees_pred A matrix where each column represents a predicted tree.
#' @param W.post.full A list containing the structure of the spatial weights matrix.
#' @param n_locations_all A numeric value representing the total number of locations.
#' @param spatial_theta A numeric vector of spatial parameters.
#' @param tau2 A numeric value representing the variance of the spatial effect.
#' @param rho A numeric value representing the spatial correlation parameter.
#' @param sigma2_samples A numeric vector of variance samples.
#'
#' @return A numeric vector representing the updated spatial effect.
#' @export
#'
update_spatial_effect <- function(Y, trees_pred, W.post.full, n_locations_all, spatial_theta, tau2, rho, sigma2_samples, j) {
  sourceCpp("src/CARBayes.cpp")

  offset <- (Y - rowSums(trees_pred))
  spatial_theta <- gaussiancarupdate(
            Wtriplet = W.post.full$W.triplet,
            Wbegfin = W.post.full$W.begfin,
            W.post.full$W.triplet.sum,
            nsites = n_locations_all,
            phi = spatial_theta,
            tau2 = tau2,
            rho = rho,
            nu2 = sigma2_samples[j],
            offset = offset
        )
  spatial_theta <- spatial_theta - mean(spatial_theta)
  
  return(spatial_theta)
}