#' Update rho
#'
#' This function performs a Metropolis Hastings step to update the spatial correlation parameter ρ.
#'
#' @param rho A numeric value representing the current spatial correlation parameter.
#' @param proposal_sd_rho A numeric value representing the standard deviation of the proposal distribution for ρ.
#' @param W_post_full A list containing the structure of the spatial weights matrix.
#' @param n_locations_all A numeric value representing the total number of locations.
#' @param spatial_theta A numeric vector of spatial parameters.
#' @param tau2 A numeric value representing the variance of the spatial effect.
#' @param Wstar_eigen_vals A numeric vector of eigenvalues of the standardized spatial weights matrix.
#' @param det_Q A numeric value representing the determinant of the precision matrix of the spatial effect.
#' @param temp A numeric value used in the calculation of the log-probability of the current state.
#'
#' @return A list with three elements: 'rho' representing the updated spatial correlation parameter, 'det_Q' representing the updated determinant of the precision matrix, and 'temp' representing the updated value used in the calculation of the log-probability.
#' @export
#'
update_rho <- function(rho, proposal_sd_rho, W_post_full, n_locations_all, spatial_theta, tau2, Wstar_eigen_vals, det_Q, temp, rho_samples, j) {

  proposal_rho <- rtruncnorm(n = 1, a = 0, b = 1, mean = rho, sd = proposal_sd_rho)
  temp2 <- quadform(
            as.matrix(W_post_full$W.triplet),
            W_post_full$W.triplet.sum,
            W_post_full$n.triplet,
            n_locations_all,
            spatial_theta,
            spatial_theta,
            proposal_rho
        )
  det_Q_proposal <- 0.5 * sum(log(proposal_rho * Wstar_eigen_vals + (1 - proposal_rho)))
  logprob_current <- det_Q - temp / tau2
  logprob_proposal <- det_Q_proposal - temp2 / tau2
  logprob_hastings <- log(dtruncnorm(x = rho, a = 0, b = 1, mean = proposal_rho, sd = proposal_sd_rho)) -
                            log(dtruncnorm(x = proposal_rho, a = 0, b = 1, mean = rho, sd = proposal_sd_rho))

  accept_prob <- exp(logprob_proposal - logprob_current + logprob_hastings)

  if (accept_prob > runif(1)) {
    rho <- proposal_rho
    det_Q <- det_Q_proposal
    temp <- temp2
  }

  rho_samples[j] <- rho
  
  return(list(rho.samples = rho_samples, det.Q = det_Q, temp = temp))
}