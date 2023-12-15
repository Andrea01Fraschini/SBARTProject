update_f <- function(ws, w_count, siam, n_locations_all, spatial_theta, rho, tau2, det_q, temp, w_sel, w_siam_full, w_post_full, w_star, w_star_eigen, w_star_eigen_vals, w_sel_samples, j) {
  source("R/common/format_w_matrix.R")
  sourceCpp("src/CARBayes.cpp")

  proposal_W_sel <- sample(1:w_count, 1)
  W_siam_proposal <- siam
  for (i in 1:w_count) {
    W_siam_proposal <- W_siam_proposal * ws[[i]] ^ I(proposal_W_sel == i)
  }
  rownames(W_siam_proposal) <- 1:(n_locations_all)
  colnames(W_siam_proposal) <- 1:(n_locations_all)
  W_post_proposal <- format_w_matrix(W_siam_proposal)

  temp3 <- quadform(
          as.matrix(W_post_proposal$W.triplet),
          W_post_proposal$W.triplet.sum,
          W_post_proposal$n.triplet,
          n_locations_all,
          spatial_theta,
          spatial_theta,
          rho
      )

  Wstar_proposal <- diag(apply(W_siam_proposal, 1, sum)) - W_siam_proposal
  Wstar_eigen_proposal <- eigen(Wstar_proposal)
  Wstar_eigen_vals_proposal <- Wstar_eigen_proposal$values

  det_Q_proposal <- 0.5 * sum(log((rho * Wstar_eigen_vals_proposal + (1 - rho))))
  logprob_current <- det_q - temp / tau2
  logprob_proposal <- det_Q_proposal - temp3 / tau2

  accept_prob <- exp(logprob_proposal - logprob_current)

  if (accept_prob > runif(1)) {
    w_sel <- proposal_W_sel
    det_q <- det_Q_proposal
    w_siam_full <- W_siam_proposal
    w_post_full <- W_post_proposal
    w_star <- Wstar_proposal
    w_star_eigen <- Wstar_eigen_proposal
    w_star_eigen_vals <- Wstar_eigen_vals_proposal
  }

  w_sel_samples[j] <- w_sel
  
  return(list(w_sel_samples = w_sel_samples, det_q = det_q, w_siam_full = w_siam_full, w_post_full = w_post_full, w_star = w_star, w_star_eigen = w_star_eigen, w_star_eigen_vals = w_star_eigen_vals))
}