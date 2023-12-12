update_f <- function(W, W_count, SIAM, n_locations_all, spatial_theta, rho, tau2, det_Q, temp, W_sel, W_siam_full, W_post_full, Wstar, Wstar_eigen, Wstar_eigen_vals, W_sel.samples, j) {
  source("R/common/format_w_matrix.R")

  proposal_W_sel <- sample(1:W_count, 1)
  W_siam_proposal <- SIAM
  for (i in 1:W_count) {
    W_siam_proposal <- W_siam_proposal * W[[i]] ^ I(proposal_W_sel == i)
  }
  rownames(W_siam_proposal) <- 1:(n_locations_all)
  colnames(W_siam_proposal) <- 1:(n_locations_all)
  W_post_proposal <- format_w_matrix(W_siam_proposal)

  temp3 <- quadform(
          as.matrix(W_post_proposal$W_triplet),
          W_post_proposal$W_triplet.sum,
          W_post_proposal$n_triplet,
          n_locations_all,
          spatial_theta,
          spatial_theta,
          rho
      )

  Wstar_proposal <- diag(apply(W_siam_proposal, 1, sum)) - W_siam_proposal
  Wstar_eigen_proposal <- eigen(Wstar_proposal)
  Wstar_eigen_vals_proposal <- Wstar_eigen_proposal$values

  det_Q_proposal <- 0.5 * sum(log((rho * Wstar_eigen_vals_proposal + (1 - rho))))
  logprob_current <- det_Q - temp / tau2
  logprob_proposal <- det_Q_proposal - temp3 / tau2

  accept_prob <- exp(logprob_proposal - logprob_current)

  if (accept_prob > runif(1)) {
    W_sel <- proposal_W_sel
    det_Q <- det_Q_proposal
    W_siam_full <- W_siam_proposal
    W_post_full <- W_post_proposal
    Wstar <- Wstar_proposal
    Wstar_eigen <- Wstar_eigen_proposal
    Wstar_eigen_vals <- Wstar_eigen_vals_proposal
  }

  W_sel.samples[j] <- W_sel
  
  return(list(W_sel.samples = W_sel.samples, det.Q = det_Q, W.siam.full = W_siam_full, W.post.full = W_post_full, Wstar = Wstar, Wstar.eigen = Wstar_eigen, Wstar.eigen_vals = Wstar_eigen_vals))
}