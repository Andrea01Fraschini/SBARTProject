update_dirichlet_alpha <- function(dt_list, p, j, warmup, dirichlet_alpha, a0, b0, cov_sel_prob) {
  dt.split_vars <- unlist(lapply(dt_list, function(x) x$split))
  rules.count <- as.numeric(table(factor(dt.split_vars[!is.na(dt.split_vars)], levels = 1:p)))
  
  if (j < warmup) {
    posterior.dirichlet.alpha <- rep(1, p) + rules.count
  } else {
    proposal.dirichlet.alpha <- max(rnorm(1, dirichlet.alpha, 0.1), 0.1 ^ 10)
    sum_s <- log(ifelse(cov.sel_prob < 0.1 ^ 300, 0.1 ^ 300, cov.sel_prob))

    dirichlet_likelihood <- function(x) {
      lik <- sum(sum_s * (rep(x, p) - 1)) +
                     lgamma(sum(rep(x, p))) -
                     sum(lgamma(rep(x, p)))
      return(lik)
    }

    dirichlet_lik.proposal <- dirichlet_likelihood(proposal.dirichlet.alpha / p)
    dirichlet_lik <- dirichlet_likelihood(dirichlet.alpha / p)
    log_ratio <- dirichlet_lik.proposal +
              log(
                  (proposal.dirichlet.alpha / (proposal.dirichlet.alpha + p)) ^ (a0 - b0) *
                  (p / (proposal.dirichlet.alpha + p)) ^ (b - 1) *
                  abs(1 / (proposal.dirichlet.alpha + p) - proposal.dirichlet.alpha / (proposal.dirichlet.alpha + p) ^ 2)
              ) + dnorm(dirichlet.alpha, proposal.dirichlet.alpha, 0.1, log = TRUE) -
              dirichlet_lik -
              log(
                  (dirichlet.alpha / (dirichlet.alpha + p)) ^ (a0 - b0) *
                  (p / (dirichlet.alpha + p)) ^ (b - 1) *
                  abs(1 / (dirichlet.alpha + p) - dirichlet.alpha / (dirichlet.alpha + p) ^ 2)
              ) - dnorm(proposal.dirichlet.alpha, dirichlet.alpha, 0.1, log = TRUE)

    if (log_ratio > log(runif(1))) {
      dirichlet.alpha <- proposal.dirichlet.alpha
    }
    posterior.dirichlet.alpha <- rep(dirichlet.alpha / p, p) + rules.count
  }

  cov.sel_prob <- rdirichlet(1, posterior.dirichlet.alpha)
  
  return(list(dirichlet_alpha = dirichlet.alpha, posterior_dirichlet_alpha = posterior.dirichlet.alpha, cov_sel_prob = cov.sel_prob))
}