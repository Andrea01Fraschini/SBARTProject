#' Update function for Dirichlet alpha parameter
#'
#' This function updates the Dirichlet alpha parameter in a Bayesian model using a Metropolis-Hastings algorithm.
#' It is used in the context of decision tree models, where the Dirichlet alpha parameter controls the distribution of split variables.
#'
#' @param dt_list A list of decision trees.
#' @param p The number of potential split variables.
#' @param j The current iteration of the MCMC.
#' @param warmup The number of warmup iterations.
#' @param dirichlet_alpha The current value of the Dirichlet alpha parameter.
#' @param a0 A hyperparameter for the Dirichlet alpha parameter.
#' @param b0 A hyperparameter for the Dirichlet alpha parameter.
#' @param cov_sel_prob The current probabilities of selecting each split variable.
#'
#' @return A list containing: 
#' - cov_sel_prob: The updated probabilities of selecting each split variable.
#' - rules_count: The number of rules for each split variable.
#' - dirichlet_alpha: The updated Dirichlet alpha parameter.
#' - posterior_dirichlet_alpha: The updated posterior Dirichlet alpha parameter.
#' 
#' @export
#'
update_dirichlet_alpha <- function(
    dt_list,
    p,
    j,
    warmup,
    dirichlet_alpha,
    a0,
    b0,
    cov_sel_prob
) {
    dt.split_vars <- unlist(lapply(dt_list, function(x) x$split))
    rules_count <- as.numeric(table(factor(dt.split_vars[!is.na(dt.split_vars)], levels = 1:p)))

    if (j < warmup) {
        posterior_dirichlet_alpha <- rep(1, p) + rules_count
    } else {
        proposal_dirichlet_alpha <- max(rnorm(1, dirichlet_alpha, 0.1), 0.1^10)
        sum_s <- log(ifelse(cov_sel_prob < 0.1^300, 0.1^300, cov_sel_prob))

        dirichlet_likelihood <- function(x) {
            lik <- sum(sum_s * (rep(x, p) - 1)) +
                lgamma(sum(rep(x, p))) -
                sum(lgamma(rep(x, p)))
            return(lik)
        }

        dirichlet_lik.proposal <- dirichlet_likelihood(proposal_dirichlet_alpha / p)
        dirichlet_lik <- dirichlet_likelihood(dirichlet_alpha / p)
        log_ratio <- dirichlet_lik.proposal +
            log(
                (proposal_dirichlet_alpha / (proposal_dirichlet_alpha + p))^(a0 - b0) *
                    (p / (proposal_dirichlet_alpha + p))^(b0 - 1) *
                    abs(1 / (proposal_dirichlet_alpha + p) - proposal_dirichlet_alpha / (proposal_dirichlet_alpha + p)^2)
            ) + dnorm(dirichlet_alpha, proposal_dirichlet_alpha, 0.1, log = TRUE) -
            dirichlet_lik -
            log(
                (dirichlet_alpha / (dirichlet_alpha + p))^(a0 - b0) *
                    (p / (dirichlet_alpha + p))^(b0 - 1) *
                    abs(1 / (dirichlet_alpha + p) - dirichlet_alpha / (dirichlet_alpha + p)^2)
            ) - dnorm(proposal_dirichlet_alpha, dirichlet_alpha, 0.1, log = TRUE)

        if (log_ratio > log(runif(1))) {
            dirichlet_alpha <- proposal_dirichlet_alpha
        }
        posterior_dirichlet_alpha <- rep(dirichlet_alpha / p, p) + rules_count
    }

    cov_sel_prob <- rdirichlet(1, posterior_dirichlet_alpha)

    return(
        list(
            cov_sel_prob = cov_sel_prob, 
            rules_count = rules_count, 
            dirichlet_alpha = dirichlet_alpha, 
            posterior_dirichlet_alpha = posterior_dirichlet_alpha
        )
    )
}
