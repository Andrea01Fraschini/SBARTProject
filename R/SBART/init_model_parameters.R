#' Initialize Model Parameters
#'
#' This function initializes the parameters for a model.
#'
#' @param x A matrix of covariates.
#' @param y A vector of responses (Including missing ones).
#' @param siam A matrix representing spatial information.
#' @param n_trees The number of trees in the model.
#'
#' @return A list with the following elements:
#' - p: The number of covariates.
#' - n: The number of observations.
#' - n_locations_all: The total number of locations.
#' - prob_grow: The probability of inducing a grow perturbation in tree structure.
#' - prob_prune: The probability of inducing a prune perturbation in tree structure.
#' - prob_change: The probability of inducing a change perturbation in tree structure.
#' - alpha: The hyperparameter for regularization prior over tree depth.
#' - beta: The hyperparameter for regularization prior over tree depth.
#' - dirichlet_alpha: The hyperparameter for the Dirichlet prior over the selection probabilities.
#' - posterior_dirichlet_alpha: The posterior hyperparameter for the Dirichlet prior over the selection probabilities.
#' - cov_sel_prob: The selection probabilities for covariates.
#' - tau2_alpha: The alpha parameter for the inverse gamma prior over the variance of the random effects.
#' - tau2_beta: The beta parameter for the inverse gamma prior over the variance of the random effects.
#' - tau2_posterior_shape: The shape parameter for the posterior distribution of the variance of the random effects.
#' - tau2: The variance of the random effects.
#' - proposal_sd_rho: The standard deviation of the proposal distribution for the spatial correlation parameter.
#' - rho: The spatial correlation parameter.
#' - a0: The alpha parameter for the prior over the mean.
#' - b0: The beta parameter for the prior over the mean.
#' - y: The response vector centered to the mean.
#' - residuals: The residuals.
#' - sigma2: The variance of the error term.
#' - nu: The nu parameter for the inverse gamma prior over the variance of the error term.
#' - lambda: The lambda parameter for the inverse gamma prior over the variance of the error term.
#' - sigma2_a: The alpha parameter for the inverse gamma prior over the variance of the error term.
#' - sigma2_b: The beta parameter for the inverse gamma prior over the variance of the error term.
#' - sigma_mu: The variance of the prior over the mean.
#' - missing_indexes: The indexes of missing observations.
#'
#' @examples
#' # To use this function:
#' # source("init_model_parameters.R")
#' # model_params <- init_model_parameters(x, y, siam, n_trees)
#'
#' @export
#'
init_model_parameters <- function(x, y, siam, n_trees) {
    p <- dim(x)[2] # dimension of covariates
    n <- length(which(!is.na(y))) # num. of the locations with observation
    n_locations_all <- length(y) # num. of the locations
    missing_indexes <- which(is.na(y)) # indexes of missing observations

    # Perturbation probabilities
    #
    # ρ_grow = Probability to induce a grow perturbation in tree structure.
    # ρ_prune = Probability to induce a prune perturbation in tree structure.
    # ρ_change = Probability to induce a change perturbation in tree structure.
    # --------------------------
    prob_grow <- 0.28
    prob_prune <- 0.28
    prob_change <- 0.44

    # Depth regulating prior hyperparameters
    #
    # P(depth(N) = d) = α / (1 + d)^β
    # α, β: Hyperparameters for regularization prior over tree depth.
    # --------------------------
    alpha <- 0.95
    beta <- 2

    # Hyperparameters for spatial random effect
    #
    # θ_i ∣ θ_(-i), ρ, τ^2 ∼  N(ρ (∑^n_(k=1) w_ikθ_k) / ρ (∑^n_{k=1} w_{ik})+1-ρ, τ^2 / ρ (∑^n_{k=1} w_{ik}) +1-ρ )
    # τ^2 ∼ IG(α_τ, β_τ)
    # --------------------------
    tau2_alpha <- 1 # α_τ
    tau2_beta <- 0.01 # β_τ
    tau2_posterior_shape <- tau2_alpha + n_locations_all / 2
    tau2 <- 0.1 # initial value for τ^2
    proposal_sd_rho <- 0.2
    rho <- 0.5 # initial value for ρ
    a0 <- 0.5
    b0 <- 1

    # Shift the mean of y
    #
    # y_i = y_i - μ
    # explanation: Center the response variable to have mean zero.
    # --------------------------
    shift_amount <- mean(y, na.rm = TRUE)
    y <- y - shift_amount
    residuals <- y # initial value for residuals

    # Sigma prior hyperparameters
    #
    # σ^2 ∼ IG(ν/2, νλ/2) = IG(a_σ , b_σ)
    # '''(Chipman, 2010) We then pick a value of ν between 3 and 10 to get an appropriate shape, and a value of λ so that the qth quantile of the prior on σ is located at ˆσ, that is, P(σ < σˆ) = q. We consider values of q such as 0.75, 0.90 or 0.99 to center the distribution below ˆσ.'''
    # --------------------------
    sigma2 <- var(y, na.rm = TRUE) # initial value for sigma2
    nu <- 3
    q <- 0.90
    sigma_quantile <- function(lambda) invgamma::qinvgamma(q, nu / 2, rate = lambda * nu / 2, lower.tail = TRUE, log.p = FALSE) - sqrt(sigma2)
    lambda <- uniroot.all(sigma_quantile, c(0.1^5, 10))
    sigma2_a <- nu / 2
    sigma2_b <- nu * lambda / 2

    # Selection probabilities for covariates
    #
    # (s_1, ... ,s_P) ∼ Dirichlet( α/P, ... ,α/P )
    # --------------------------
    dirichlet_alpha <- 1
    posterior_dirichlet_alpha <- rep(1, p)
    cov_sel_prob <- rdirichlet(1, rep(dirichlet_alpha, p))

    # Mean prior hyperparameters
    #
    # μ ∼ N(0, σ_μ^2)
    # '''(Chipman, 2010) The essence of our strategy is then to choose µ_µ and σ_µ so that N(mµ_µ,mσ2_µ) assigns substantial probability to the interval (y_min, y_max). This can be conveniently done by choosing µ_µ and σ_µ so that mµ_µ − k√σ_µ = ymin and mµ_µ + k√mσ_µ = ymax for some preselected value of k, we recommend k = 2.'''
    # --------------------------
    k <- 2 # mean shrinkage
    sigma_mu <- max(
        (min(y, na.rm = TRUE) / (-k * sqrt(n_trees)))^2,
        (max(y, na.rm = TRUE) / (+k * sqrt(n_trees)))^2
    )

    return(
        list(
            p = p,
            n = n,
            n_locations_all = n_locations_all,
            prob_grow = prob_grow,
            prob_prune = prob_prune,
            prob_change = prob_change,
            alpha = alpha,
            beta = beta,
            dirichlet_alpha = dirichlet_alpha,
            posterior_dirichlet_alpha = posterior_dirichlet_alpha,
            cov_sel_prob = cov_sel_prob,
            tau2_alpha = tau2_alpha,
            tau2_beta = tau2_beta,
            tau2_posterior_shape = tau2_posterior_shape,
            tau2 = tau2,
            proposal_sd_rho = proposal_sd_rho,
            rho = rho,
            a0 = a0,
            b0 = b0,
            y = y,
            residuals = residuals,
            sigma2 = sigma2,
            nu = nu,
            lambda = lambda,
            sigma2_a = sigma2_a,
            sigma2_b = sigma2_b,
            sigma_mu = sigma_mu,
            missing_indexes = missing_indexes
        )
    )
}
