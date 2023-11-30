#' Initialize Model Parameters
#'
#' This function initializes the parameters for a model.
#'
#' @param X A matrix of covariates.
#' @param y.train A vector of training responses.
#' @param SIAM A matrix representing spatial information.
#' @param n.trees The number of trees in the model.
#'
#' @return A list of initialized parameters.
#'
#' @examples
#' # To use this function:
#' # source("initialize_model_parameters.R")
#' # model_params <- initialize_model_parameters(X, y.train, SIAM, n.trees)
#'
#' @export
#' 
initialize_model_parameters <- function(X, y.train, SIAM, n.trees) {
  p <- dim(X)[2] # dimension of covariates
  n <- length(y.train) # number of observations 
  n.locations.all <- dim(SIAM)[1] # number of locations

  # Perturbation probabilities 
  #
  # ρ_grow = Probability to induce a grow perturbation in tree structure.
  # ρ_prune = Probability to induce a prune perturbation in tree structure.
  # ρ_change = Probability to induce a change perturbation in tree structure.
  # -------------------------- 
  prob.grow <- 0.28
  prob.prune <- 0.28
  prob.change <- 0.44

  # Depth regulating prior hyperparameters 
  #
  # P(depth(N) = d) = α / (1 + d)^β 
  # α, β: Hyperparameters for regularization prior over tree depth.
  # --------------------------
  alpha <- 0.95
  beta <- 2

  # Selection probabilities for covariates 
  #
  # (s_1, ... ,s_P) ∼ Dirichlet( α/P, ... ,α/P ) 
  # --------------------------
  dirichlet.alpha <- 1
  posterior.dirichlet.alpha <- rep(1, p)
  cov.sel_prob <- rdirichlet(1, rep(dirichlet.alpha, p))

  # Hyperparameters for spatial random effect 
  #
  # θ_i ∣ θ_(-i), ρ, τ^2 ∼  N(ρ (∑^n_(k=1) w_ikθ_k) / ρ (∑^n_{k=1} w_{ik})+1-ρ, τ^2 / ρ (∑^n_{k=1} w_{ik}) +1-ρ )
  # τ^2 ∼ IG(α_τ, β_τ)
  # --------------------------
  tau2.a <- 1 # α_τ
  tau2.b <- 0.01 # β_τ
  tau2.posterior.shape <- tau2.a + n / 2
  tau2 <- 0.5 # initial value for τ^2
  proposal.sd.rho <- 0.2
  rho <- 0.5 # initial value for ρ
  a0 <- 0.5
  b0 <- 1

  # Shift the mean of Y 
  #
  # y_i = y_i - μ
  # explanation: Center the response variable to have mean zero.
  # --------------------------
  shift.amount <- mean(y.train)
  y <- y.train - shift.amount
  residuals <- y # initial value for residuals

  # Sigma prior hyperparameters 
  #
  # σ^2 ∼ IG(ν/2, νλ/2) = IG(a_σ , b_σ)
  # '''(Chipman, 2010) We then pick a value of ν between 3 and 10 to get an appropriate shape, and a value of λ so that the qth quantile of the prior on σ is located at ˆσ, that is, P(σ < σˆ) = q. We consider values of q such as 0.75, 0.90 or 0.99 to center the distribution below ˆσ.'''
  # --------------------------
  sigma2 <- var(y) # initial value for sigma2
  nu <- 3
  q <- 0.90
  sigma.quantile <- function(lambda) invgamma::qinvgamma(q, nu / 2, rate = lambda * nu / 2, lower.tail = TRUE, log.p = FALSE) - sqrt(sigma2)
  lambda <- uniroot.all(sigma.quantile, c(0.1 ^ 5, 10))
  sigma2.a <- nu / 2
  sigma2.b <- nu * lambda / 2

  # Mean prior hyperparameters
  #
  # μ ∼ N(0, σ_μ^2) 
  # '''(Chipman, 2010) The essence of our strategy is then to choose µ_µ and σ_µ so that N(mµ_µ,mσ2_µ) assigns substantial probability to the interval (y_min, y_max). This can be conveniently done by choosing µ_µ and σ_µ so that mµ_µ − k√σ_µ = ymin and mµ_µ + k√mσ_µ = ymax for some preselected value of k, we recommend k = 2.'''
  # --------------------------
  k <- 2 # mean shrinkage
  sigma_mu <- max(
        (min(y) / (-k * sqrt(n.trees))) ^ 2,
        (max(y) / (+k * sqrt(n.trees))) ^ 2
    )

  return(
        list(
            p = p,
            n = n,
            n.locations.all = n.locations.all,
            prob.grow = prob.grow,
            prob.prune = prob.prune,
            prob.change = prob.change,
            alpha = alpha,
            beta = beta,
            dirichlet.alpha = dirichlet.alpha,
            posterior.dirichlet.alpha = posterior.dirichlet.alpha,
            cov.sel_prob = cov.sel_prob,
            tau2.a = tau2.a,
            tau2.b = tau2.b,
            tau2.posterior.shape = tau2.posterior.shape,
            tau2 = tau2,
            proposal.sd.rho = proposal.sd.rho,
            rho = rho,
            a0 = a0,
            b0 = b0,
            shift.amount = shift.amount,
            y = y,
            residuals = residuals,
            sigma2 = sigma2,
            nu = nu,
            q = q,
            sigma.quantile = sigma.quantile,
            lambda = lambda,
            sigma2.a = sigma2.a,
            sigma2.b = sigma2.b,
            k = k,
            sigma_mu = sigma_mu
        )
    )
}