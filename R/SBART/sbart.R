#' Structurally Informed Bayesian Additive Regression Trees (SBART) function
#'
#' @param X A matrix of covariates.
#' @param y.train A vector of observations.
#' @param W An array of weighted adjacency matrices.
#' @param SIAM A Structurally Informed Adjacency Matrix specification.
#' @param missing_indexes Indexes of missing observations in matrices.
#' @param n.trees Number of trees (integer). Default is 50.
#' @param n.iterations Number of MCMC iterations. Default is 10000.
#' @param n.burnin Number of iterations to discard as burn-in. Default is 5000.
#' @param thin Thin factor. Default is 10.
#' @param warmup Iterations warmup period. Default is n.iterations / 10.
#' 
#' @return A list with the following elements:
#' @return \itemize{
#'   \item \code{covariates_selection_chain}: Covariate selection chain.
#'   \item \code{spatial_theta_chain}: Spatial theta chain.
#'   \item \code{sigma2_chain}: Sigma2 samples chain.
#'   \item \code{trees_chain}: Trees prediction chain.
#'   \item \code{W_selection_chain}: W selection samples chain.
#' }
#' @export
#' 
sbart.fit <- function(
    X,
    y.train,
    W,
    SIAM,
    missing_indexes,
    n.trees = 50L,
    n.iterations = 10000L,
    n.burnin = 5000L,
    thin = 10L,
    warmup = n.iterations / 10
) {
  sourceCpp("src/CARBayes.cpp") # this C++ code from CARBayes
  source("MCMC/GROW.R")
  source("MCMC/CHANGE.R")
  source("MCMC/PRUNE.R")
  source("MCMC/CommonFunctions.R") # TODO: fix 
  source("MCMC/Prediction.R") # TODO: fix    
  source("MCMC/SampleMean.R") # TODO: fix    
  source("R/SBART/initialize_model_parameters.R")
  source("R/SBART/MCMC/init_MCMC.R")

  # Get model parameters
  #
  # --------------------------
  model_params <- initialize_model_parameters(X, y.train, SIAM, n.trees)

  with(model_params, {
    p <- p
    n <- n
    n.locations.all <- n.locations.all
    prob.grow <- prob.grow
    prob.prune <- prob.prune
    prob.change <- prob.change
    alpha <- alpha
    beta <- beta
    dirichlet.alpha <- dirichlet.alpha
    posterior.dirichlet.alpha <- posterior.dirichlet.alpha
    cov.sel_prob <- cov.sel_prob
    tau2.a <- tau2.a
    tau2.b <- tau2.b
    tau2.posterior.shape <- tau2.posterior.shape
    tau2 <- tau2
    proposal.sd.rho <- proposal.sd.rho
    rho <- rho
    a0 <- a0
    b0 <- b0
    shift.amount <- shift.amount
    y <- y
    residuals <- residuals
    sigma2 <- sigma2
    nu <- nu
    q <- q
    sigma.quantile <- sigma.quantile
    lambda <- lambda
    sigma2.a <- sigma2.a
    sigma2.b <- sigma2.b
    k <- k
    sigma_mu <- sigma_mu
  })

  # Initialize MCMC variables
  #
  # --------------------------
  initial_variables <- initialize_mcmc_chains(n.iterations, n.locations.all, p, n.trees, n, X, missing_indexes, SIAM, W)

  with(initial_variables, {
    sigma2.samples <- sigma2.samples
    rho.samples <- rho.samples
    tau2.samples <- tau2.samples
    spatial_theta <- spatial_theta
    cov_sel <- cov_sel
    obs_list.ind <- obs_list.ind
    dt_list <- dt_list
    trees <- trees
    trees.pred <- trees.pred
    W_sel <- W_sel
  })

  # Run MCMC
  #
  # --------------------------
 
  for (j in 2:n.iterations) {
    for (t in 1:n.trees) {

      # Update residuals
      #
      # R_{i,(-t)} = y_i - ∑^T_{r ≠ t} g(x_i; T_t, M_t) - θ_i
      # --------------------------
      residuals <- y - rowSums(trees[, - t]) - spatial_theta[-missing_indexes]

      # Find depth of the tree 
      tree.depth <- length(dt_list[[t]]$position) # CS: I thougth it was a list per node, but now I see it's one per tree, I don't get the representation now.

      # Metropolis Hastings step to sample T_t
      #
      # T_t ∼ [T_t | R_{i,(-t)},...,R_{n,(-t)}, σ^2]
      # -------------------------- 
      step <- ifelse(tree.depth == 1, # check if root node, if so, only grow perturbation is possible
                1, # GROW.root 
                sample(2:4, 1, prob = c(prob.grow, prob.prune, prob.change)) # Pick a perturbation 
            )

      perturbation <- switch(
                step, # selection var 
                GROW.root,
                GROW,
                PRUNE,
                CHANGE,
            )

      # Apply perturbation to trees 
      result <- perturbation(
                sigma2 = sigma2.samples[j - 1],
                sigma_mu = sigma_mu,
                dt = dt_list[[t]],
                residuals = residuals,
                prop.prob = cov.sel_prob,
                obs = obs_list.ind[[t]],
                x.list = Xlist,
                xcut = X.unique,
                n.available = n,
                prob.grow = prob.grow,
                prob.change = prob.change,
                prob.prune = prob.prune,
                alpha = alpha,
                beta = beta
            )

      dt_list[[t]] <- result$dt # update tree structure
      obs_list.ind[[t]] <- result$obs # update observations indexes

      # Step to sample M_t
      #
      # M_t ∼ [M_t | T_t, R_{i,(-t)},...,R_{n,(-t)}, σ^2]
      # --------------------------
      mean.samples <- Mean.sample(
                sigma2 = sigma2.samples[j - 1],
                sigma_mu = sigma_mu,
                dt = dt_list[[t]],
                obs = obs_list.ind[[t]],
                residuals = residuals,
                n.available = n
            )
      trees[, t] <- mean.samples$T # CS: How does it work? what are you modifying in these 3 lines? I guess the M_t, but how?
      teeemp <- mean.samples$dt
      dt_list[[t]] <- teeemp
    }

    # Sample variance parameter
    #
    # R_{final,i} = y_i - ∑^T_{t=1} g(x_i; T_t, M_t) - θ_i
    # σ^2 ∼ IG(a_σ + n/2, b_σ + 1/2(∑_{i=1}^n R_{final,i}))
    # --------------------------
    Rfinal <- y - rowSums(trees) - spatial_theta[-missing_indexes]
    sigma2.samples[j] <- rinvgamma(1, sigma2.a + n / 2, scale = sigma2.b + sum((Rfinal) ^ 2) / 2)

    # Update of spatial effect 
    #
    # Short version:
    # θ_i ∼ N(...,...)
    #
    # Long version (Possibly with mistakes):
    # θ_i ∼  N( ((ρ (∑^n_(k=1) w_ikθ_k) / τ^2) + e_i/ σ^2) / (( 1 / τ^2 ( ρ (∑_^n{k=1} w_ik) + 1 - ρ )^-1) + 1/σ^2),  1/(( 1 / τ^2 ( ρ (∑_^n{k=1} w_ik) + 1 - ρ )^-1) + 1/σ^2))
    # --------------------------
    offset <- (y - rowSums(trees.pred))
    spatial_theta <- gaussiancarupdate(# CS: I'd put some snake case or, dot case idk how is it call here but yeah, in the function name
            Wtriplet = W.post.full$W.triplet,
            Wbegfin = W.post.full$W.begfin,
            W.post.full$W.triplet.sum,
            nsites = n.locations.all,
            phi = spatial_theta,
            tau2 = tau2,
            rho = rho,
            nu2 = sigma2.samples[j],
            offset = offset
        )
    spatial_theta <- spatial_theta - mean(spatial_theta)

    # CS: HERE I FINISHED READING CAREFULLY... TO BE CONTINUE...

    # Update tau2 
    #
    # Short version:
    # τ^2 ∼ IG(...,...)
    #
    # Long version (Possibly with mistakes):
    # τ^2 ∼ IG( α_τ + n/2, β_τ + 1/2((∑^n_{i=1} θ^2_i ( ρ ∑_{k=1}^n w_ik + 1 - ρ)) ρ ( (∑^n_{i=1} (∑^n_{k=1} θ_i θ_k w_ik )))))
    # --------------------------
    temp <- quadform(# CS: What does this?, ig something for the inverse gamma?
            as.matrix(W.post.full$W.triplet),
            W.post.full$W.triplet.sum,
            W.post.full$n.triplet,
            n.locations.all,
            spatial_theta,
            spatial_theta,
            rho
        )
    tau2.posterior.scale <- tau2.b + temp
    tau2 <- 1 / rgamma(1, tau2.posterior.shape, scale = (1 / tau2.posterior.scale))
    tau2.samples[j] <- tau2

    # update rho based on Metropolis Hastings step # CS: Why is it a metropolis step?
    proposal.rho <- rtruncnorm(n = 1, a = 0, b = 1, mean = rho, sd = proposal.sd.rho)
    temp2 <- quadform(
            as.matrix(W.post.full$W.triplet),
            W.post.full$W.triplet.sum,
            W.post.full$n.triplet,
            n.locations.all,
            spatial_theta,
            spatial_theta,
            proposal.rho
        )
    det.Q.proposal <- 0.5 * sum(log(proposal.rho * Wstar.eigen_vals + (1 - proposal.rho)))
    logprob.current <- det.Q - temp / tau2
    logprob.proposal <- det.Q.proposal - temp2 / tau2
    logprob.hastings <- log(dtruncnorm(x = rho, a = 0, b = 1, mean = proposal.rho, sd = proposal.sd.rho)) -
                            log(dtruncnorm(x = proposal.rho, a = 0, b = 1, mean = rho, sd = proposal.sd.rho))

    accept.prob <- exp(logprob.proposal - logprob.current + logprob.hastings)

    if (accept.prob > runif(1)) {
      rho <- proposal.rho
      det.Q <- det.Q.proposal
      temp <- temp2
    }
    rho.samples <- rho

    # update f based on Metropolis Hastings step # CS: Here I decide if I accept the new structure or not?
    proposal.W_sel <- sample(1:W.count, 1)
    W.siam.proposal <- SIAM
    for (i in 1:W.count) {
      W.siam.proposal <- W.siam.proposal * W[[i]] ^ I(proposal.W_sel == i)
    }
    rownames(W.siam.proposal) <- 1:(n.locations.all)
    colnames(W.siam.proposal) <- 1:(n.locations.all)
    W.post.proposal <- formatWMatrix(W.siam.proposal)

    temp3 <- quadform(
            as.matrix(W.post.proposal$W.triplet),
            W.post.proposal$W.triplet.sum,
            W.post.proposal$n.triplet,
            n.locations.all,
            spatial_theta,
            spatial_theta,
            rho
        )

    Wstar.proposal <- diag(apply(W.siam.proposal, 1, sum)) - W.siam.proposal
    Wstar.eigen.proposal <- eigen(Wstar.proposal)
    Wstar.eigen_vals.proposal <- Wstar.eigen.proposal$values

    det.Q.proposal <- 0.5 * sum(log((rho * Wstar.eigen_vals.proposal + (1 - rho))))
    logprob.current <- det.Q - temp / tau2
    logprob.proposal <- det.Q.proposal - temp3 / tau2

    accept.prob <- exp(logprob.proposal - logprob.current)

    if (accept.prob > runif(1)) {
      W_sel <- proposal.W_sel
      det.Q <- det.Q.proposal
      W.siam.full <- W.siam.proposal
      W.post.full <- W.post.proposal
      Wstar <- Wstar.proposal
      Wstar.eigen <- Wstar.eigen.proposal
      Wstar.eigen_vals <- Wstar.eigen_vals.proposal
    }
    W_sel.samples[j] <- W_sel

    # update dirichlet alpha 
    dt.split_vars <- unlist(lapply(dt_list, function(x) x$split))
    # foreach predictor, count how many times it appears in splitting rules 
    rules.count <- as.numeric(table(factor(dt.split_vars[!is.na(dt.split_vars)], levels = 1:p)))
    if (j < warmup) {
      posterior.dirichlet.alpha <- rep(1, p) + rules.count
    } else {
      # alpha / (alpha + p) ~ Beta(a0, b0)
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

    trees.pred <- matrix(unlist(sapply(1:n.trees, function(x) Mean.predict(dt_list[[x]], Xlist, Xmult, X.unique, n))), nrow = n, ncol = n.trees)

    # check which variables are in the model 
    cov_sel.temp <- ifelse(rules.count > 0, 1, 0)
    cov_sel <- rbind(cov_sel, cov_sel.temp)
  }

  results <- list(
        covariates_selection_chain = cov_sel,
        spatial_theta_chain = spatial_theta,
        sigma2_chain = sigma2.samples,
        trees_chain = trees.pred,
        W_selection_chain = W_sel.samples
    )

  return(results)
}

#' SBART prediction function
#'
#' @param sbart.output The output from the SBART function.
#' @param X.test A matrix of test covariates.
#' @param missing_indexes Indexes of missing observations in matrices.
#'
#' @return A matrix of predicted values for the missing observations.
#' @return \itemize{
#'  \item \code{y.missing}: Predicted values for the missing observations.
#' }
#' @export
#' 
sbart.predict <- function(sbart.output, X.test, missing_indexes) {
  mean <- rowSums(sbart.output$trees_chain) + spatial
  sigma2 <- sbart.output$sigma2_chain

  chains_len <- length(sigma2)
  n.trees <- dim(sbart.output$trees_chain)[2]
  n.missing <- dim(X.test)[1]
  y.missing <- matrix(nrow = n.missing, ncol = chains_len)

  count <- 0
  for (i in 1:chains_len) {
    for (j in 1:n.trees) {
      count <- count + 1
      y.missing[, count] <- rnorm(n.missing, c(mean)[missing_indexes], sigma2[j])
    }
  }

  return(y.missing) # TODO: Dunno if this is correct.
}