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
  source("R/SBART/init_model_parameters.R")
  source("R/SBART/MCMC/init_chain.R")

  # Get model parameters
  #
  # --------------------------
  model_params <- init_model_parameters(X, y.train, SIAM, n.trees)

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
    lambda <- lambda
    sigma2.a <- sigma2.a
    sigma2.b <- sigma2.b
    sigma_mu <- sigma_mu
  })

  # Initialize MCMC chain
  #
  # --------------------------
  initial_variables <- init_chain(n.iterations, n.locations.all, p, n.trees, n, X, missing_indexes, SIAM, W)

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
      # --------------------------
      residuals <- update_residuals(y, trees, t, spatial_theta, missing_indexes)


      # Metropolis Hastings step to sample T_t
      #
      # -------------------------- 
      result <- sample_trees(sigma2, sigma_mu, dt, residuals, prop.prob, obs, x.list, xcut, n.available, prob.grow, prob.change, prob.prune, alpha, beta)
      dt <- result$dt # update tree structure
      obs <- result$obs # update observations indexes

      # Step to sample M_t
      #
      # --------------------------
      mean.samples <- sample_mean(
                sigma2 = sigma2.samples[j - 1],
                sigma_mu = sigma_mu,
                dt = dt_list[[t]],
                obs = obs_list.ind[[t]],
                residuals = residuals,
                n.available = n
            )
      trees[, t] <- mean.samples$T 
      teeemp <- mean.samples$dt
      dt_list[[t]] <- teeemp
    }

    # Sample variance parameter
    #
    # --------------------------
    sigma2.samples[j] <- sample_variance(y, trees, spatial_theta, missing_indexes, sigma2.a, sigma2.b, n)
  
    # Update of spatial effect 
    #
    # --------------------------
    spatial_theta <- update_spatial_effect(
                y = y,
                trees_pred = trees.pred,
                W_post_full = W.post.full,
                n_locations_all = n.locations.all,
                spatial_theta = spatial_theta,
                tau2 = tau2,
                rho = rho,
                sigma2_samples = sigma2.samples[j]
            )

    # Update tau2 
    #
    # --------------------------
    tau2.samples[j] <- update_tau2(
            W_triplet = W.post.full$W.triplet,
            W_triplet_sum = W.post.full$W.triplet.sum,
            n_triplet = W.post.full$n.triplet,
            n_locations_all = n.locations.all,
            spatial_theta = spatial_theta,
            rho = rho,
            tau2_b = tau2.b,
            tau2_posterior_shape = tau2.posterior.shape
        )

    # Update rho based on Metropolis Hastings step
    #
    # --------------------------
    result_rho <- update_rho(
            W_triplet = W.post.full$W.triplet,
            W_triplet_sum = W.post.full$W.triplet.sum,
            n_triplet = W.post.full$n.triplet,
            n_locations_all = n.locations.all,
            spatial_theta = spatial_theta,
            rho = rho,
            tau2 = tau2,
            proposal.sd.rho = proposal.sd.rho,
            W = W,
            W.count = W.count,
            W_sel = W_sel,
            SIAM = SIAM,
            W.siam.full = W.siam.full,
            W.post.full = W.post.full,
            Wstar = Wstar,
            Wstar.eigen = Wstar.eigen,
            Wstar.eigen_vals = Wstar.eigen_vals,
            warmup = warmup
        )

    with(result_rho, {
      rho.samples <- rho
      det.Q <- det.Q
      temp <- temp
    })

    # update f based on Metropolis Hastings step
    #
    # --------------------------
    result_f <- update_f(W, W_count, SIAM, n_locations_all, spatial_theta, rho, tau2, det_Q, temp, W_sel, W_siam_full, W_post_full, Wstar, Wstar_eigen, Wstar_eigen_vals)

    with(result_f,{
      W_sel <- W_sel
      det.Q <- det.Q
      W_siam.full <- W_siam.full
      W.post.full <- W.post.full
      Wstar <- Wstar
      Wstar.eigen <- Wstar.eigen
      Wstar.eigen_vals <- Wstar.eigen_vals
    })


    # Update dirichlet alpha 
    #
    # --------------------------
    result_dirichlet_alpha <- update_dirichlet_alpha(dt_list, p, j, warmup, dirichlet_alpha, a0, b0, cov_sel_prob)

    with(result_dirichlet_alpha, {
      dirichlet_alpha <- dirichlet_alpha
      posterior_dirichlet_alpha <- posterior_dirichlet_alpha
      cov_sel_prob <- cov_sel_prob
    })

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