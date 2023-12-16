
sbart_fit <- function(
    x,
    y,
    ws,
    siam,
    missing_indexes,
    n_trees = 50L,
    n_iterations = 10000L,
    warmup = 1000L
) {

  # Set progress bar
  #
  #---------------------------
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = n_iterations - 1,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress barb


  source("R/SBART/predict.R")
  source("R/SBART/init_model_parameters.R")
  source("R/SBART/MCMC/init_chain.R")
  source("R/SBART/MCMC/update_residuals.R")
  source("R/SBART/MCMC/sample_trees.R")
  source("R/SBART/MCMC/sample_means.R")
  source("R/SBART/MCMC/sample_variance.R")
  source("R/SBART/MCMC/update_dirichlet_alpha.R")
  source("R/SBART/MCMC/update_f.R")
  source("R/SBART/MCMC/update_rho.R")
  source("R/SBART/MCMC/update_spatial_effect.R")
  source("R/SBART/MCMC/update_tau.R")

  # Get model parameters
  #
  # --------------------------
  model_params <- init_model_parameters(x, y, siam, n_trees)

  p <- model_params$p
  n <- model_params$n
  n_locations_all <- model_params$n_locations_all
  prob_grow <- model_params$prob_grow
  prob_prune <- model_params$prob_prune
  prob_change <- model_params$prob_change
  alpha <- model_params$alpha
  beta <- model_params$beta
  dirichlet_alpha <- model_params$dirichlet_alpha
  posterior_dirichlet_alpha <- model_params$posterior_dirichlet_alpha
  cov_sel_prob <- model_params$cov_sel_prob
  tau2_alpha <- model_params$tau2_alpha
  tau2_beta <- model_params$tau2_beta
  tau2_posterior_shape <- model_params$tau2_posterior_shape
  tau2 <- model_params$tau2
  proposal_sd_rho <- model_params$proposal_sd_rho
  rho <- model_params$rho
  a0 <- model_params$a0
  b0 <- model_params$b0
  y <- model_params$y
  residuals <- model_params$residuals
  sigma2 <- model_params$sigma2
  nu <- model_params$nu
  lambda <- model_params$lambda
  sigma2_a <- model_params$sigma2_a
  sigma2_b <- model_params$sigma2_b
  sigma_mu <- model_params$sigma_mu
  missing_indexes <- model_params$missing_indexes

  # Initialize MCMC chain
  #
  # --------------------------
  initial_variables <- init_chain(n_iterations, n_locations_all, p, n_trees, n, x, y, missing_indexes, siam, ws, rho)

  sigma2_samples <- initial_variables$sigma2_samples
  rho_samples <- initial_variables$rho_samples
  tau2_samples <- initial_variables$tau2_samples
  spatial_theta <- initial_variables$spatial_theta
  cov_sel <- initial_variables$cov_sel
  obs_list_ind <- initial_variables$obs_list_ind
  dt_list <- initial_variables$dt_list
  trees <- initial_variables$trees
  trees_pred <- initial_variables$trees_pred
  x_list <- initial_variables$x_list
  x_mult <- initial_variables$x_mult
  x_unique <- initial_variables$x_unique
  w_sel <- initial_variables$w_sel
  w_sel_samples <- initial_variables$w_sel_samples
  w_count <- initial_variables$w_count
  w_siam <- initial_variables$w_siam
  w_siam_full <- initial_variables$w_siam_full
  w_post <- initial_variables$w_post
  w_post_full <- initial_variables$w_post_full
  w_star <- initial_variables$w_star
  w_star_eigen <- initial_variables$w_star_eigen
  w_star_eigen_vals <- initial_variables$w_star_eigen_vals
  det_q <- initial_variables$det_q
  y <- initial_variables$y
  missing_indexes <- initial_variables$missing_indexes
  y_da <- initial_variables$y_da

  # Run MCMC
  #
  # --------------------------
  for (j in 2:n_iterations) {
    # Update progress bar
    pb$tick()

    for (t in 1:n_trees) {

      # Update residuals
      #
      # --------------------------
      residuals <- update_residuals(y, trees, t, spatial_theta, missing_indexes)


      # Metropolis Hastings step to sample T_t
      #
      # -------------------------- 
      tree_samples <- sample_trees(dt_list, prob_grow, prob_change, prob_prune, sigma2_samples, j, sigma_mu, t, residuals, cov_sel_prob, obs_list_ind, x_list, x_unique, n, alpha, beta)

      dt_list <- tree_samples$dt_list
      obs_list_ind <- tree_samples$obs_list_ind

      # Step to sample M_t
      #
      # --------------------------
      mean_samples <- sample_means(sigma2_samples, sigma_mu, obs_list_ind, residuals, x_cut, n, trees, dt_list, t, j)
      
      trees <- mean_samples$trees
      dt_list <- mean_samples$dt_list
    }

    # Sample variance parameter
    #
    # --------------------------
    sigma2_samples <- sample_variance(y, trees, spatial_theta, missing_indexes, sigma2_a, sigma2_b, n_locations_all, sigma2_samples, j)

  
    # Update of spatial effect 
    #
    # --------------------------
    spatial_theta <- update_spatial_effect(y, trees_pred, w_post_full, n_locations_all, spatial_theta, tau2, rho, sigma2_samples, j)


    # Update tau2 
    #
    # --------------------------
    result_tau2 <- update_tau2(w_post_full, n_locations_all, spatial_theta, rho, tau2_beta, tau2_posterior_shape, tau2_samples, j)

    tau2_samples <- result_tau2$tau2_samples
    temp <- result_tau2$temp


    # Update rho based on Metropolis Hastings step
    #
    # --------------------------
    result_rho <- update_rho(rho, proposal_sd_rho, w_post_full, n_locations_all, spatial_theta, tau2, w_star_eigen_vals, det_q, temp, rho_samples, j)

    rho_samples <- result_rho$rho_samples
    det_q <- result_rho$det_q
    temp <- result_rho$temp


    # update f based on Metropolis Hastings step
    #
    # --------------------------
    result_f <- update_f(ws, w_count, siam, n_locations_all, spatial_theta, rho, tau2, det_q, temp, w_sel, w_siam_full, w_post_full, w_star, w_star_eigen, w_star_eigen_vals, w_sel_samples, j)

    w_sel_samples <- result_f$w_sel_samples
    det_q <- result_f$det_q
    w_siam_full <- result_f$w_siam_full
    w_star <- result_f$w_star
    w_star_eigen <- result_f$w_star_eigen
    w_star_eigen_vals <- result_f$w_star_eigen_vals


    # Update dirichlet alpha 
    #
    # --------------------------
    result_dirichlet <- update_dirichlet_alpha(dt_list, p, j, warmup, dirichlet_alpha, a0, b0, cov_sel_prob)

    cov_sel_prob <- result_dirichlet$cov_sel_prob
    rules_count <- result_dirichlet$rules_count


    trees_pred <- matrix(unlist(sapply(1:n_trees, function(x) mean_predict(dt_list[[x]], x_list, x_mult, x_unique, n_locations_all))), nrow = n_locations_all, ncol = n_trees)

    if(any(is.na(trees_pred))){
      browser()
    }

    # check which variables are in the model 
    cov_sel_temp <- ifelse(rules_count > 0, 1, 0)
    cov_sel <- rbind(cov_sel, cov_sel_temp)


  }

  results <- list(
        covariates_selection_chain = cov_sel,
        spatial_theta_chain = spatial_theta,
        sigma2_chain = sigma2_samples,
        trees_chain = trees_pred,
        w_selection_chain = w_sel_samples
    )

  return(results)
}


sbart_predict <- function(sbart.output, X.test, missing_indexes) {
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

  return(y.missing)
}