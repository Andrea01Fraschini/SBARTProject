#' sbart
#'
#' This function implements the SBART algorithm presented in the paper 
#' "Bayesian additive regression trees in spatial data analysis with sparse observations" by Chanmin Kim.
#'
#' @param x A matrix of covariates.
#' @param y A vector of observations.
#' @param ws A list of weight matrices for spatial interaction.
#' @param siam A matrix of spatial interaction.
#' @param missing_indexes A vector of indexes of missing observations.
#' @param n_trees The number of trees to be used in the model.
#' @param n_iterations The number of iterations for the MCMC chain.
#' @param warmup The number of warmup iterations for the MCMC chain.
#' @param progress A function to be called at each iteration of the MCMC chain.
#'
#' @return A list containing the following elements:
#' - covariates_selection_chain: A matrix of covariates selection.
#' - spatial_theta_chain: A matrix of spatial random effects.
#' - sigma2_chain: A vector of samples for the error variance.
#' - trees_chain: A matrix of tree predictions.
#' - w_selection_chain: A vector of samples for the index of the selected spatial weights matrix.
#' - dt_history: A list of decision trees.
#'
#' @export
#'
sbart <- function(
    x,
    y,
    ws,
    siam,
    missing_indexes,
    n_trees = 50L,
    n_iterations = 10000L,
    warmup = 1000L,
    progress = NULL
) {
    # Set progress bar
    #
    #---------------------------
    pb <- progress_bar$new(
        format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]", # nolint: line_length_linter.
        total = n_iterations - 1,
        complete = "=", # Completion bar character
        incomplete = "-", # Incomplete bar character
        current = ">", # Current bar character
        clear = FALSE, # If TRUE, clears the bar when finish
        width = 100
    ) # Width of the progress barb

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
    params <- init_model_parameters(
        x = x,
        y = y,
        siam = siam,
        n_trees = n_trees
    )

    # Initialize MCMC chain
    #
    # --------------------------
    vars <- init_chain(
        n_iterations = n_iterations,
        n_locations_all = params$n_locations_all,
        p = params$p,
        n_trees = n_trees,
        n = params$n,
        x = x,
        y = params$y,
        missing_indexes = missing_indexes,
        siam = siam,
        ws = ws,
        rho = params$rho
    )

    cov_sel_prob <- params$cov_sel_prob
    tau2 <- params$tau2
    rho <- params$rho
    dirichlet_alpha <- params$dirichlet_alpha

    sigma2_samples <- vars$sigma2_samples
    rho_samples <- vars$rho_samples
    tau2_samples <- vars$tau2_samples
    spatial_theta <- vars$spatial_theta
    cov_sel <- vars$cov_sel
    obs_list_ind <- vars$obs_list_ind
    dt_list <- vars$dt_list
    trees <- vars$trees
    trees_pred <- vars$trees_pred
    x_list <- vars$x_list
    x_mult <- vars$x_mult
    x_unique <- vars$x_unique
    w_sel <- vars$w_sel
    w_sel_samples <- vars$w_sel_samples
    w_count <- vars$w_count
    w_siam_full <- vars$w_siam_full
    w_post_full <- vars$w_post_full
    w_star <- vars$w_star
    w_star_eigen <- vars$w_star_eigen
    w_star_eigen_vals <- vars$w_star_eigen_vals
    det_q <- vars$det_q
    y <- vars$y
    missing_indexes <- vars$missing_indexes
    y_da <- vars$y_da

    # Initialize tree structures history
    tree_structures_history <- list()
    tree_structures_history[[1]] <- dt_list

    # Initialize y predictions history
    y_history <- list()

    # Run MCMC
    #
    # --------------------------
    for (j in 2:n_iterations) {
        for (t in 1:n_trees) {
            # Update residuals
            #
            # --------------------------
            residuals <- update_residuals(
                y = y,
                trees = trees,
                t = t,
                spatial_theta = spatial_theta,
                missing_indexes = vars$missing_indexes
            )

            # Metropolis Hastings step to sample T_t
            #
            # --------------------------
            result_trees <- sample_trees(
                dt_list = dt_list,
                prob_grow = params$prob_grow,
                prob_change = params$prob_change,
                prob_prune = params$prob_prune,
                sigma2_samples = sigma2_samples,
                j = j,
                sigma_mu = params$sigma_mu,
                t = t,
                residuals = residuals,
                cov_sel_prob = cov_sel_prob,
                obs_list_ind = obs_list_ind,
                x_list = x_list,
                x_unique = x_unique,
                n = params$n,
                alpha = params$alpha,
                beta = params$beta
            )

            dt_list <- result_trees$dt_list
            obs_list_ind <- result_trees$obs_list_ind

            # Step to sample M_t
            #
            # --------------------------
            result_means <- sample_means(
                sigma2_samples = sigma2_samples,
                sigma_mu = params$sigma_mu,
                obs_list_ind = obs_list_ind,
                residuals = residuals,
                x_cut = x_unique,
                n_available = params$n,
                trees = trees,
                dt_list = dt_list,
                t = t,
                j = j
            )

            trees <- result_means$trees
            dt_list <- result_means$dt_list
        }

        # Sample variance parameter
        #
        # --------------------------
        sigma2_samples <- sample_variance(
            y = y,
            trees = trees,
            spatial_theta = spatial_theta,
            missing_indexes = missing_indexes,
            sigma2_a = params$sigma2_a,
            sigma2_b = params$sigma2_b,
            n = params$n,
            sigma2_samples = sigma2_samples,
            j = j
        )

        # Update of spatial effect
        #
        # --------------------------
        spatial_theta <- update_spatial_effect(
            y = y,
            trees_pred = trees_pred,
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            tau2 = tau2,
            rho = rho,
            sigma2_samples = sigma2_samples,
            j = j
        )

        # Update tau2
        #
        # --------------------------
        results_tau2 <- update_tau2(
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            rho = rho,
            tau2_b = params$tau2_b,
            tau2_posterior_shape = params$tau2_posterior_shape,
            tau2_samples = tau2_samples,
            j = j
        )

        tau2_samples <- results_tau2$tau2_samples
        tau2 <- tau2_samples[j]
        temp <- results_tau2$temp

        # Update rho based on Metropolis Hastings step
        #
        # --------------------------
        results_rho <- update_rho(
            rho = rho,
            proposal_sd_rho = params$proposal_sd_rho,
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            tau2 = tau2,
            w_star_eigen_vals = w_star_eigen_vals,
            det_q = det_q,
            temp = temp,
            rho_samples = rho_samples,
            j = j
        )

        rho_samples <- results_rho$rho_samples
        rho <- rho_samples[j]
        temp <- results_rho$temp
        det_q <- results_rho$det_q

        # update f based on Metropolis Hastings step
        #
        # --------------------------
        results_f <- update_f(
            ws = ws,
            w_count = w_count,
            siam = siam,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            rho = rho_samples[j],
            tau2 = tau2_samples[j],
            det_q = det_q,
            temp = temp,
            w_sel = w_sel,
            w_siam_full = w_siam_full,
            w_post_full = w_post_full,
            w_star = w_star,
            w_star_eigen = w_star_eigen,
            w_star_eigen_vals = w_star_eigen_vals,
            w_sel_samples = w_sel_samples,
            j = j
        )

        w_sel_samples <- results_f$w_sel_samples
        det_q <- results_f$det_q
        w_siam_full <- results_f$w_siam_full
        w_post_full <- results_f$w_post_full
        w_star <- results_f$w_star
        w_star_eigen <- results_f$w_star_eigen
        w_star_eigen_vals <- results_f$w_star_eigen_vals


        # Update dirichlet alpha
        #
        # --------------------------
        result_dirichlet <- update_dirichlet_alpha(
            dt_list = dt_list,
            p = params$p,
            j = j,
            warmup = 1000L,
            dirichlet_alpha = dirichlet_alpha,
            a0 = params$a0,
            b0 = params$b0,
            cov_sel_prob = cov_sel_prob
        )

        cov_sel_prob <- result_dirichlet$cov_sel_prob
        rules_count <- result_dirichlet$rules_count
        dirichlet_alpha <- result_dirichlet$dirichlet_alpha
        posterior_dirichlet_alpha <- result_dirichlet$posterior_dirichlet_alpha


        trees_pred <- matrix(unlist(sapply(1:n_trees, function(x) mean_predict(dt_list[[x]], x_list, x_mult, x_unique, params$n_locations_all))), nrow = params$n_locations_all, ncol = n_trees) # nolint: line_length_linter.

        # check which variables are in the model
        cov_sel_temp <- ifelse(rules_count > 0, 1, 0)
        cov_sel <- rbind(cov_sel, cov_sel_temp)

        y_da[, j - 1] <- rnorm(length(missing_indexes), c(rowSums(trees_pred) + spatial_theta)[missing_indexes], sqrt(sigma2_samples[j])) # nolint: line_length_linter.
        y[missing_indexes] <- y_da[, j - 1]

        # For error handling
        if(any(is.na(y[missing_indexes]))) {
            print("Error: y[missing_indexes] is NA")
            print(which(is.na(y[missing_indexes])))
        }

        # save current tree structure
        tree_structures_history[[j]] <- dt_list

        # save current y predictions
        y_history[[j]] <- y

        # Update progress bar
        pb$tick()

        if (!is.null(progress)) {
            progress(
                iteration = j,
                dt_list = dt_list
            )
        }
    }

    results <- list(
        covariates_selection_chain = cov_sel,
        spatial_theta_chain = spatial_theta,
        sigma2_chain = sigma2_samples,
        trees_chain = trees_pred,
        w_selection_chain = w_sel_samples,
        dt_history = tree_structures_history,
        y_predictions = y,
        y_predictions_history = y_history
    )

    return(results)
}
