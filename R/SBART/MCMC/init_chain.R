#' Initialize MCMC chains for Bayesian spatial model
#'
#' @param n_iterations Number of iterations for the MCMC chain
#' @param n_locations_all Total number of locations
#' @param p Number of covariates
#' @param n_trees Number of decision trees
#' @param n Number of observations
#' @param x Matrix of covariates
#' @param y Vector of observations
#' @param missing_indexes Indexes of missing observations
#' @param siam Spatial interaction adjacency matrix
#' @param ws List of weight matrices for spatial interaction
#' @param rho Spatial smoothing parameter
#'
#' @return A list containing the following elements:
#'   - sigma2_samples: A vector of samples for the error variance.
#'   - rho_samples: A vector of samples for the spatial autocorrelation parameter.
#'   - tau2_samples: A vector of samples for the variance of the random effects.
#'   - spatial_theta: A vector of spatial random effects.
#'   - cov_sel: A vector indicating the selected covariates.
#'   - obs_list_ind: A list of vectors, each containing the indices of observations for each tree.
#'   - dt_list: A list of decision trees.
#'   - trees: A matrix of tree values.
#'   - trees_pred: A matrix of tree predictions.
#'   - x_list: A list of vectors, each containing the values of a covariate for non-missing observations.
#'   - x_mult: A list of vectors, each containing the values of a covariate for all observations.
#'   - x_unique: A list of vectors, each containing the unique values of a covariate for non-missing observations.
#'   - w_sel: The index of the selected spatial weights matrix.
#'   - w_sel_samples: A vector of samples for the index of the selected spatial weights matrix.
#'   - w_count: The number of spatial weights matrices.
#'   - w_siam: The spatial interaction adjacency matrix for non-missing observations.
#'   - w_siam_full: The full spatial interaction adjacency matrix.
#'   - w_post: The row-normalized spatial interaction adjacency matrix for non-missing observations.
#'   - w_post_full: The full row-normalized spatial interaction adjacency matrix.
#'   - w_star: TODO.
#'   - w_star_eigen: The eigenvalues and eigen vectors of the w_star matrix.
#'   - w_star_eigen_vals: The eigenvalues of the w_star matrix.
#'   - det_q: The determinant of the precision matrix Q.
#'   - y: The response vector with missing values filled.
#'   - missing_indexes: The indices of missing values in the response vector.
#'   - y_da: A matrix of imputed values for the missing responses.
#' @export
#'
init_chain <- function(
    n_iterations, 
    n_locations_all, 
    p, 
    n_trees, 
    n, 
    x, 
    y, 
    missing_indexes, 
    siam, 
    ws, 
    rho
) {
    source("R/common/format_w_matrix.R")
    # TODO: Document better, with the references and etc.
    missing_indexes <- which(is.na(y))

    # Initialize samples for sigma2, rho, and tau2
    sigma2_samples <- rep(0.1, 1)
    rho_samples <- rep(0, n_iterations)
    tau2_samples <- rep(0, n_iterations)

    # Initialize spatial theta and covariate selection vector
    spatial_theta <- rep(0, n_locations_all)
    cov_sel <- rep(0, p)

    # Initialize lists for observation indexes and decision tree structures
    obs_list_ind <- list()
    dt_list <- list()
    for (ii in 1:n_trees) {
        obs_list_ind[[ii]] <- 1:n
        dt_list[[ii]] <- list(
            position = 1,
            parent = NA,
            terminal = TRUE,
            split = NA,
            value = NA,
            mu = NA,
            begin = 1,
            end = n
        )
    }

    # Initialize matrices for tree values and predictions
    trees <- matrix(0, nrow = n, ncol = n_trees)
    trees_pred <- matrix(0, nrow = n_locations_all, ncol = n_trees)

    # Initialize lists for covariate indexes
    x_list <- x_mult <- list()
    for (i in 1:p) {
        x_list[[i]] <- x[-missing_indexes, i]
        x_mult[[i]] <- x[, i]
    }
    x_mult[[p + 1]] <- 1:(n_locations_all)
    x_unique <- lapply(1:p, function(t) sort(unique(x[-missing_indexes, t])))

    # Initialize ws matrix and related parameters
    w_sel <- 1
    w_sel_samples <- NULL
    w_count <- length(ws)
    lapply(ws, function(x) diag(x) <- 0)
    w_siam <- siam
    for (i in 1:w_count) {
        w_siam <- w_siam * ws[[i]]^I(w_sel == i)
    }
    w_siam_full <- w_siam
    w_siam <- w_siam[-missing_indexes, -missing_indexes]
    rownames(w_siam) <- 1:(n_locations_all - length(missing_indexes))
    colnames(w_siam) <- 1:(n_locations_all - length(missing_indexes))
    w_post <- format_w_matrix(w_siam)
    w_post_full <- format_w_matrix(w_siam_full)

    # Compute eigenvalues of w_star
    w_star <- diag(apply(w_siam, 1, sum)) - w_siam
    w_star_eigen <- eigen(w_star)
    w_star_eigen_vals <- w_star_eigen$values
    det_q <- 0.5 * sum(log((rho * w_star_eigen_vals + (1 - rho))))

    # Fill missing values in y with normal random values
    y_da <- matrix(nrow = length(missing_indexes), ncol = n_iterations)
    set.seed(1)
    y_da[, 1] <- rnorm(length(missing_indexes), mean(y, na.rm = TRUE), 1)
    y[missing_indexes] <- y_da[, 1]

    return(
        list(
            sigma2_samples = sigma2_samples,
            rho_samples = rho_samples,
            tau2_samples = tau2_samples,
            spatial_theta = spatial_theta,
            cov_sel = cov_sel,
            obs_list_ind = obs_list_ind,
            dt_list = dt_list,
            trees = trees,
            trees_pred = trees_pred,
            x_list = x_list,
            x_mult = x_mult,
            x_unique = x_unique,
            w_sel = w_sel,
            w_sel_samples = w_sel_samples,
            w_count = w_count,
            w_siam = w_siam,
            w_siam_full = w_siam_full,
            w_post = w_post,
            w_post_full = w_post_full,
            w_star = w_star,
            w_star_eigen = w_star_eigen,
            w_star_eigen_vals = w_star_eigen_vals,
            det_q = det_q,
            y = y,
            missing_indexes = missing_indexes,
            y_da = y_da
        )
    )
}
