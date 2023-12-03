#' Initialize MCMC chains for Bayesian spatial model
#'
#' @param n.iterations Number of iterations for the MCMC chain
#' @param n.locations.all Total number of locations
#' @param p Number of covariates
#' @param n.trees Number of decision trees
#' @param n Number of observations
#' @param X Matrix of covariates
#' @param Y Vector of observations
#' @param missing_indexes Indexes of missing observations
#' @param SIAM Spatial interaction adjacency matrix
#' @param W List of weight matrices for spatial interaction
#' @param rho Spatial smoothing parameter
#'
#' @return A list containing the following elements:
#'   - sigma2.samples: A vector of samples for the error variance.
#'   - rho.samples: A vector of samples for the spatial autocorrelation parameter.
#'   - tau2.samples: A vector of samples for the variance of the random effects.
#'   - spatial_theta: A vector of spatial random effects.
#'   - cov_sel: A vector indicating the selected covariates.
#'   - obs_list.ind: A list of vectors, each containing the indices of observations for each tree.
#'   - dt_list: A list of decision trees.
#'   - trees: A matrix of tree values.
#'   - trees.pred: A matrix of tree predictions.
#'   - Xlist: A list of vectors, each containing the values of a covariate for non-missing observations.
#'   - Xmult: A list of vectors, each containing the values of a covariate for all observations.
#'   - X.unique: A list of vectors, each containing the unique values of a covariate for non-missing observations.
#'   - W_sel: The index of the selected spatial weights matrix.
#'   - W_sel.samples: A vector of samples for the index of the selected spatial weights matrix.
#'   - W.count: The number of spatial weights matrices.
#'   - W.siam: The spatial interaction adjacency matrix for non-missing observations.
#'   - W.siam.full: The full spatial interaction adjacency matrix.
#'   - W.post: The row-normalized spatial interaction adjacency matrix for non-missing observations.
#'   - W.post.full: The full row-normalized spatial interaction adjacency matrix.
#'   - Wstar: The spatial weights matrix for the Moran's I statistic.
#'   - Wstar.eigen: The eigenvalues and eigenvectors of the Wstar matrix.
#'   - Wstar.eigen_vals: The eigenvalues of the Wstar matrix.
#'   - det.Q: The determinant of the precision matrix Q.
#'   - Y: The response vector with missing values filled.
#'   - missing_indexes: The indices of missing values in the response vector.
#'   - Y.da: A matrix of imputed values for the missing responses.
#' @export
#' 
init_chain <- function(n.iterations, n.locations.all, p, n.trees, n, X, Y, missing_indexes, SIAM, W, rho) {
  source('R/common/format_w_matrix.R')
  # TODO: Document better, with the references and etc.
  missing_indexes <- which(is.na(Y))

  # Initialize samples for sigma2, rho, and tau2
  sigma2.samples <- rep(0.1, 1)        
  rho.samples <- rep(0, n.iterations)
  tau2.samples <- rep(0, n.iterations)

  # Initialize spatial theta and covariate selection vector
  spatial_theta <- rep(0, n.locations.all)
  cov_sel <- rep(0, p)

  # Initialize lists for observation indexes and decision tree structures
  obs_list.ind <- list()
  dt_list <- list()
  for (ii in 1:n.trees) {
    obs_list.ind[[ii]] <- 1:n
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
  trees <- matrix(0, nrow = n, ncol = n.trees)
  trees.pred <- matrix(0, nrow = n.locations.all, ncol = n.trees)

  # Initialize lists for covariate indexes
  Xlist <- Xmult <- list()
  for (i in 1:p) {
    Xlist[[i]] <- X[-missing_indexes, i]
    Xmult[[i]] <- X[, i]
  }
  Xmult[[p + 1]] <- 1:(n.locations.all)
  X.unique <- lapply(1:p, function(t) sort(unique(X[-missing_indexes, t])))

  # Initialize W matrix and related parameters
  W_sel <- 1
  W_sel.samples <- NULL
  W.count <- length(W)
  lapply(W, function(x) diag(x) <- 0)
  W.siam <- SIAM
  for (i in 1:W.count) {
    W.siam <- W.siam * W[[i]] ^ I(W_sel == i)
  }
  W.siam.full <- W.siam
  W.siam <- W.siam[-missing_indexes, -missing_indexes]
  rownames(W.siam) <- 1:(n.locations.all - length(missing_indexes))
  colnames(W.siam) <- 1:(n.locations.all - length(missing_indexes))
  W.post <- format_w_matrix(W.siam)
  W.post.full <- format_w_matrix(W.siam.full)

  # Compute eigenvalues of Wstar
  Wstar <- diag(apply(W.siam, 1, sum)) - W.siam
  Wstar.eigen <- eigen(Wstar)
  Wstar.eigen_vals <- Wstar.eigen$values
  det.Q <- 0.5 * sum(log((rho * Wstar.eigen_vals + (1 - rho))))

  # TODO: Separate concerns, Y.da should be in predict
  # Fill missing values in Y with normal random values
  Y.da <- matrix(nrow=length(missing_indexes), ncol=n.iterations)
  set.seed(1)
  Y.da[,1] <- rnorm(length(missing_indexes), mean(Y, na.rm=TRUE), 1)
  Y[missing_indexes] <- Y.da[,1]

  return(
    list(
        sigma2.samples = sigma2.samples,
        rho.samples = rho.samples,
        tau2.samples = tau2.samples,
        spatial_theta = spatial_theta,
        cov_sel = cov_sel,
        obs_list.ind = obs_list.ind,
        dt_list = dt_list,
        trees = trees,
        trees.pred = trees.pred,
        Xlist = Xlist,
        Xmult = Xmult,
        X.unique = X.unique,
        W_sel = W_sel,
        W_sel.samples = W_sel.samples,
        W.count = W.count,
        W.siam = W.siam,
        W.siam.full = W.siam.full,
        W.post = W.post,
        W.post.full = W.post.full,
        Wstar = Wstar,
        Wstar.eigen = Wstar.eigen,
        Wstar.eigen_vals = Wstar.eigen_vals,
        det.Q = det.Q,
        Y = Y,
        missing_indexes = missing_indexes,
        Y.da = Y.da
        )
    )
}