#' Initialize MCMC chains for Bayesian spatial model
#'
#' @param n.iterations Number of iterations for the MCMC chain
#' @param n.locations.all Total number of locations
#' @param p Number of covariates
#' @param n.trees Number of decision trees
#' @param n Number of observations
#' @param X Matrix of covariates
#' @param missing_indexes Indexes of missing observations
#' @param SIAM Spatial interaction adjacency matrix
#' @param W List of weight matrices for spatial interaction
#'
#' @return A list containing initialized MCMC chains and other parameters
#'
#' @examples
#' initialize_mcmc_chains(1000, 50, 3, 10, 500, X, missing_indexes, SIAM, W)
#' 
initialize_mcmc_chains <- function(n.iterations, n.locations.all, p, n.trees, n, X, missing_indexes, SIAM, W) {

  # TODO: Document better, with the references and etc.

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
  trees.pred <- matrix(0, nrow = n, ncol = n.trees)

  # Initialize lists for covariate indexes
  Xlist <- Xmult <- list()
  for (i in 1:p) {
    Xlist[[i]] <- X[-missing_indexes, i]
    Xmult[[i]] <- X[, i]
  }
  Xmult[[p + 1]] <- 1:(n)
  X.unique <- lapply(1:p, function(t) sort(unique(X[, t])))

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
  W.siam <- W.siam[-missing_indexes, - missing_indexes]
  rownames(W.siam) <- 1:(n.locations.all - length(missing_indexes))
  colnames(W.siam) <- 1:(n.locations.all - length(missing_indexes))
  W.post <- formatWMatrix(W.siam)
  W.post.full <- formatWMatrix(W.siam.full)

  # Compute eigenvalues of Wstar
  Wstar <- diag(apply(W.siam, 1, sum)) - W.siam
  Wstar.eigen <- eigen(Wstar)
  Wstar.eigen_vals <- Wstar.eigen$values
  det.Q <- 0.5 * sum(log((rho * Wstar.eigen_vals + (1 - rho))))

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
        det.Q = det.Q
        )
    )
}