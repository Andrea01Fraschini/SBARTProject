#' Change Tree Structure
#'
#' This function attempts to change the structure of a decision tree by proposing a new split rule for a randomly selected internal node. 
#' The new tree structure is accepted or rejected based on a log-likelihood ratio test.
#'
#' @param sigma2 The error variance.
#' @param sigma_mu The variance of the leaf parameters (mu_ij).
#' @param dt A list representing the decision tree.
#' @param residuals A vector of residuals (R).
#' @param prop.prob A vector of proposal probabilities for covariates (from Dirichlet).
#' @param obs A vector of indices of observations.
#' @param x.list A list of all covariates.
#' @param xcut A partition of the predictor space.
#' @param n.available The number of available observations.
#' @param prob.grow The probability of growing the tree.
#' @param prob.change The probability of changing the tree.
#' @param prob.prune The probability of pruning the tree.
#' @param alpha The depth regularization parameter.
#' @param beta The regularization parameter.
#'
#' @return A list containing the following elements:
#'   - dt: The updated decision tree.
#'   - obs: The updated vector of observation indices.
#'
#' @examples
#' # Assuming all the required parameters are defined...
#' result = CHANGE(sigma2, sigma_mu, dt, residuals, prop.prob, obs, x.list, xcut, n.available, prob.grow, prob.change, prob.prune, alpha, beta)
#' @export
#'
CHANGE <- function(
    sigma2, # error variance 
    sigma_mu, # leaf parameters (mu_ij) variance
    dt, # list of decision trees
    residuals, # vector of residuals (R)
    prop.prob, # proposal probability for covariates (from Dirichlet)
    obs, # indexes of observations 
    x.list, # list of all covariates 
    xcut, # partition of the predictor space 
    n.available, # number of available observations 
    prob.grow, 
    prob.change,
    prob.prune,
    alpha, # depth regularization params
    beta 
) {
    n <- n.available
    p <- length(x.list) # number of covariates

    terminal_nodes <- which(dt$terminal)
    singly_nodes <- which(table(dt$parent[terminal_nodes]) == 2) # nodes with 2 child leaves 
    singly.position <- as.numeric(names(singly_nodes))
    singly.intern_nodes <- ifelse(length(singly.position) == 1, singly.position, sample(singly.position, 1)) # pick a singly internal parent node

    subset.ind <- which(dt$position == singly.intern_nodes) # select picked node index 
    begin <- dt$begin[subset.ind]
    end <- dt$end[subset.ind]
    obs.ind <- obs[begin:end]
    enough.unique <- which(mapply(function(x) length(unique(x.list[[x]][obs[begin:end]])), 1:p) >= 2) # indexes of covariates with enough unique values 
    prop.split_var <- sample(enough.unique, 1, replace = FALSE, prob = prop.prob[enough.unique])

    unique.len <- length(unique(x.list[[prop.split_var]][obs[begin:end]])) # number of unique values 
    prop.split_rule <- value <- sort(unique(x.list[[prop.split_var]][obs.ind]))[sample(1:(unique.len - 1), 1) + 1]

    obs.left.star <- obs.ind[which(x.list[[prop.split_var]][obs.ind] < value)]
    obs.right.star <- setdiff(obs.ind, obs.left.star)

    leaves.left <- which(dt$position == 2 * singly.intern_nodes)
    leaves.right <- which(dt$position == 2 * singly.intern_nodes + 1)
    obs.left <- obs[dt$begin[leaves.left]:dt$end[leaves.left]]
    obs.right <- obs[dt$begin[leaves.right]:dt$end[leaves.right]]

    # Likelihood ratio (log scale)
    n.left <- length(obs.left)
    n.right <- length(obs.right)
    n.left.star <- length(obs.left.star)
    n.rigth.star <- length(obs.right.star)

    sigma_ratio <- sigma2 / sigma_mu
    log1.temp <- log(sqrt((sigma_ratio + n.left) * (sigma_ratio + n.right)))
    log2.temp <- log(sqrt((sigma_ratio + n.left.star) * (sigma_ratio + n.rigth.star)))
    res_sum.left <- sum(residuals[obs.left])
    res_sum.right <- sum(residuals[obs.right])
    res_sum.left.star <- sum(residuals[obs.left.star])
    res_sum.right.star <- sum(residuals[obs.right.star])
    
    # UNKNOWN: don't know what 0.5 is 
    log_lh_ratio <- log1.temp - log2.temp + (0.5 / sigma2 * (res_sum.left.star^2 / (n.left.star + sigma_ratio) + res_sum.right.star^2 / (n.rigth.star + sigma_ratio) - res_sum.left^2 / (n.left + sigma_ratio) - res_sum.right^2 / (n.right + sigma_ratio)))

    accept.logprob <- log_lh_ratio

    if (accept.logprob > log(runif(1))) {
        # accept new tree structure
        dt.new <- dt 
        subset.ind <- which(dt.new$position == singly.intern_nodes) # parent node
        dt.new$terminal[subset.ind] <- FALSE
        dt.new$split[subset.ind] <- prop.split_var
        dt.new$value[subset.ind] <- which(xcut[[prop.split_var]] == prop.split_rule)

        subset.ind <- which(dt.new$position == 2 * singly.intern_nodes) # left leaf
        dt.new$begin[subset.ind] <- begin
        dt.new$end[subset.ind] <- begin + n.left.star - 1

        subset.ind <- which(dt.new$position == 2 * singly.intern_nodes + 1) # right leaf
        dt.new$begin[subset.ind] <- begin + n.left.star
        dt.new$end[subset.ind] <- end

        obs.new <- obs
        obs.new[begin:(begin + n.left.star - 1)] <- sort(obs.left.star)
        obs.new[(begin + n.left.star):end] <- sort(obs.right.star)

        return(list(dt = dt.new, obs = obs.new))
    }

    # reject new tree structure 
    return(list(dt = dt, obs = obs))
}