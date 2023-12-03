#' Grow Decision Tree at Root
#'
#' This function attempts to grow the decision tree at the root by proposing a new split rule. 
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
#' result = GROW.root(sigma2, sigma_mu, dt, residuals, prop.prob, obs, x.list, xcut, n.available, prob.grow, prob.change, prob.prune, alpha, beta)
#' @export
#'
GROW.root <- function(
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
    source("R/common/log_likelihood_ratio.R")

    n <- n.available
    p <- length(x.list) # number of covariates

    prop.split_var <- sample(1:p, 1, replace = FALSE, prob = prop.prob) # pick a predictor (splitting variable)
    prop.split_rule <- sample(2:length(xcut[[prop.split_var]]), 1) # pick a splitting value for the rule  
    value <- xcut[[prop.split_var]][prop.split_rule]
    
    obs.left <- which(x.list[[prop.split_var]] < value) # observations < prop.split_rule
    obs.right <- setdiff(1:n, obs.left) # observations >= prop.split_rule 

    # Transition ratio (log scale) 
    trans_ratio <- log(prob.prune) - log(max(prop.prob[prop.split_var], 0)) + log(length(xcut[[prop.split_var]]) - 1) - log(prob.grow) 

    # Likelihood ratio (log scale)
    likelihood_ratio <- log_likelihood_ratio(sigma2 = sigma2, sigma_mu = sigma_mu, residuals = residuals, obs.left = obs.left, obs.right = obs.right)


    # Structure ratio (log scale)
    d <- 0 # depth for root 
    struct_ratio <- log(alpha) + 2 * log((1 - alpha / (2 + d)^beta)) - log((1 + d)^beta - alpha) + log(max(prop.prob[prop.split_var], 0)) - log(length(xcut[[prop.split_var]]) - 1)

    accept.logprob <- trans_ratio + likelihood_ratio + struct_ratio

    if (accept.logprob > log(runif(1))) {
        # New tree structure 
        dt.new <- dt 
        dt.new$split <- prop.split_var
        dt.new$terminal <- FALSE # root is no longer terminal 
        dt.new$value <- prop.split_rule 
        dt.add <- list(
            position = c(2, 3), 
            parent = c(1, 1), # root 
            terminal = rep(TRUE, 2), # both new nodes are terminal 
            split = rep(NA, 2), 
            value = rep(NA, 2), 
            mu = rep(NA, 2), 
            begin = c(1, length(obs.left) + 1), # begin of included residuals 
            end = c(length(obs.left), n) # end of included residuals  
        )
        dt.new <- mapply(c, dt.new, dt.add, SIMPLIFY = FALSE)

        obs.new <- obs
        obs.new[1:length(obs.left)] <- sort(obs.left)
        obs.new[(length(obs.left) + 1):n] <- sort(obs.right)

        return(list(dt = dt.new, obs = obs.new))
    }

    # reject new structure 
    return(list(dt = dt, obs = obs))
}

#' Grow Decision Tree
#'
#' This function attempts to grow the decision tree by proposing a new split rule for a randomly selected terminal node. 
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
#' @export
#' 
GROW <- function(
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
    prop.term_node <- ifelse(length(terminal_nodes) == 1, terminal_nodes, sample(terminal_nodes, 1)) # pick only one terminal node 
   
    begin <- dt$begin[prop.term_node]
    end <- dt$end[prop.term_node]
    obs.selected_indexes <- obs[begin:end] # pick all observations (indexes) associated with the node
   
    if (length(obs.selected_indexes) < 2) { # not enough unique values 
        return(list(dt = dt, obs = obs)) # return the current tree structure 
    }

    enough.unique <- which(mapply(function(x) length(unique(x.list[[x]][obs.selected_indexes])), 1:p) >= 2) # indexes of covariates with enough unique values 
    prop.split_var <- sample(enough.unique, 1, replace = FALSE, prob = prop.prob[enough.unique]) # pick a predictor 

    x.prop.split_var <- x.list[[prop.split_var]][obs.selected_indexes]
    xcut.prop.split_var <- xcut[[prop.split_var]]
    unique.vals <- unique(x.prop.split_var)
    unique.len <- length(unique.vals) # number of unique values for proposed predictor 
    prop.split_rule <- sort(unique.vals)[sample(1:(unique.len - 1), 1) + 1]
    prop.split_rule <- which(xcut.prop.split_var == prop.split_rule)

    obs.left <- obs.selected_indexes[which(x.prop.split_var < xcut.prop.split_var[prop.split_rule])] # observations < prop.split_rule
    obs.right <- setdiff(obs.selected_indexes, obs.left) # observations >= prop.rule 

    # New tree structure 
    dt.new <- dt
    dt.new.position <- dt.new$position[prop.term_node]
    dt.new$termina[prop.term_node] <- FALSE
    dt.add <- list(
        position = c(2 * dt.new.position, 2 * dt.new.position + 1), 
        parent = rep(dt.new.position, 2), 
        terminal = rep(TRUE, 2), 
        split = rep(NA, 2), 
        value = rep(NA, 2), 
        mu = rep(NA, 2), 
        begin = c(begin, begin + length(obs.left)), 
        end = c(begin + length(obs.left) - 1, end)
    )
    dt.new <- mapply(c, dt.new, dt.add, SIMPLIFY = FALSE)

    # find internal nodes with two terminal child nodes 
    nodes <- which(table(dt.new$parent[which(dt.new$terminal)]) == 2)
    count <- length(nodes) 

    # Transition ratio (log scale) 
    trans_ratio <- log(prob.prune) + log(length(terminal_nodes)) - log(max(prop.prob[prop.split_var] / sum(prop.prob[enough.unique]), 0)) + log((unique.len - 1)) - log(prob.grow) - log(count) # unsure on prob.grow and prob.prune, in case replace with 0.28

    # Likelihood ratio (log scale)
    source("CommonFunctions.R")
    likelihood_ratio <- log_likelihood_ratio(sigma2 = sigma2, sigma_mu = sigma_mu, residuals = residuals, obs.left = obs.left, obs.right = obs.right)

    # Structure ratio (log scale)
    d <- 1 # depth 
    while (dt$position[prop.term_node] >= 2^d) {
        d <- d + 1
    }
    d <- d - 1
    struct_ratio <- log(alpha) + 2 * log((1 - alpha / (2 + d)^beta)) - log((1 + d)^beta - alpha) + log(max(prop.prob[prop.split_var] / sum(prop.prob[enough.unique]), 0)) - log(unique.len - 1)

    accept.logprob <- trans_ratio + likelihood_ratio + struct_ratio

    if (accept.logprob > log(runif(1))) {
        # accept new structure
        dt.new$split[prop.term_node] <- prop.split_var
        dt.new$value[prop.term_node] <- prop.split_rule

        obs.new <- obs
        obs.new[begin:(begin + length(obs.left) - 1)] <- sort(obs.left)
        obs.new[(begin + length(obs.left)):end] <- sort(obs.right)

        return(list(dt = dt.new, obs = obs.new))
    }

    # reject new structure 
    return(list(dt = dt, obs = obs))
}
