# Sources: bartMachine appendix A (https://arxiv.org/abs/1312.2171), C. Kim (2022) (https://doi.org/10.1080/00949655.2022.2102633) appendix A.

get_terminal_nodes <- function(tree) {
    terminal_nodes <- which(tree$terminal)
    return(terminal_nodes)
}

get_gen2_internal_nodes <- function(tree) {
    terminal_nodes <- get_terminal_nodes(tree)
    gen2_internal_nodes <- which(table(tree$parent[terminal_nodes]) == 2)
    gen2_internal_nodes <- as.integer(names(gen2_internal_nodes)) # necessary due to the fact that table gives names to vector elements 
    return(gen2_internal_nodes) 
}

get_node_depth <- function(tree, node_index) {
    node_position <- tree$position[node_index]
    depth <- floor(log2(node_position))
    return(depth)
}

grow_log_transition_ratio <- function(
    grow_probability, 
    prune_probability,
    current_tree,
    new_tree
) {
    current_terminal_nodes_count <- length(get_terminal_nodes(current_tree))    
    new_gen2_nodes_count <- length(get_gen2_internal_nodes(new_tree))

    # missing the probability of picking a predictor, should simplify
    log_transition_ratio <- log(grow_probability) - log(prune_probability) + log(current_terminal_nodes_count) - log(new_gen2_nodes_count)
    return(log_transition_ratio)
}

grow_log_likelihood_ratio <- function(
    sigma2,
    sigma_mu2,
    residuals, 
    obs_indexes_left, 
    obs_indexes_right
) {
    obs_left_count <- length(obs_indexes_left)
    obs_right_count <- length(obs_indexes_right)
    obs_all_count<- obs_left_count + obs_right_count

    variance_all <- sigma2 + obs_all_count* sigma_mu2
    variance_left <- sigma2 + obs_left_count * sigma_mu2
    variance_right <- sigma2 + obs_right_count * sigma_mu2

    obs_indexes_all <- union(obs_indexes_left, obs_indexes_right)
    all_residuals_sum2 <- sum(residuals[obs_indexes_all])^2
    left_residuals_sum2 <- sum(residuals[obs_indexes_left])^2
    right_residuals_sum2<- sum(residuals[obs_indexes_right])^2

    sqrt_term <- 0.5 * (log(sigma2) + log(variance_all) - log(variance_left) - log(variance_right))
    
    exp_term <- 0.5 * sigma_mu2 / sigma2 * (
        left_residuals_sum2 / variance_left +
        right_residuals_sum2/ variance_right - 
        all_residuals_sum2 / variance_all 
    )

    log_likelihood_ratio <- sqrt_term + exp_term
    return(log_likelihood_ratio)
}

grow_log_structure_ratio <- function(
    alpha, 
    beta,
    node_to_grow_index,
    current_tree
) {
    depth <- get_node_depth(current_tree, node_to_grow_index)
    
    numerator <- 1 - alpha / (2 + depth)^beta
    denominator <-(1 + depth)^beta - alpha

    # missing the probability of picking a predictor, should simplify
    log_structure_ratio <- log(alpha) + 2 * log(numerator) - log(denominator) 
    return(log_structure_ratio)
}

prune_log_transition_ratio <- function(
    grow_probability, 
    prune_probability,
    current_tree
) {
    current_terminal_nodes_count <- length(get_terminal_nodes(current_tree))    
    current_gen2_nodes_count <- length(get_gen2_internal_nodes(current_tree))

    # missing the probability of picking a predictor, should simplify
    log_transition_ratio <- log(prune_probability) - log(grow_probability) + log(current_gen2_nodes_count) - log(current_terminal_nodes_count - 1) 
    return(log_transition_ratio)
}

# This is just the inverse of the likelihood ratio for the GROW proposal
prune_log_likelihood_ratio <- function(
    sigma2,
    sigma_mu2,
    residuals, 
    obs_indexes_left, 
    obs_indexes_right
) {
    log_likelihood_ratio <- -grow_log_likelihood_ratio(sigma2, sigma_mu2, residuals, obs_indexes_left, obs_indexes_right)
    return(log_likelihood_ratio)
}

# This is just the inverse of the structure ratio for the GROW proposal
prune_log_structure_ratio <- function(
    alpha, 
    beta,
    node_to_grow_index,
    current_tree
) {
    log_structure_ratio <- -grow_log_structure_ratio(alpha, beta, node_to_grow_index, current_tree)
    return(log_structure_ratio)
}

# Transition and structure ratios cancel out
change_log_acceptance_ratio <- function(
    sigma2, 
    sigma_mu2,
    obs_indexes_left,
    obs_indexes_right,
    obs_star_indexes_left,
    obs_star_indexes_right
) {
    obs_left_count <- length(obs_indexes_left)
    obs_right_count <- length(obs_indexes_right)
    obs_star_left_count <- length(obs_star_indexes_left)
    obs_star_right_count <- length(obs_star_indexes_right)

    variance_left <- sigma2 + obs_left_count
    variance_right <- sigma2 + obs_right_count
    variance_star.left <- sigma2 + obs_star_left_count
    variance_star.right <- sigma2 + obs_star_right_count

    left_residuals_sum2 <- sum(residuals[obs_indexes_left])
    right_residuals_sum2 <- sum(residuals[obs_indexes_right])
    left_residuals_star_sum2 <- sum(residuals[obs_star_indexes_left])
    right_residuals_star_sum2 <- sum(residuals[obs_star_indexes_right])

    sqrt_term <- 0.5 * (log(variance_left) + log(variance_right) - log(variance_star.left - log(variance_star.right)))

    exp_term <- 0.5 * sigma_mu2 / sigma2 * (
        left_residuals_star_sum2 / variance_star.left + 
        right_residuals_star_sum2 / variance_star.right - 
        left_residuals_sum2 / variance_left - 
        right_residuals_sum2 / variance_right
    )

    log_likelihood_ratio <- sqrt_term + exp_term
    return(log_likelihood_ratio)
}
