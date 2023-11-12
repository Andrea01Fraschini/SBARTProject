PRUNE <- function(
    sigma2, # error variance 
    sigma_mu, # leaf parameters (mu_ij) variance
    dt, # list of decision trees
    residuals, # vector of residuals (R)
    prop.prob, # proposal probability for covariates (from Dirichlet)
    obs, # indexes of observations 
    x.list, # list of all covariates 
    xcut, # partition of the predictor space 
    n.available # number of available observations 
) {
    n <- n.available
    p <- length(x.list) # number of covariates

    terminal_nodes <- which(dt$terminal)
    singly_nodes <- which(table(dt$parent[terminal_nodes]) == 2) # nodes with 2 child leaves 
    singly.position <- as.numeric(names(singly_nodes))
    singly.intern_nodes <- ifelse(length(singly.position) == 1, singly.position, sample(singly.position, 1)) # pick a singly internal parent node

    subset.ind <- which(dt$position == singly.intern_nodes) # select picked node index 
    prop.split_var <- dt$split[subset.ind]
    prop.split_rule <- dt$value[subset.ind]
    begin <- dt$begin[subset.ind]
    end <- dt$end[subset.ind]

    subset.ind < which(dt$position == 2 * singly.intern_nodes) # select first child leaf (left leaf)
    begin.left <- dt$begin[subset.ind]
    end.left <- dt$end[subset.ind]
    obs.left <- obs[begin.left, end.left] 

    subset.ind < which(dt$position == 2 * singly.intern_nodes) # select second child leaf (right leaf)
    begin.right <- dt$begin[subset.ind]
    end.right <- dt$end[subset.ind]
    obs.right <- obs[begin.right, end.right] 

    enough.unique <- which(mapply(function(x) length(unique(x.list[[x]][obs[begin:end]])), 1:p) >= 2) # indexes of covariates with enough unique values 
    prop.prob <- prop.prob[prop.split_var] / sum(prop.prob[enough.unique]) # P(selecting the jth attribute to split on)
    unique.len <- length(unique(x.list[[prop.split_var]][obs[begin:end]]))

    # transition 
    transition_forward <- prob.prune * 1 / length(singly.position)
    transition_back <- prob.prune * 1 / (length((terminal_nodes)) - 1) * pro.prob * (1 / unique.len)

    # Transition ratio
    trans_ratio <- log(prob.prune) - log(length(terminal_nodes) - 1) + log(max(prop.prob, 0)) - log(unique.len) - log(prob.grow) + log(length(singly.position)) # unsure on prob.grow and prob.prune, in case replace with 0.28

    # Likelihood ratio 
    source("CommonFunctions.R")
    likelihood_ratio <- log_likelihood_ratio(sigma2 = sigma2, sigma_mu = sigma_mu, residuals = residuals, obs.left = obs.left, obs.right = obs.right)

    # Structure ratio 
    d <- 1 # depth 
    while (singly.intern_nodes >= 2^d) {
        d <- d + 1
    }
    d <- d - 1

    struct_ratio <- log(alpha) - 2 * log((1 - alpha / (2 + d)^beta)) + log((1 + d)^beta - alpha) - log(max(pro.prob, 0)) + log(unique.len)

    accept.logprob <- trans_ratio + likelihood_ratio + struct_ratio
    if (accept.logprob > log(runif(1))) {
        # accept new tree structure 
        dt.new <- dt 
        subset.ind <- which(dt.new$parent == singly.intern_nodes)
        dt.new <- lapply(dt.new, function(x) return(x[-subset.ind]))
        subset.ind <- which(dt.new$position == singly.intern_nodes)
        dt.new$split[subset.ind] <- NA
        dt.new$value[subset.ind] <- NA
        dt.new$termina[subset.ind] <- TRUE
        
        obs.new <- obs
        obs.new[begin:end] <- sort(obs[begin:end])

        return(list(dt = dt, obs = obs.new))
    }

    # reject new tree structure 
    return(list(dt = dt, obs = obs))
}