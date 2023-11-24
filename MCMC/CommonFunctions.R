# likelihood ratio in log scale used for GROW and PRUNE
log_likelihood_ratio <- function(sigma2, sigma_mu, residuals, obs.left, obs.right) {
    n.left <- length(obs.left)
    n.right <- length(obs.right)

    # temp variables for computing likelihood
    term.all <- sigma2 + (n.left + n.right) * sigma_mu
    term.left <- sigma2 + n.left * sigma_mu
    term.right <- sigma2 + n.right * sigma_mu
    res_sum.all <- sum(residuals[union(obs.left, obs.right)])
    res_sum.left <- sum(residuals[obs.left])
    res_sum.right <- sum(residuals[obs.right])

    log_lh_ratio <- log(sqrt(sigma2 * term.all) / sqrt(term.left * term.right)) + (sigma_mu / (2 * sigma2) * (res_sum.left^2 / term.left + res_sum.right^2 / term.right) - res_sum.all^2 / term.all)
    return(log_lh_ratio)
}

# format the W matrix in the triplet format (https://coin-or.github.io/Ipopt/IMPL.html)
formatWMatrix <- function(W) {
    # check matrix dimension
    if(!is.matrix(W)) stop("W is not a matrix.", call.=FALSE)
    
    n.rows <- nrow(W)
    if(ncol(W)!= n.rows) stop("W is not a square matrix.", call.=FALSE)    
    
    ## check validity of inputed W matrix
    if(sum(is.na(W)) > 0) stop("W has missing 'NA' values.", call.=FALSE)
    if(!is.numeric(W)) stop("W has non-numeric values.", call.=FALSE)
    if(min(W) < 0) stop("W has negative elements.", call.=FALSE)
    if(sum(W != t(W)) > 0) stop("W is not symmetric.", call.=FALSE)

    library(reshape)
    temp <- melt(t(W))
    temp <- temp[, c(2, 1, 3)]
    # remove all zeros 
    W.triplet <- subset(temp, value > 0) 
    
    names(W.triplet) <- NULL 
    
    W.triplet <- as.matrix(W.triplet)
    n.triplet <- nrow(W.triplet)
    W.triplet.sum <- tapply(W.triplet[, 3], W.triplet[, 1], sum)
    n.neighbours <- tapply(W.triplet[, 3], W.triplet[, 1], length)

    # create the start and finish points for W updating 
    W.begfin <- array(NA, c(n.rows, 2))
    temp <- 1 
    for (i in 1:n.rows) {
        W.begfin[i, ] <- c(temp, (temp + n.neighbours[i] - 1))
        temp <- temp + n.neighbours[i]
    }

    # return the critical quantities
    results <- list(
        W = W, 
        W.triplet = W.triplet, 
        n.triplet = n.triplet, 
        W.triplet.sum = W.triplet.sum, 
        n.neighbours = n.neighbours, 
        W.begfin = W.begfin, 
        n.rows = n.rows
    ) 

    return(results)
}
