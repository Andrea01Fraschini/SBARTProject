Mean.sample <- function(sigma2, sigma_mu, dt, obs, residuals, xcut, n.available) {
    n <- n.available
    T <- rep(0, n) # assignment of obs through trees  

    terminal_nodes <- which(dt$terminal)
    
    # sample mean for leaf nodes 
    for (i in 1:length(terminal_nodes)) {
        obs.ind <- obs[(dt$begin[terminal_nodes[i]]):(dt$end[terminal_nodes[i]])]
        var <- 1 / (1 / sigma_mu + length(obs.ind) / sigma2)
        mean <- var * (sum(residuals[obs.ind]) / sigma2)

        T[obs.ind] <- dt$mu[terminal_nodes[i]] <- rnorm(1, mean, sqrt(var))
    }

    return(list(T = T, dt = dt))
}