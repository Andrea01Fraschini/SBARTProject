sbart <- function(
    X, # matrix of covariates
    y.train, # vector of observations 
    W, # array of weighted adjacency matrices
    SIAM, # Structurally Informed Adjacency Matrix specification
    missing_indexes, # indexes of missing observations in matrices 
    n.trees = 50L, # number of trees (integer)
    n.iterations = 10000L, # number of MCMC iteriations  
    n.burnin = 5000L, # number of iterations to discard as burn-in 
    thin = 10L, # thin factor 
    warmup = n.iterations / 10 # iterations warmup period 
) {
    library(MASS)
    library(mnormt)
    library(MCMCpack)
    library(rootSolve)
    library(R.matlab)
    library(truncnorm)
    library(data.table)
    library(Rcpp)
    sourceCpp("MCMC/CARBayes.cpp") # this C++ code from CARBayes
    source("MCMC/GROW.R")
    source("MCMC/CHANGE.R")
    source("MCMC/PRUNE.R")
    source("MCMC/CommonFunctions.R") # TODO: fix 
    source("MCMC/Prediction.R") # TODO: fix    
    source("MCMC/SampleMean.R") # TODO: fix    
   
    p <- dim(X)[2] # dimension of covariates
    n <- length(y.train) # number of observations 
    n.locations.all <- dim(SIAM)[1]

    # initialize model parameters
    # perturbation probabilities
    prob.grow <- 0.28
    prob.prune <- 0.28 
    prob.change <- 0.44

    # depth regulating prior hyperparameters 
    alpha <- 0.95
    beta <- 2
    
    # selection probabilities for covariates 
    dirichlet.alpha <- 1
    posterior.dirichlet.alpha <- rep(1, p) 
    cov.sel_prob <- rdirichlet(1, rep(dirichlet.alpha, p))

    # hyperparameters for spatial random effect 
    tau2.a <- 1
    tau2.b <- 0.01 
    tau2.posterior.shape <- tau2.a + n / 2
    tau2 <- 0.5 
    proposal.sd.rho <- 0.2 
    rho <- 0.5
    a0 <- 0.5 
    b0 <- 1
    
    # shift the mean of Y 
    shift.amount <- mean(y.train)
    y <- y.train - shift.amount 
    residuals <- y # initial value for residuals 

    # sigma prior hyperparameters 
    sigma2 <- var(y) # initial value for sigma2
    nu <- 3 
    q <- 0.90 
    sigma.quantile <- function(lambda) invgamma::qinvgamma(q, nu / 2, rate = lambda * nu / 2, lower.tail = TRUE, log.p = FALSE) - sqrt(sigma2)
    lambda <- uniroot.all(sigma.quantile, c(0.1 ^ 5, 10))
    sigma2.a <- nu / 2
    sigma2.b <- nu * lambda / 2
    k <- 2 
    sigma_mu <- max(
        (min(y) / (-k * sqrt(n.trees))) ^ 2, 
        (max(y) / (+k * sqrt(n.trees))) ^ 2
    )

    sigma2.samples <- rep(0.1, 1)
    rho.samples <- rep(0, n.iterations) 
    tau2.samples <- rep(0, n.iterations)

    spatial_theta <- rep(0, n.locations.all)
    cov_sel <- rep(0, p) # selected covariates (1 if selected, 0 otherwise)

    obs_list.ind <- list() # list of observations indexes
    dt_list <- list() # list of current decision trees structures
    for (ii in 1:n.trees) {
        obs_list.ind[[ii]] <- 1:n
        # initialize all structures with root nodes 
        dt_list[[ii]] <- list(
            position = 1, 
            parent = NA, 
            terminal = FALSE, 
            split = NA,
            value = NA, 
            mu = NA,
            begin = 1,
            end = n
        )
    }

    trees <- matrix(0, nrow = n, ncol = n.trees) # g(x; Tt, Mt) values associated to covariates through trees 
    trees.pred <- matrix(0, nrow = n, ncol = n.trees) # predictions made by trees

    Xlist <- Xmult <- list() # list of covariates indexes
    for (i in 1:p) {
        Xlist[[i]] <- X[-missing_indexes, i]
        Xmult[[i]] <- X[, i] # TODO: are these the predictors for making predictions on missing data?
    }
    Xmult[[p + 1]] <- 1:(n)
    X.unique <- lapply(1:p, function(t) sort(unique(X[, t]))) # unique predictor values 

    W_sel <- 1 # slected index for proposed W matrix 
    W_sel.samples <- NULL # track which functions in Fd have been proposed 
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
    W.post <- formatWMatrix(W.siam)
    W.post.full <- formatWMatrix(W.siam.full)

    Wstar <- diag(apply(W.siam, 1, sum)) - W.siam
    Wstar.eigen <- eigen(Wstar)
    Wstar.eigen_vals <- Wstar.eigen$values 
    det.Q <- 0.5 * sum(log((rho * Wstar.eigen_vals + (1 - rho))))

    # =============================================================================================
    # Run MCMC
    for (j in 2:n.iterations) {
        for (t in 1:n.trees) {
            residuals <- y - rowSums(trees[, -t]) - spatial_theta[-missing_indexes]
            
            print(t) # TODO: remove 
            
            # Find depth of the tree 
            tree.depth <- length(dt_list[[t]]$position)
            
            step <- ifelse(tree.depth == 1, # check if root node
                1, # GROW.root 
                sample(2:4, 1, prob = c(prob.grow, prob.prune, prob.change)) # Pick a perturbation 
            )

            perturbation <- switch(
                step, # selection var 
                GROW.root,
                GROW,
                PRUNE, 
                CHANGE,
            )
            
            # apply perturbation to trees 
            result <- perturbation(
                sigma2 = sigma2.samples[j - 1],
                sigma_mu = sigma_mu,
                dt = dt_list[[t]],
                residuals = residuals, 
                prop.prob = cov.sel_prob, 
                obs = obs_list.ind[[t]], 
                x.list = Xlist,
                xcut = X.unique, 
                n.available = n,
                prob.grow = prob.grow, 
                prob.change = prob.change,
                prob.prune = prob.prune,
                alpha = alpha, 
                beta = beta
            )  

            dt_list[[t]] <- result$dt
            obs_list.ind[[t]] <- result$obs

            mean.samples <- Mean.sample(
                sigma2 = sigma2.samples[j - 1], 
                sigma_mu = sigma_mu, 
                dt = dt_list[[t]], 
                obs = obs_list.ind[[t]], 
                residuals = residuals, 
                n.available = n
            )
            trees[,t] <- mean.samples$T
            teeemp <- mean.samples$dt
            dt_list[[t]] <- teeemp
        }

        # Sample variance parameter
        Rfinal <- y - rowSums(trees) - spatial_theta[-missing_indexes]
        sigma2.samples[j] <- rinvgamma(1, sigma2.a + n / 2, scale = sigma2.b + sum((Rfinal)^2) / 2)
    
        # update of spatial effect 
        offset <- (y - rowSums(trees.pred))
        spatial_theta <- gaussiancarupdate(
            Wtriplet = W.post.full$W.triplet,
            Wbegfin = W.post.full$W.begfin, 
            W.post.full$W.triplet.sum, 
            nsites = n.locations.all,
            phi = spatial_theta, 
            tau2 = tau2, 
            rho = rho, 
            nu2 = sigma2.samples[j], 
            offset = offset
        )
        spatial_theta <- spatial_theta - mean(spatial_theta)

        # browser() # TODO: remove

        temp <- quadform(
            as.matrix(W.post.full$W.triplet),
            W.post.full$W.triplet.sum, 
            W.post.full$n.triplet, 
            n.locations.all, 
            spatial_theta,
            spatial_theta,
            rho
        )
        tau2.posterior.scale <- tau2.b + temp 
        tau2 <- 1 / rgamma(1, tau2.posterior.shape, scale = (1 / tau2.posterior.scale)) 
        tau2.samples[j] <- tau2 

        # update rho based on Metropolis Hastings step 
        proposal.rho <- rtruncnorm(n = 1, a = 0, b = 1, mean = rho, sd = proposal.sd.rho)
        temp2 <- quadform(
            as.matrix(W.post.full$W.triplet),
            W.post.full$W.triplet.sum, 
            W.post.full$n.triplet, 
            n.locations.all, 
            spatial_theta,
            spatial_theta,
            proposal.rho
        )
        det.Q.proposal <- 0.5 * sum(log(proposal.rho * Wstar.eigen_vals + (1 - proposal.rho)))
        logprob.current <- det.Q - temp / tau2
        logprob.proposal <- det.Q.proposal - temp2 / tau2 
        logprob.hastings <- log(dtruncnorm(x = rho, a = 0, b = 1, mean = proposal.rho, sd = proposal.sd.rho)) -
                            log(dtruncnorm(x = proposal.rho, a = 0, b = 1, mean = rho, sd = proposal.sd.rho))
        
        accept.prob <- exp(logprob.proposal - logprob.current + logprob.hastings)
        
        if (accept.prob > runif(1)) {
            rho <- proposal.rho
            det.Q <- det.Q.proposal
            temp <- temp2
        }
        rho.samples <- rho
        
        # update f based on Metropolis Hastings step 
        proposal.W_sel <- sample(1:W.count, 1)
        W.siam.proposal <- SIAM 
        for (i in 1:W.count) {
            W.siam.proposal <- W.siam.proposal * W[[i]] ^ I(proposal.W_sel == i)
        }
        rownames(W.siam.proposal) <- 1:(n.locations.all)
        colnames(W.siam.proposal) <- 1:(n.locations.all)
        W.post.proposal <- formatWMatrix(W.siam.proposal)
    
        temp3 <- quadform(
            as.matrix(W.post.proposal$W.triplet),
            W.post.proposal$W.triplet.sum, 
            W.post.proposal$n.triplet, 
            n.locations.all, 
            spatial_theta,
            spatial_theta,
            rho
        )
        
        Wstar.proposal <- diag(apply(W.siam.proposal, 1, sum)) - W.siam.proposal
        Wstar.eigen.proposal <- eigen(Wstar.proposal)
        Wstar.eigen_vals.proposal <- Wstar.eigen.proposal$values

        det.Q.proposal <- 0.5 * sum(log((rho * Wstar.eigen_vals.proposal + (1 - rho))))
        logprob.current <- det.Q - temp / tau2
        logprob.proposal <- det.Q.proposal - temp3 / tau2

        accept.prob <- exp(logprob.proposal - logprob.current)
    
        if (accept.prob > runif(1)) {
            W_sel <- proposal.W_sel
            det.Q <- det.Q.proposal
            W.siam.full <- W.siam.proposal
            W.post.full <- W.post.proposal
            Wstar <- Wstar.proposal
            Wstar.eigen <- Wstar.eigen.proposal
            Wstar.eigen_vals <- Wstar.eigen_vals.proposal
        }
        W_sel.samples[j] <- W_sel

        # browser() # TODO: fix

        # update dirichlet alpha 
        dt.split_vars <- unlist(lapply(dt_list, function(x) x$split))
        # foreach predictor, count how many times it appears in splitting rules 
        rules.count <- as.numeric(table(factor(dt.split_vars[!is.na(dt.split_vars)], levels = 1:p))) 
        if (j < warmup) {
            posterior.dirichlet.alpha <- rep(1, p) + rules.count
        } else {
            # alpha / (alpha + p) ~ Beta(a0, b0)
            proposal.dirichlet.alpha <- max(rnorm(1, dirichlet.alpha, 0.1), 0.1^10)
            sum_s <- log(ifelse(cov.sel_prob < 0.1^300, 0.1^300, cov.sel_prob))
           
            dirichlet_likelihood <- function(x) {
                lik <- sum(sum_s * (rep(x, p) - 1)) +
                       lgamma(sum(rep(x, p))) - 
                       sum(lgamma(rep(x, p)))
                return(lik)
            }

            dirichlet_lik.proposal <- dirichlet_likelihood(proposal.dirichlet.alpha / p)
            dirichlet_lik <- dirichlet_likelihood(dirichlet.alpha / p)
            log_ratio <- dirichlet_lik.proposal + 
                log(
                    (proposal.dirichlet.alpha/(proposal.dirichlet.alpha + p)) ^ (a0 - b0) * 
                    (p / (proposal.dirichlet.alpha + p)) ^ (b - 1) * 
                    abs(1 / (proposal.dirichlet.alpha + p) - proposal.dirichlet.alpha / (proposal.dirichlet.alpha + p) ^ 2)
                ) + dnorm(dirichlet.alpha, proposal.dirichlet.alpha, 0.1, log = TRUE) - 
                dirichlet_lik - 
                log(
                    (dirichlet.alpha / (dirichlet.alpha + p)) ^ (a0 - b0) * 
                    (p / (dirichlet.alpha + p)) ^ (b - 1) * 
                    abs(1 / (dirichlet.alpha + p) - dirichlet.alpha / (dirichlet.alpha + p) ^ 2)
                ) - dnorm(proposal.dirichlet.alpha, dirichlet.alpha, 0.1, log = TRUE)
        
            if (log_ratio > log(runif(1))) {
                dirichlet.alpha <- proposal.dirichlet.alpha
            }
            posterior.dirichlet.alpha <- rep(dirichlet.alpha / p, p) + rules.count
        }

        cov.sel_prob <- rdirichlet(1, posterior.dirichlet.alpha)

        trees.pred <- matrix(unlist(sapply(1:n.trees, function(x) Mean.predict(dt_list[[x]], Xlist, Xmult, X.unique, n))), nrow = n, ncol = n.trees)
        
        # check which variables are in the model 
        cov_sel.temp <- ifelse(rules.count > 0, 1, 0)
        cov_sel <- rbind(cov_sel, cov_sel.temp)
    }

    results <- list(
        covariates_selection_chain = cov_sel, 
        spatial_theta_chain = spatial_theta,
        sigma2_chain = sigma2.samples,
        trees_chain = trees.pred, 
        W_selection_chain = W_sel.samples
    )

    return(results)
}

# Paolo: IDK if it makes sense to have the prections be seperate... 
sbart.predict <- function(sbart.output, X.test, missing_indexes) {
    mean <- rowSums(sbart.output$trees_chain) + spatial
    sigma2 <-sbart.output$sigma2_chain
    
    chains_len <- length(sigma2)
    n.trees <- dim(sbart.output$trees_chain)[2]
    n.missing <- dim(X.test)[1]
    y.missing <- matrix(nrow = n.missing, ncol = chains_len)

    count <- 0
    for (i in 1:chains_len) {
        for (j in 1:n.trees) {
            count <- count + 1
            y.missing[, count] <- rnorm(n.missing, c(mean)[missing_indexes], sigma2[j])
        }
    }
}

