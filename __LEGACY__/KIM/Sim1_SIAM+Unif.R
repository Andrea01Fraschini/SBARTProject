Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

#------ Load required libraries
library(MASS)
library(mnormt)
library(MCMCpack)
library(rootSolve)
library(R.matlab)
library(truncnorm)
library(data.table)
library(Rcpp)


#################
# Simulation Data
#################

# Square lattice region
x.easting <- 1:15
x.northing <- 1:15
Grid <- expand.grid(x.easting, x.northing)
K <- nrow(Grid) # num. of locations (15 by 15 = 225)

# Distance and neighborhood matrix
Distance <- distance <- as.matrix(dist(Grid))
diag(Distance) <- 0
W <-array(0, c(K,K))

# Define spatial connections (see Kim (2020+) for more details)
wind_seed1 <- c(1,17,18,32:35, 48:52, 63:69, 79:86, 94:103, 110:120, 125:135, 141:150, 156:165, 172:180, 187:195, 203:210, 218:225)
wind_seed2 <- setdiff(which(lower.tri(matrix(NA,15,15), diag = FALSE)==TRUE), wind_seed1)
wind_seed3 <- setdiff(which(t(lower.tri(matrix(NA,15,15), diag = FALSE))==TRUE), wind_seed1)
wind_mat <- matrix(0, K, K)
for(i in 1:225){
  if(i %in% wind_seed1){wind_mat[i, wind_seed1] <- 1}
  if(i %in% wind_seed2){wind_mat[i, wind_seed2] <- 1}
  if(i %in% wind_seed3){wind_mat[i, wind_seed3] <- 1}
}
# Generate the covariates and response data
mean.vector <- rep(0,15^2)
mean.vector[wind_seed2] <- -2.5
mean.vector[wind_seed3] <- 0.5


for(process in 1:100){
  
  set.seed(process)
  x1 <- rnorm(K, 1*(Grid[,1]/10-1)^2, 1.5)
  x2 <- rnorm(K, mean.vector, 1)
  x3 <- rnorm(K, 0.5, 1.5)
  x.com <- rmnorm(K, c(-1, 1), matrix(c(1.5,0.25,0.25,1.5), 2, 2))
  x4 <- x.com[,1]
  x5 <- x.com[,2]
  
  phi <- rep(0, K)
  phi[wind_seed1] <- rmnorm(1, rep(0, K), 1.5*exp(-0.05 * Distance))[wind_seed1]
  phi[wind_seed2] <- rmnorm(1, rep(0, K), exp(-0.1 * Distance))[wind_seed2]
  phi[wind_seed3] <- rmnorm(1, rep(0, K), 1.5*exp(-0.1 * Distance))[wind_seed3]
  
  theta <- rnorm(K, mean=rep(0,K), sd=0.1)
  
  Y.true <- Y <- 2.5+1.5*x1 + 1.5*x2 - 1*x3 + 1.5*x4 -1.5*x5 + 0.25*x4*x5 + theta + phi
  
  # randomly selected missing locations
  missing <- mis.ind <- sample(1:225, 195, replace=F)
  Y[missing] <- NA
  
  # Total 50 potential predictors
  P <- 50
  n <- 225
  
  cov <- list()
  for(i in 1:45){
    cov[[i]] <- rnorm(n,0,1) # draw x independently from a normal distribution
  }
  Xpred1 <- do.call(cbind, cov)
  
  # Vector of X predictors
  Xpred <- cbind(x1,x2,x3,x4,x5, Xpred1)
  
  
  ####################
  # Set the BART model
  ####################
  
  # Import files for the spatial random effect
  sourceCpp("CARBayes.cpp") # this C++ code from CARBayes
  source("commonfunctions.R")
  
  ### Import Files
  source("PRUNE.R")
  source("CHANGE.R")
  source("Common.R")
  source("Prediction.R")
  source("GROW.R")
  
  P <- dim(Xpred)[2]
  P <- dim(Xpred)[2]  # Num. of predictors
  
  shift <- mean(Y, na.rm=TRUE)
  Y <- Y - shift      # Shifting the mean of Y
  
  A.WEIGHT <- NULL    # uncertainty parameter
  n.full <- length(Y)  # Num. of the locations
  n.complete <- length(which(!is.na(Y)))  # Num. of the locations with observation
  
  
  
  # Initial Setup (priors, initial values and hyper-parameters)
  p.grow <- 0.28            # Prob. of GROW
  p.prune <- 0.28           # Prob. of PRUNE
  p.change <- 0.44          # Prob. of CHANGE
  m <- 50                  # Num. of Trees: default setting 100
  
  sigma2 <- var(Y, na.rm=T)  # Initial value of SD^2
  nu <- 3                   # default setting (nu, q) = (3, 0.90) from Chipman et al. 2010
  f <- function(lambda) invgamma::qinvgamma(0.90, nu/2, rate = lambda*nu/2, lower.tail = TRUE, log.p = FALSE) - sqrt(sigma2)
  lambda <- uniroot.all(f, c(0.1^5,10))
  sigma_mu <- max((min(Y, na.rm=T)/(-2*sqrt(m)))^2, (max(Y, na.rm=T)/(2*sqrt(m)))^2) # sigma_mu based on min/max of Y
  dir.alpha <- 1         # Hyper-parameter on selection probabilities
  
  alpha <- 0.95             # alpha (1+depth)^{-beta} where depth=0,1,2,...
  beta <- 2                 # default setting (alpha, beta) = (0.95, 2)
  
  # Set (hyper-)parameters and initial values
  prior.tau2 <- c(1, 0.01)
  tau2.posterior.shape <- prior.tau2[1] + 0.5*n.full
  proposal.sd.rho <- 0.2
  rho <- 0.5
  spatial <- rep(0, n)
  tau2 <- 0.1
  
  # Weight matrix
  a.weight <- 1 # initial value for a_w parameter in the weight
  dist.mat <- ifelse(distance < 1, 1, distance)
  W1 <- 1/(distance)^0.5
  W2 <- 1/(distance)^1
  W3 <- 1/(distance)^1.5
  W4 <- 1/(distance)^2
  W5 <- exp(-distance)
  diag(W1) <- 0
  diag(W2) <- 0
  diag(W3) <- 0
  diag(W4) <- 0
  diag(W5) <- 0
  #W <- ifelse(W == Inf, 1, W)
  
  W.wind <- wind_mat*(W1^(I(a.weight==1)))*(W2^(I(a.weight==2)))*(W3^(I(a.weight==3)))*(W4^(I(a.weight==4)))*(W5^(I(a.weight==5)))
  W.wind <- W.wind[-mis.ind, -mis.ind]
  rownames(W.wind) <- 1:(n.full-length(mis.ind))
  colnames(W.wind) <- 1:(n.full-length(mis.ind))
  W.post <- common.Wcheckformat(W.wind)
  W.wind.full <- wind_mat*(W1^(I(a.weight==1)))*(W2^(I(a.weight==2)))*(W3^(I(a.weight==3)))*(W4^(I(a.weight==4)))*(W5^(I(a.weight==5)))
  W.post.full <- common.Wcheckformat(W.wind.full)
  
  det.Q <- 0
  Wstar <- diag(apply(W.wind,1,sum)) - W.wind
  Wstar.eigen <- eigen(Wstar)
  Wstar.val <- Wstar.eigen$values
  det.Q <- 0.5 * sum(log((rho * Wstar.val + (1-rho))))
  

  #####################################################################
  ############# Run main MCMC #########################################
  #####################################################################
  
  n.iter <- 20000            # Num. of Iterations
  Tree <- matrix(0,nrow=n, ncol=m)
  Sigma2 <- NULL            # Variance parameter
  Sigma2[1] <- 0.1
  R <- Y  # Initial values of R
  
  # Missing index & Obj for imputed outcome
  mis.ind <- which(is.na(Y))
  Y.da <- matrix(nrow=length(mis.ind), ncol=n.iter)
  Y.da[,1] <- rnorm(length(mis.ind), mean(Y, na.rm=TRUE), 1)
  Y[mis.ind] <- Y.da[,1]
  count <- 0
  
  post.dir.alpha <- rep(1,P) # Posterior on selection probabilities
  
  seq <- seq(10001, 20000, by=10)
  ind <- rep(0,P)
  
  Obs_list <- list()
  for(i in 1:m){
    Obs_list[[i]] <- 1:n.complete
  }
  Tree <- matrix(0,nrow=n.complete, ncol=m)
  Tree11 <- matrix(0,nrow=n, ncol=m)
  
  dt_list <- list()
  for(i in 1:m){
    dt_list[[i]] <- list( position=rep(1,1), parent=rep(NA,1), Terminal=rep(1,1), Split=rep(NA,1), Value=rep(NA,1), MU=rep(NA,1), begin=rep(1,1), end=rep(n.complete,1))
  }
  
  prop.prob <- rdirichlet(1, rep(dir.alpha,P))
  
  Xpred.list <- xpred.mult <- list()
  for(i in 1:P){
    Xpred.list[[i]] <- Xpred[-mis.ind,i]
    xpred.mult[[i]] <- Xpred[,i]
  }
  xpred.mult[[P+1]] <- 1:(n)
  Xcut <- lapply(1:dim(Xpred)[2], function(t) sort(unique(Xpred[-mis.ind,t]))) # unique values of the predictors
  
  ### Run MCMC
  
  for(j in 2:n.iter){
        
    for(t in 1:m){
      
      R <- Y[-mis.ind] - rowSums(Tree[,-t]) - spatial[-mis.ind]
      
      ### Find the depth of the tree (0 or 1 or 2)
      tree.length <- length(dt_list[[t]]$position)
      if(tree.length == 1){  # tree has no node yet
        grow.step <- GROW.first(sigma2=Sigma2[j-1], sigma_mu=sigma_mu, dt=dt_list[[t]], R=R, prop.prob=prop.prob, Obs=Obs_list[[t]], ind=2)
        dt_list[[t]] <- grow.step$dt
        Obs_list[[t]] <- grow.step$Obs
        
      } else {
        step <- sample(1:3, 1, prob=c(p.grow, p.prune, p.change))  # Pick a step
        if(step==3){  # CHANGE step
          change.step <- CHANGE(sigma2=Sigma2[j-1], sigma_mu=sigma_mu, dt=dt_list[[t]], R=R, prop.prob=prop.prob, Obs=Obs_list[[t]], ind=2)
          dt_list[[t]] <- change.step$dt
          Obs_list[[t]] <- change.step$Obs
        }else{
          if(step==2){   # PRUNE step
            prune.step <- PRUNE(sigma2=Sigma2[j-1], sigma_mu=sigma_mu, dt=dt_list[[t]], R=R, prop.prob=prop.prob, Obs=Obs_list[[t]], ind=2)
            dt_list[[t]] <- prune.step$dt
            Obs_list[[t]] <- prune.step$Obs
          }else{
            if(step==1){   # GROW step
              grow.step <- GROW(sigma2=Sigma2[j-1], sigma_mu=sigma_mu, dt=dt_list[[t]], R=R, prop.prob=prop.prob,  Obs=Obs_list[[t]], ind=2)
              dt_list[[t]] <- grow.step$dt
              Obs_list[[t]] <- grow.step$Obs
            }}}
      }
      
      Mean <- Mean.Parameter(Sigma2[j-1], sigma_mu, dt=dt_list[[t]],  Obs=Obs_list[[t]], R, ind=2)
      Tree[,t] <- Mean$T
      dt_list[[t]] <- Mean$dt
    }
    
    
    # Sample variance parameter
    Sigma2[j] <- rinvgamma(1, nu/2+n.complete/2, scale = nu*lambda/2 + sum((Y[-mis.ind]-rowSums(Tree)-spatial[-mis.ind])^2)/2)
    
    #######################################
    # Start update the spatial random effect
    #######################################
    
    offset <- (Y-rowSums(Tree11))
    spatial <- gaussiancarupdate(Wtriplet=W.post.full$W.triplet, Wbegfin=W.post.full$W.begfin, W.post.full$W.triplet.sum, nsites=length(Y), phi=spatial, tau2=tau2, rho=rho, nu2=Sigma2[j], offset=offset)   
    spatial <- spatial - mean(spatial)
    
    temp2 <- quadform(as.matrix(W.post.full$W.triplet), W.post.full$W.triplet.sum, W.post.full$n.triplet, length(Y), spatial, spatial, rho)
    tau2.posterior.scale <- temp2 + prior.tau2[2]
    tau2 <- 1 / rgamma(1, tau2.posterior.shape, scale=(1/tau2.posterior.scale))
    TAU2[j] <- tau2
    
    
    # update rho parameter
    proposal.rho <- rtruncnorm(n=1, a=0, b=1, mean=rho, sd=proposal.sd.rho)
    temp3 <- quadform(as.matrix(W.post.full$W.triplet), W.post.full$W.triplet.sum, W.post.full$n.triplet, length(Y), spatial, spatial, proposal.rho)
    #  tau3.posterior.scale <- temp3 + prior.tau2[2]
    #  tau3 <- 1 / rgamma(1, tau2.posterior.shape, scale=(1/tau3.posterior.scale))
    det.Q.proposal <- 0.5 * sum(log((proposal.rho * Wstar.val + (1-proposal.rho))))
    logprob.current <- det.Q - temp2 / tau2
    logprob.proposal <- det.Q.proposal - temp3 / tau2
    hastings <- log(dtruncnorm(x=rho, a=0, b=1, mean=proposal.rho, sd=proposal.sd.rho)) - log(dtruncnorm(x=proposal.rho, a=0, b=1, mean=rho, sd=proposal.sd.rho))
    prob <- exp(logprob.proposal - logprob.current + hastings)
    #### Accept or reject the proposal
    if(prob > runif(1))
    {
      rho <- proposal.rho
      det.Q <- det.Q.proposal
      temp2 <- temp3
    }
    RHO[j] <- rho
    proposal.a.weight <- sample(1:5, 1)
    
    
    W.wind.proposal <- wind_mat*(W1^(I(proposal.a.weight==1)))*(W2^(I(proposal.a.weight==2)))*(W3^(I(proposal.a.weight==3)))*(W4^(I(proposal.a.weight==4)))*(W5^(I(proposal.a.weight==5)))
    #  W.wind.proposal <- W.wind.proposal[-mis.ind, -mis.ind]
    rownames(W.wind.proposal) <- 1:(n.full)
    colnames(W.wind.proposal) <- 1:(n.full)
    W.post.proposal <- common.Wcheckformat(W.wind.proposal)
    
    temp3 <- quadform(as.matrix(W.post.proposal$W.triplet),W.post.proposal$W.triplet.sum , W.post.proposal$n.triplet, length(Y), spatial, spatial, rho)
    
    Wstar.proposal <- diag(apply(W.wind.proposal,1,sum)) - W.wind.proposal
    Wstar.eigen.proposal <- eigen(Wstar.proposal)
    Wstar.val.proposal <- Wstar.eigen.proposal$values
    
    det.Q.proposal <- 0.5 * sum(log((rho * Wstar.val.proposal + (1-rho))))
    logprob.current <- det.Q - temp2 / tau2
    logprob.proposal <- det.Q.proposal - temp3 / tau2
    prob <- exp(logprob.proposal - logprob.current)
    
    #### Accept or reject the proposal
    if(prob > runif(1))
    {
      a.weight <- proposal.a.weight
      det.Q <- det.Q.proposal
      W.wind.full <- W.wind.proposal
      W.post.full <- W.post.proposal
      Wstar <- Wstar.proposal
      Wstar.eigen <- Wstar.eigen.proposal
      Wstar.val <- Wstar.val.proposal
    }
    A.WEIGHT[j] <- a.weight
        
    DT <- unlist(lapply(dt_list, function(x) x$Split))
    add <- as.numeric(table(factor(DT[!is.na(DT)], levels=1:P)))
    
    
    Tree11 <- matrix(unlist(sapply(1:m, function(x) Mean.Parameter_pred(dt_list[[x]], 1))), ncol=m, nrow=n)
    
    # Data augmentation
    count <- count + 1
    Y.da[,count] <- rnorm(length(mis.ind), c(rowSums(Tree11)+spatial)[mis.ind], sqrt(Sigma2[j]))
    Y[mis.ind] <- Y.da[,count]
    ind.temp<-ifelse(add > 0, 1, 0)
    ind <- rbind(ind, ind.temp)
  }
  
  save(Y.true, Y, shift,Y.da, mis.ind, A.WEIGHT, ind, Sigma2, file=paste0("Sim1_unif_",process,".RData"))
  
}
