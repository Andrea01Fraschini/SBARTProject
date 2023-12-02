sample_data <- function() {
  # Square lattice region
  x.easting <- 1:15
  x.northing <- 1:15
  Grid <- expand.grid(x.easting, x.northing)
  K <- nrow(Grid) # num. of locations (15 by 15 = 225)

  # Distance and neighborhood matrix
  Distance <- distance <- as.matrix(dist(Grid))
  diag(Distance) <- 0
  W <- array(0, c(K, K))

  # Define spatial connections (see Kim (2020+) for more details)
  wind_seed1 <- c(1, 17, 18, 32:35, 48:52, 63:69, 79:86, 94:103, 110:120, 125:135, 141:150, 156:165, 172:180, 187:195, 203:210, 218:225)
  wind_seed2 <- setdiff(which(lower.tri(matrix(NA, 15, 15), diag = FALSE) == TRUE), wind_seed1)
  wind_seed3 <- setdiff(which(t(lower.tri(matrix(NA, 15, 15), diag = FALSE)) == TRUE), wind_seed1)
  wind_mat <- matrix(0, K, K)
  for (i in 1:225) {
    if (i %in% wind_seed1) { wind_mat[i, wind_seed1] <- 1 }
    if (i %in% wind_seed2) { wind_mat[i, wind_seed2] <- 1 }
    if (i %in% wind_seed3) { wind_mat[i, wind_seed3] <- 1 }
  }

  # Generate the covariates and response data
  mean.vector <- rep(0, 15 ^ 2)
  mean.vector[wind_seed2] <- -2.5
  mean.vector[wind_seed3] <- 0.5

  x1 <- rnorm(K, 1 * (Grid[, 1] / 10 - 1) ^ 2, 1.5)
  x2 <- rnorm(K, mean.vector, 1)
  x3 <- rnorm(K, 0.5, 1.5)
  x.com <- rmnorm(K, c(-1, 1), matrix(c(1.5, 0.25, 0.25, 1.5), 2, 2))
  x4 <- x.com[, 1]
  x5 <- x.com[, 2]

  phi <- rep(0, K)
  phi[wind_seed1] <- rmnorm(1, rep(0, K), 1.5 * exp(-0.05 * Distance))[wind_seed1]
  phi[wind_seed2] <- rmnorm(1, rep(0, K), exp(-0.1 * Distance))[wind_seed2]
  phi[wind_seed3] <- rmnorm(1, rep(0, K), 1.5 * exp(-0.1 * Distance))[wind_seed3]

  theta <- rnorm(K, mean = rep(0, K), sd = 0.1)

  Y.true <- Y <- 2.5 + 1.5 * x1 + 1.5 * x2 - 1 * x3 + 1.5 * x4 - 1.5 * x5 + 0.25 * x4 * x5 + theta + phi

  # Randomly selected missing locations
  missing <- mis.ind <- sample(1:225, 195, replace = FALSE)
  Y[missing] <- NA

  # Total 50 potential predictors
  P <- 50
  n <- 225

  cov <- list()
  for (i in 1:45) {
    cov[[i]] <- rnorm(n, 0, 1) # draw x independently from a normal distribution
  }
  Xpred1 <- do.call(cbind, cov)

  # Vector of X predictors
  Xpred <- cbind(x1, x2, x3, x4, x5, Xpred1)

  n.full <- length(Y) # num. of the locations
  n.complete <- length(which(!is.na(Y))) # num. of the locations with observation

  # Weight matrix
  a.weight <- 1 # initial value for a_w parameter in the weight
  dist.mat <- ifelse(distance < 1, 1, distance)
  W1 <- 1 / (distance) ^ 0.5
  W2 <- 1 / (distance) ^ 1
  W3 <- 1 / (distance) ^ 1.5
  W4 <- 1 / (distance) ^ 2
  W5 <- exp(-distance)
  diag(W1) <- 0
  diag(W2) <- 0
  diag(W3) <- 0
  diag(W4) <- 0
  diag(W5) <- 0

  Ws <- list(W1, W2, W3, W4, W5)

  result <- list(
    Xpred = Xpred,
    Y = Y,
    mis.ind = mis.ind,
    Ws = Ws,
    wind_mat = wind_mat
  )

  return(result)
}