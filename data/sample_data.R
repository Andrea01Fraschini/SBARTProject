#' Generate Sample Data
#'
#' This function generates a sample dataset for spatial analysis. It creates a square lattice region, 
#' defines spatial connections, generates covariates and response data, and creates a weight matrix.
#'
#' @return A list containing the following elements:
#'   - x_predcistors: A matrix of predictors.
#'   - y: The response variable with some missing values.
#'   - missing_indexes: Indices of the missing values in Y.
#'   - ws: A list of weight matrices.
#'   - wind_matrix: A matrix defining spatial connections.
#'
#' @examples
#' data = sample_data()
#' @export
#'
sample_data <- function() {
  # Square lattice region
  x_easting <- 1:15
  x_northing <- 1:15
  grid <- expand.grid(x_easting, x_northing)
  k <- nrow(grid) # num. of locations (15 by 15 = 225)

  # Distance and neighborhood matrix
  distance <- as.matrix(dist(grid))
  diag(distance) <- 0
  w <- array(0, c(k, k))

  # Define spatial connections (see kim (2020+) for more details)
  wind_seed1 <- c(1, 17, 18, 32:35, 48:52, 63:69, 79:86, 94:103, 110:120, 125:135, 141:150, 156:165, 172:180, 187:195, 203:210, 218:225)
  wind_seed2 <- setdiff(which(lower.tri(matrix(NA, 15, 15), diag = FALSE) == TRUE), wind_seed1)
  wind_seed3 <- setdiff(which(t(lower.tri(matrix(NA, 15, 15), diag = FALSE)) == TRUE), wind_seed1)
  wind_matrix <- matrix(0, k, k)
  for (i in 1:225) {
    if (i %in% wind_seed1) { wind_matrix[i, wind_seed1] <- 1 }
    if (i %in% wind_seed2) { wind_matrix[i, wind_seed2] <- 1 }
    if (i %in% wind_seed3) { wind_matrix[i, wind_seed3] <- 1 }
  }

  # Generate the covariates and response data
  mean_vector <- rep(0, 15 ^ 2)
  mean_vector[wind_seed2] <- -2.5
  mean_vector[wind_seed3] <- 0.5

  x1 <- rnorm(k, 1 * (grid[, 1] / 10 - 1) ^ 2, 1.5)
  x2 <- rnorm(k, mean_vector, 1)
  x3 <- rnorm(k, 0.5, 1.5)
  x_com <- rmnorm(k, c(-1, 1), matrix(c(1.5, 0.25, 0.25, 1.5), 2, 2))
  x4 <- x_com[, 1]
  x5 <- x_com[, 2]

  phi <- rep(0, k)
  phi[wind_seed1] <- rmnorm(1, rep(0, k), 1.5 * exp(-0.05 * distance))[wind_seed1]
  phi[wind_seed2] <- rmnorm(1, rep(0, k), exp(-0.1 * distance))[wind_seed2]
  phi[wind_seed3] <- rmnorm(1, rep(0, k), 1.5 * exp(-0.1 * distance))[wind_seed3]

  theta <- rnorm(k, mean = rep(0, k), sd = 0.1)

  y <- 2.5 + 1.5 * x1 + 1.5 * x2 - 1 * x3 + 1.5 * x4 - 1.5 * x5 + 0.25 * x4 * x5 + theta + phi

  y_actual <- y

  # Randomly selected missing locations
  missing_indexes <- sample(1:225, 195, replace = FALSE)
  y[missing_indexes] <- NA

  n <- 225

  cov <- list()
  for (i in 1:45) {
    cov[[i]] <- rnorm(n, 0, 1) # draw x independently from a normal distribution
  }

  # Vector of X predictors
  x_predictors <- cbind(x1, x2, x3, x4, x5, do.call(cbind, cov))
  
  # Weight matrix
  distance_matrix <- ifelse(distance < 1, 1, distance)
  w1 <- 1 / (distance) ^ 0.5
  w2 <- 1 / (distance) ^ 1
  w3 <- 1 / (distance) ^ 1.5
  w4 <- 1 / (distance) ^ 2
  w5 <- exp(-distance)
  diag(w1) <- 0
  diag(w2) <- 0
  diag(w3) <- 0
  diag(w4) <- 0
  diag(w5) <- 0

  ws <- list(w1, w2, w3, w4, w5)

  result <- list(
    y = y,
    x_predictors = x_predictors,
    missing_indexes = missing_indexes,
    ws = ws,
    wind_matrix = wind_matrix,
    y_actual = y_actual
  )

  return(result)
}