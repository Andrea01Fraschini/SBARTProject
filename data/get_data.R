#' Get data
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
get_data <- function() {
  source("config.R")
  source("data/AgrImOnIA/main_by_range.R")
  # Load the data
  generate_data(date_begin, date_end, response_variable, covariates_of_interest)
  # data <- read.csv(paste0("data/", data_filename, ".csv"), header = TRUE, row.names = 1)
  load("data/input_data.RData")
  data <- merged_data

  # Response variable
  y <- data$Y

  # Vector of X predictors
  x_predictors <- data$X

  # Define spatial connections (see kim (2020+) for more details)
  wind_matrix <- read.csv("python/adjacency_files/wind_adjacency_matrix_10m_30.csv", header = FALSE, skip = 1)
  colnames(wind_matrix) <- NULL
  wind_matrix <- as.matrix(wind_matrix) 

  # DON'T CHANGE ANYTHING BELOW THIS LINE

  # Randomly selected missing locations
  missing_indexes <- which(is.na(y) == TRUE)

  # Weight matrix
  w1 <- read.csv("python/cost_matrices/cost_matrix_0.csv", header = FALSE, skip = 1)
  w2 <- read.csv("python/cost_matrices/cost_matrix_1.csv", header = FALSE, skip = 1)
  w3 <- read.csv("python/cost_matrices/cost_matrix_2.csv", header = FALSE, skip = 1)
  w4 <- read.csv("python/cost_matrices/cost_matrix_3.csv", header = FALSE, skip = 1)
  w5 <- read.csv("python/cost_matrices/cost_matrix_4.csv", header = FALSE, skip = 1)

  colnames(w1) <- NULL
  colnames(w2) <- NULL
  colnames(w3) <- NULL
  colnames(w4) <- NULL
  colnames(w5) <- NULL

  w1 <- as.matrix(w1)
  w2 <- as.matrix(w2)
  w3 <- as.matrix(w3)
  w4 <- as.matrix(w4)
  w5 <- as.matrix(w5)

  ws <- list(w1, w2, w3, w4, w5)

  # to compensate for the suppression of Albaredo Arnaboldi
  CAMPOSPINOSO_index <- 655
  ALBAREDO_index <- 1155
  compensation <- function(w) {
    w[ALBAREDO_index, ] <- w[CAMPOSPINOSO_index, ]
    w[, ALBAREDO_index] <- w[, CAMPOSPINOSO_index]
    return(w)
  }
  ws <- lapply(ws, compensation)

  result <- list(
    y = y,
    x_predictors = x_predictors,
    missing_indexes = missing_indexes,
    ws = ws,
    wind_matrix = wind_matrix
  )

  return(result)
}