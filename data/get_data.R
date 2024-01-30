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
  # Load the data
  source("config.R")
  data <- read.csv(paste0("data/", data_filename, ".csv"), header = TRUE, row.names = 1)

  # Response variable
  y <-

  # Vector of X predictors
  x_predictors <- 
  
  # Define spatial connections (see kim (2020+) for more details)
  wind_matrix <- read.csv("python/adjacency_files/<PUT THE FILENAME HERE>", header = FALSE)

  # DON'T CHANGE ANYTHING BELOW THIS LINE

  # Randomly selected missing locations
  missing_indexes <- which(is.na(y) == TRUE)

  # Weight matrix
  w1 <- read.csv("python/cost_matrices/cost_matrix_0.csv", header = FALSE)
  w2 <- read.csv("python/cost_matrices/cost_matrix_1.csv", header = FALSE)
  w3 <- read.csv("python/cost_matrices/cost_matrix_2.csv", header = FALSE)
  w4 <- read.csv("python/cost_matrices/cost_matrix_3.csv", header = FALSE)
  w5 <- read.csv("python/cost_matrices/cost_matrix_4.csv", header = FALSE)

  ws <- list(w1, w2, w3, w4, w5)

  result <- list(
    y = y,
    x_predictors = x_predictors,
    missing_indexes = missing_indexes,
    ws = ws,
    wind_matrix = wind_matrix
  )

  return(result)
}