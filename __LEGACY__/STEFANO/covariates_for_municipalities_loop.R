#### COVARIATES FOR EACH MUNICIPALITY, LOOP VERSION ####

library(sf)
library(dplyr)


setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice\\REGIONE_LOMBARDIA")
shp_data <- st_read("Comuni_correnti_poligonali.shp")

setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice")
# In this code i skipped the part of data cleaning and i already start from the
#cleaned dataset i saved
dfcov_nowind_cleaned<-read.csv("dfcov_nowind_cleaned.csv", header=T)





# Creating a new dataset with rows in the specified date range
dfcov_subset <- subset(dfcov_nowind_cleaned, Time >= as.Date("2016-01-01") & Time <= as.Date("2016-01-07"))




# Function to create a square around a point with a given side length
create_square <- function(center, side_length) {
  half_side <- side_length / 2
  square_coords <- matrix(
    c(center[1] - half_side, center[2] - half_side,
      center[1] + half_side, center[2] - half_side,
      center[1] + half_side, center[2] + half_side,
      center[1] - half_side, center[2] + half_side,
      center[1] - half_side, center[2] - half_side),
    ncol = 2,
    byrow = TRUE
  )
  st_polygon(list(square_coords))
}

# Define side length of the square
side_length <- 0.1

# Extract unique dates from dfcov_nowind_cleaned
unique_dates <- unique(dfcov_subset$Time)

# Creating an empty list to store results for each date
results_list <- list()




# Loop through each unique date
for (date in unique_dates) {
  # Extract the grid for the current day
  dfcov_data <- data.frame(dfcov_subset[dfcov_subset$Time == date, ])
  
  # Create an sf object from the dataset
  sf_data <- st_as_sf(dfcov_data, coords = c("Longitude", "Latitude"))
  
  
  
  # Create squares around each point in the dataset
  squares <- lapply(1:nrow(sf_data), function(i) {
    center <- st_coordinates(sf_data[i, ])
    square <- create_square(center, side_length)
    square
  })
  
  # Ensure each element in the list is a valid sf polygon
  squares <- st_sfc(squares, crs = st_crs(sf_data))
  
  # Create an sf object from squares
  sf_squares <- st_sf(geometry = squares)
  
  # Add covariates to the squares using row index
  sf_squares$id <- 1:nrow(sf_squares)
  squares_with_covariates <- st_join(sf_squares, sf_data, by = "id")
  
  #2nd step of interpolation: intersections
  
  
  st_crs(squares_with_covariates) <- st_crs("EPSG:4326")
  
  
  
  
  
  
  
  # Transform squares_data to match the CRS of shp_data
  squares_with_covariates <- st_transform(squares_with_covariates, crs = st_crs(shp_data))
  
  # Perform intersection
  intersections <- st_intersection(shp_data, squares_with_covariates)
  
  # Compute area of the intersections
  intersections$area <- st_area(intersections$geometry)
  
  # Convert the "area" column to numeric for the entire dataset
  intersections$area <- as.numeric(intersections$area)
  
  
  # Specify the column indices for the covariates (26 to 52)
  covariate_indices <- 26:52
  # Convert the indices to column names
  covariate_names <- colnames(intersections)[covariate_indices]
  
  
  # Create a new data frame to store the results
  result_df <- data.frame(NOME_COM = unique(intersections$NOME_COM))
  
  
  result_df[, covariate_names] <- 0  # Initialize covariate columns to zero
  
  
  
  
  
  for (municipality in result_df$NOME_COM) {
    # Subset the intersections data for the current municipality
    municipality_data <- intersections[intersections$NOME_COM == municipality, ]
    
    # Extract non-geographic data (assuming covariates are after geometry column)
    covariate_data <- as.data.frame(municipality_data[, c("area", covariate_names)])
    
    # Loop through each covariate and calculate the weighted sum
    for (covariate in covariate_names) {
      weighted_sum <- sum(covariate_data$area * covariate_data[, covariate]) / sum(covariate_data$area)
      
      # Assign the result to the corresponding cell in the result_df
      result_df[result_df$NOME_COM == municipality, covariate] <- weighted_sum
    }
  }
  
  
  # Store the results for the current date with the corresponding date column
  result_df$date <- date
  results_list[[as.character(date)]] <- result_df
}

# Combine the results for all dates into a single dataframe
final_result <- do.call(rbind, results_list)


