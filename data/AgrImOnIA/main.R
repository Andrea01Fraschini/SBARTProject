# Load libraries and functions
library(sf)
library(dplyr)
library("lubridate")
source("data/AgrImOnIA/libraries.R")
source("data/AgrImOnIA/transformations/clean_data.R")
source("data/AgrImOnIA/transformations/create_square.R")
source("data/AgrImOnIA/transformations/join_data.R")
source("config.R")

# Read data
df <- read.csv("data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv", header=T)
shp_data <- st_read("data/AgrImOnIA/raw/Comuni_correnti_poligonali.shp")

# Define date range
date_begin <- as.Date(date_begin)
date_end <- as.Date(date_end)

# Clean data
columns_to_remove <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m", "WE_precipitation_t")
df_cleaned <- clean_data(df, columns_to_remove)

# Subset data
df_subset <- subset(df_cleaned, Time >= date_begin & Time <= date_end)

# Initialize variables
side_length <- 0.1
unique_dates <- unique(df_subset$Time)
results_list <- list()

# Loop through each unique date
for (t in 1:length(unique_dates)) {
  date <- unique_dates[t]
  # Extract the grid for the current day
  dfcov_data <- data.frame(df_subset[df_subset$Time == date, ])
  
  # Create an sf object from the dataset
  sf_data <- st_as_sf(dfcov_data, coords = c("Longitude", "Latitude"))
  
  # Initialize squares list
  squares <- list()
  
  # Create squares around each point in the dataset
  for (i in 1:nrow(sf_data)) {
    center <- st_coordinates(sf_data[i, ])
    square <- create_square(center, side_length)
    squares[[i]] <- square
  }
  
  # Ensure each element in the list is a valid sf polygon
  squares <- st_sfc(squares, crs = st_crs(sf_data))
  
  # Create an sf object from squares
  sf_squares <- st_sf(geometry = squares)
  
  # Add covariates to the squares using row index
  sf_squares$id <- 1:nrow(sf_squares)
  squares_with_covariates <- st_join(sf_squares, sf_data, by = "id")
  
  # Set CRS
  st_crs(squares_with_covariates) <- st_crs("EPSG:4326")
  
  # Transform squares_data to match the CRS of shp_data
  squares_with_covariates <- st_transform(squares_with_covariates, crs = st_crs(shp_data))
  
  # Perform intersection
  intersections <- st_intersection(shp_data, squares_with_covariates)
  
  # Compute area of the intersections
  intersections$area <- st_area(intersections$geometry)
  
  # Convert the "area" column to numeric for the entire dataset
  intersections$area <- as.numeric(intersections$area)
  
  # Specify the column indices for the covariates (26 to 51)
  covariate_indices <- 26:51
  # Convert the indices to column names
  covariate_names <- colnames(intersections)[covariate_indices]
  
  # Create a new data frame to store the results
  result_df <- data.frame(NOME_COM = unique(intersections$NOME_COM))
  
  # Initialize covariate columns to zero
  for (covariate in covariate_names) {
    result_df[, covariate] <- 0
  }
  
  for (municipality in result_df$NOME_COM) {
    # Subset the intersections data for the current municipality
    municipality_data <- intersections[intersections$NOME_COM == municipality, ]
    
    # Extract non-geographic data (assuming covariates are after geometry column)
    covariate_data <- as.data.frame(municipality_data[, c("area", covariate_names)])
    
    # Loop through each covariate and calculate the weighted sum
    for (covariate in covariate_names) {
      weighted_sum <- sum(covariate_data$area * covariate_data[, covariate], na.rm = TRUE) / sum(covariate_data$area, na.rm = TRUE)
      
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

# Write the interpolated result to a CSV file
write.csv(final_result, "data/AgrImOnIA/processed/df.csv", row.names = FALSE)

# Merge covariates and responses
# columns_to_remove <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m", "Altitude", "AQ_pm10")
# merged_data <- merge_interpolated_with_responses(shp_data, final_result, date_begin, date_end, c())

merged_data <- join_data(response_variable = response_variable)
save(merged_data, file = "data/input_data.RData")

print("----> Finished processing")