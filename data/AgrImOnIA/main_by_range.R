source("data/AgrImOnIA/libraries.R")
source("data/AgrImOnIA/transformations/clean_data.R")
source("data/AgrImOnIA/transformations/create_square.R")
source("data/AgrImOnIA/transformations/join_data.R")
source("data/AgrImOnIA/transformations/get_mean.R")
source("data/AgrImOnIA/transformations/get_intersection.R")
source("data/AgrImOnIA/transformations/create_grid.R")
source("data/AgrImOnIA/transformations/compute_weighted_sum.R")
source("config.R")

# Read data
df_cov <- read.csv("data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv", header=T)
df_y <- read.csv("data/AgrImOnIA/raw/Agrimonia_Dataset_v_3_0_0.csv", header=T)
shp_data <- st_read("data/AgrImOnIA/raw/Comuni_correnti_poligonali.shp")

# Define date range
date_begin <- as.Date(date_begin)
date_end <- as.Date(date_end)

# Subset data
df_subset <- subset(df_cov, Time >= date_begin & Time <= date_end)

# Clean data
columns_to_remove <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m", "WE_precipitation_t")
df_cleaned <- clean_data(df_subset, columns_to_remove)

# Group by longitude and latitude, and calculate mean for each group
df_mean <- get_mean(df_cleaned, c("Longitude", "Latitude"))

# Create grid
df_grid <- create_grid(df_mean, 0.1)

# Intersect grid with shapefile
df_intersections <- get_intersection(df_grid, shp_data)

# Compute weighted sum per municipality
df_result <- compute_weighted_sum(df_intersections, covariates_of_interest)

# write.csv(df_result, "data/AgrImOnIA/processed/df.csv", row.names = FALSE)

merged_data <- join_data(
                    response_variable = response_variable, 
                    covariates_of_interest = covariates_of_interest 
                )
save(merged_data, file = "data/input_data.RData")

print("----> Finished processing data <----")