source("data/AgrImOnIA/libraries.R")
source("data/AgrImOnIA/transformations/clean_data.R")
source("data/AgrImOnIA/transformations/get_mean_by_year.R")
source("data/AgrImOnIA/transformations/generate_grid_by_year.R")
source("data/AgrImOnIA/transformations/intersect_grid_comuni.R")
source("data/AgrImOnIA/transformations/compute_weighted_sum.R")

df <- read.csv("data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv", header=T)

columns_to_remove <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m")
df_cleaned <- clean_data(df, columns_to_remove)
df_mean_by_year <- get_mean_by_year(df_cleaned)

year <- 2016
side_length <- 0.1
df_grid_by_year <- generate_grid_by_year(df_mean_by_year, side_length, year)

shp_data <- st_read("data/AgrImOnIA/raw/Comuni_correnti_poligonali.shp")

df_intersected <- intersect_grid_comuni(shp_data, df_grid_by_year)
df_intersected$area <- st_area(df_intersected$geometry)

covariate_indices <- 26:52
df_weighted_sum <- compute_weighted_sum(df_intersected, covariate_indices)

# Write the result to a CSV file
df_regular <- as.data.frame(df_weighted_sum)
df_without_geometry <- df_regular %>% select(-geometry)
write.csv(df_without_geometry, paste0("data/AgrImOnIA/processed/df_weighted_sum", year, ".csv"), row.names = FALSE)