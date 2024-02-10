generate_data <- function(date_begin, date_end, response_variable, covariates_of_interest) {
    source("data/AgrImOnIA/libraries.R")
    source("data/AgrImOnIA/transformations/clean_data.R")
    source("data/AgrImOnIA/transformations/create_square.R")
    source("data/AgrImOnIA/transformations/join_data.R")
    source("data/AgrImOnIA/transformations/get_mean.R")
    source("data/AgrImOnIA/transformations/get_intersection.R")
    source("data/AgrImOnIA/transformations/create_grid.R")
    source("data/AgrImOnIA/transformations/compute_weighted_sum.R")

    # Read data
    df_cov <- read.csv("data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv", header=T)
    shp_data_simpler <- st_read("data/AgrImOnIA/raw/AreasGeometriesWithNames.shp")
    shp_data <- st_read("data/AgrImOnIA/raw/Comuni_correnti_poligonali.shp")

    # plort SQ!$&

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

    # Transform df_grid to match the CRS of shp_data
    df_grid <- st_transform(df_grid, crs = st_crs(shp_data))

    # Get municipalities in shp_data where MUNICIP does not follow the regex pattern SQ[1-9]*
    shp_data_municipalities <- shp_data_simpler[!grepl("^SQ[1-9]*", shp_data_simpler$MUNICIPALI), "MUNICIPALI"]

    # Replace in MUINICIPALI the ' with ` 
    shp_data_municipalities <- gsub("'", "`", shp_data_municipalities$MUNICIPALI)

    # filter shp_data to keep only the municipalities in shp_data_municipalities
    shp_data <- shp_data[shp_data$NOME_COM %in% shp_data_municipalities,]

    # Intersect grid with shapefile
    df_intersections_municipalities <- get_intersection(df_grid, shp_data)

    # Compute weighted sum per municipality
    df_result_municipalities <- compute_weighted_sum(df_intersections_municipalities, covariates_of_interest)

    write.csv(df_result_municipalities, "data/AgrImOnIA/processed/df_municipalities.csv", row.names = FALSE)

    # SQUARES SECTION

    # Get municipalities in shp_data where MUNICIP follows the regex pattern SQ[1-9]*
    shp_data_squares <- shp_data_simpler[grepl("^SQ[1-9]*", shp_data_simpler$MUNICIPALI), ]

    # Equal crs
    shp_data_squares <- st_transform(shp_data_squares, crs = st_crs(df_grid))

    # Intersect grid with shapefile
    df_intersections_squares <- get_intersection(df_grid, shp_data_squares)

    # Change MUNICIPALI to NOME_COM
    df_intersections_squares$NOME_COM <- df_intersections_squares$MUNICIPALI

    # Compute weighted sum per square
    df_result_squares <- compute_weighted_sum(df_intersections_squares, covariates_of_interest)

    write.csv(df_result_squares, "data/AgrImOnIA/processed/df_squares.csv", row.names = FALSE)

    # Append the two dataframes
    df_result <- rbind(df_result_squares, df_result_municipalities)

    write.csv(df_result, "data/AgrImOnIA/processed/df.csv", row.names = FALSE)

    merged_data <- join_data(
                        response_variable = response_variable, 
                        covariates_of_interest = covariates_of_interest 
                    )
    save(merged_data, file = "data/input_data.RData")

    print("----> Finished processing data <----")
}