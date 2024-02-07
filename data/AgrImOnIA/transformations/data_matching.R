merge_interpolated_with_responses <- function(
    shp_data,
    dfcov,
    date_begin,
    date_end, 
    columns_to_exclude
) {
    # Read dataset with responses
    dfy<-read.csv("data/AgrImOnIA/raw/Agrimonia_Dataset_v_3_0_0.csv", header=T)

    # Convert points to sf object and project to EPSG:4326
    points_sf <- st_as_sf(dfy, coords = c("Longitude", "Latitude"))
    points_sf <- st_set_crs(points_sf, st_crs("EPSG:4326"))

    # Convert polygons to sf object
    polygons_sf <- st_as_sf(shp_data, crs = st_crs("EPSG:4326"))

    # Project points to the coordinate system of polygons
    points_sf <- st_transform(points_sf, st_crs(polygons_sf))

    # Perform the intersection
    intersected <- st_intersects(points_sf, polygons_sf)

    # Associate points with polygons based on NOME_COM
    dfy$NOME_COM <- apply(intersected, 1, function(x) {
        if (any(x)) polygons_sf$NOME_COM[which(x)] else NA
    })

    dfy <- dfy %>% dplyr::select(-one_of(columns_to_exclude))

    covariate_indices <- 8:39
    # Convert the indices to column names
    covariate_names <- colnames(dfy)[covariate_indices]

    dfy <- dfy %>% dplyr::select(-one_of(covariate_names))

    # unique_coordinates <- unique(dfy[, c("Longitude", "Latitude")])

    # na_counts <- dfy %>%
    #     filter(!complete.cases(.)) %>%
    #     count(Latitude, Longitude)

    # Remove rows with NAs in a specific column, in this case we take out the Nas from the
    #NOME_COM column that are the stations that were outside Lombardy
    dfy_new <- dfy[complete.cases(dfy$NOME_COM), ]

    # Creating a new dataset with rows in the specified date range
    dfy_subset <- subset(dfy_new, Time >= date_begin & Time <= date_end)
    dfy_subset$Time <- as.Date(dfy_subset$Time)
    write.csv(dfy_subset, "data/AgrImOnIA/processed/dfy_subset.csv")

    names(dfcov)[names(dfcov) == "date"] <- "Time"
    
    # Merge the datasets based on "NOME_COM" and "date" with a left join
    merged_dataset <- merge(dfcov, dfy_subset, by = c("NOME_COM", "Time"), all.x = TRUE)
    
    # remove text NAs
    filtered_df <- merged_dataset[!grepl("(?i).*NA.*", merged_dataset$AQ_pm25), ]
    # remove numeric NAs
    filtered_df <- filtered_df[!is.na(filtered_df$AQ_pm25), ]

    print("----> Finished merging data")
    return(merged_dataset)
}


