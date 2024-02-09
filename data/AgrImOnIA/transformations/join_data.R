
join_data <- function(response_variable, covariates_of_interest) {
    shp_data <- st_read("data/AgrImOnIa/raw/Comuni_correnti_poligonali.shp")

    municipalities <- read.csv("data/MunicipalitiesCentroids.csv")
    municipalities_names <- unique(municipalities$MUNICIPALITY_NAME)
    municipalities_count <- length(municipalities_names)

    covariates_dataset <- read.csv("data/AgrImOnIA/processed/df.csv")
    selected_covariate_indexes <- match(covariates_of_interest, colnames(covariates_dataset))

    covariates <- covariates_dataset[, selected_covariate_indexes]
    covariates["NOME_COM"] <- covariates_dataset$NOME_COM
    covariates_municipalities <- gsub("`", "'", unique(covariates$NOME_COM))
    
    dataset <- read.csv("data/AgrImOnIA/raw/Agrimonia_Dataset_v_3_0_0.csv")

    # Convert points to sf object and project to EPSG:4326
    points_sf <- st_as_sf(dataset, coords = c("Longitude", "Latitude"))
    points_sf <- st_set_crs(points_sf, st_crs("EPSG:4326"))

    # Convert polygons to sf object
    polygons_sf <- st_as_sf(shp_data, crs = st_crs("EPSG:4326"))

    # Project points to the coordinate system of polygons
    points_sf <- st_transform(points_sf, st_crs(polygons_sf))

    # Perform the intersection
    intersected <- st_intersects(points_sf, polygons_sf)

    # Associate points with polygons based on NOME_COM
    dataset$NOME_COM <- apply(intersected, 1, function(x) {
        if (any(x)) polygons_sf$NOME_COM[which(x)] else NA
    })

    
    dataset_avg <- dataset %>% 
        group_by(NOME_COM) %>%
        summarise(across(
            .cols = all_of(response_variable),
            .fns = ~mean(., na.rm = TRUE)
        ))
    responses <- unlist(dataset_avg[response_variable], use.names = FALSE)
    dataset_municipalities <- gsub("`", "'", na.omit(unique(dataset_avg$NOME_COM)))

    Y <- c(rep(NA, municipalities_count)) 
    X <- matrix(nrow = municipalities_count, ncol = length(selected_covariate_indexes) + 1)

    for (i in 1:municipalities_count) {
        current_name <- municipalities_names[i]
        municipality_response_index <- which(dataset_municipalities == current_name)
        municipality_covariates_index <- which(covariates_municipalities == current_name)

        if(length(municipality_response_index) > 0) {
            Y[i] <- as.numeric(responses[municipality_response_index])
        } 

        X[i, ] <- unlist(covariates[municipality_covariates_index, ], use.names = FALSE)
    }

    Y[is.nan(Y)] <- NA

    # Remove last column of X, which is the NOME_COM
    X <- X[, -length(selected_covariate_indexes) - 1]

    return(list(
        Y = Y,
        X = X
    ))
}