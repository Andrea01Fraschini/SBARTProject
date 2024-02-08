create_grid <- function (df, side_length) {
  source("data/AgrImOnIA/transformations/create_square.R")

  # Create a simple feature (sf) object from your dataset
  sf_data <- st_as_sf(df, coords = c("Longitude", "Latitude"))

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

  # Return the result
  return(squares_with_covariates)
}