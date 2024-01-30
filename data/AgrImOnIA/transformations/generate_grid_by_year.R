generate_grid_by_year <- function (df, side_length, year) {
  # Filter the dataset by year
  df_year <- df[df$year == year, ]
  
  # Create a simple feature (sf) object from your dataset
  sf_data <- st_as_sf(df_year, coords = c("Longitude", "Latitude"))
  
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
  
  # Return the result
  return(squares_with_covariates)
}
