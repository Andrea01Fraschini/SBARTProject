get_intersection <- function(df_grid, shp_data){
# Perform intersection
  intersections <- st_intersection(shp_data, df_grid)
  
  # Compute area of the intersections
  intersections$area <- st_area(intersections$geometry)
  
  # Convert the "area" column to numeric for the entire dataset
  intersections$area <- as.numeric(intersections$area)

  return(intersections)
}