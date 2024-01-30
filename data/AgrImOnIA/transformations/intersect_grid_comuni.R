intersect_grid_comuni <- function(shp_data, df_grid_by_year){
    st_crs(df_grid_by_year) <- st_crs("EPSG:4326")

    # Transform squares_data to match the CRS of shp_data
    df_grid_by_year <- st_transform(df_grid_by_year, crs = st_crs(shp_data))
    
    # Perform intersection
    intersections <- st_intersection(shp_data, df_grid_by_year)
    
    return(intersections)
}