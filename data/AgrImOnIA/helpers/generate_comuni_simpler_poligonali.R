library(sf)
library(dplyr)

# Read the csv file
areas_geometries <- read.csv("python/AreasGeometriesWithNames.csv")

# Keep only the columns with the polygon geometries
areas_geometries <- areas_geometries[, c("MUNICIPALITY_NAME", "geometry")]

# Convert the geometry column to WKT strings
areas_geometries$geometry <- st_as_text(st_as_sfc(areas_geometries$geometry))

# Convert the data frame to a simple feature collection
areas_geometries_sf <- st_as_sf(areas_geometries, wkt = "geometry")

# Make the geometries valid
areas_geometries_sf <- st_make_valid(areas_geometries_sf, reason = TRUE)

# Set the CRS
st_crs(areas_geometries_sf) <- 4326

# Transform the geometries to EPSG:4326
areas_geometries_sf <- st_transform(areas_geometries_sf, 4326)

# Plot the sf object to see the polygons
plot(areas_geometries_sf)

# Save the sf object as a shapefile
st_write(areas_geometries_sf, "data/AgrImOnIA/raw/comuni_simpler_poligonali.shp", append = FALSE, drive)