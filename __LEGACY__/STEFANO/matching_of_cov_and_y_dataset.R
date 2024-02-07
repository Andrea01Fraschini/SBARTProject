library(sf)
library(dplyr)

setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice\\REGIONE_LOMBARDIA")
shp_data <- st_read("Comuni_correnti_poligonali.shp")


setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice")
dfy<-read.csv("Agrimonia_Dataset_v_3_0_0.csv", header=T)

dfcov<-read.csv("covariates_09_2016_to_03_2017.csv", header=T)



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




columns_to_exclude <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m")
dfy<- dfy %>%
  select(-one_of(columns_to_exclude))


columns_to_exclude <- c("Altitude")
dfy<- dfy %>%
  select(-one_of(columns_to_exclude))


columns_to_exclude <- c("AQ_pm10")
dfy<- dfy %>%
  select(-one_of(columns_to_exclude))


covariate_indices <- 6:39
# Convert the indices to column names
covariate_names <- colnames(dfy)[covariate_indices]


dfy<- dfy %>%
  select(-one_of(covariate_names))







#na_count <- sum(is.na(dfy))
#rows_with_na <- dfy[!complete.cases(dfy), ]


#dfcov_cleaned <- na.omit(dfy)



unique_coordinates <- unique(dfy[, c("Longitude", "Latitude")])

# Print the unique coordinates
print(unique_coordinates)


na_counts <- dfy %>%
  filter(!complete.cases(.)) %>%
  count(Latitude, Longitude)



nas_in_column <- sum(is.na(dfy$NOME_COM))



# Remove rows with NAs in a specific column, in this case we take out the Nas from the
#NOME_COM column that are the stations that were outside the lombardy
dfy_new<- dfy[complete.cases(dfy$NOME_COM), ]


# Creating a new dataset with rows in the specified date range
dfy_subset <- subset(dfy_new, Time >= as.Date("2016-09-01") & Time <= as.Date("2017-03-31"))

colnames(dfcov)[29] <- "Time"

# Merge the datasets based on "NOME_COM" and "date" with a left join
merged_dataset <- merge(dfcov, dfy_subset, by = c("NOME_COM", "Time"), all.x = TRUE)


