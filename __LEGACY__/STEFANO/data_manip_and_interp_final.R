#Creation of the df_cov_enriched, ideas and procedures for its result. 
#The objective is to find a way to get values of the covariates for the municipalities coordinates (longitude and latitude).
#The problem is that we have a grid of covariates in the AGC_Dataset_v_3_0_0.csv that has small number of point for a region that has a big number of municipalities (small density). 
#The first idea to get new values of the grid was to get a better "resolution" of the territory was to get middle values points between the existing one, so starting
#from a grid with an interval of stepsize 0.1 to a grid of stepsize 0.05, and the values of the covariates of the new points
#are the mean of the values of the already existing points that have longitude and latitude <= 0.05.
#From the new grid we would have associated to the municipalities(for which we have the correct values of longitude and latitude)
#the values of the covariates of the point of the grid that was nearest in distance to the coordinates
#of the considered municipality.
#Now we have found a more precise way to get the values for the municipalities (wrt to their centroids), using the bilinear interpolation,
#(see https://en.wikipedia.org/wiki/Bilinear_interpolation)
#After the encounter with Paolo Maranzano, we must find instead of the values of the covariates for the centroids,
#the values for all the area of the municipalities



library(sf)
library(readr)
library(dplyr)
library(lubridate)
library(lava)
library(akima)
library(gstat)
#library(spatialEco)
library(sp)
#library(raster)#problemi con sto pacchetto
library(automap)
#library(gridded)



setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice\\REGIONE_LOMBARDIA")
shp_data <- st_read("Comuni_correnti_poligonali.shp")


plot(shp_data["geometry"])

setwd("C:\\Users\\tetol\\OneDrive\\Desktop\\Corsi univeristà\\Bayesian Statistics\\progetto\\codice")
dfcov<-read.csv("AGC_Dataset_v_3_0_0.csv", header=T)





############# SEZIONE DATA CLEANING   ######################

#we check the number of Na's
na_count <- sum(is.na(dfcov))

#32886 Na's, we delete them

rows_with_na <- dfcov[!complete.cases(dfcov), ]

#unique_latitude_longitude_na <- unique(dfcov[!complete.cases(dfcov), c("Latitude", "Longitude")])

# print(unique_latitude_longitude_na)

dfcov_cleaned <- na.omit(dfcov)




na_counts <- dfcov %>%
  filter(!complete.cases(.)) %>%
  count(Latitude, Longitude)

# Print the result
print(na_counts)
#1827/365 = 5.0054, it seems we have Nas for these points for every day of the dataset



dfcov_cleaned$Time <- as.Date(dfcov_cleaned$Time)


#dfcov_cleaned$year <- lubridate::year(dfcov_cleaned$Time)

columns_to_exclude <- c("WE_mode_wind_direction_10m", "WE_mode_wind_direction_100m")


dfcov_nowind_dir<- dfcov_cleaned %>%
  select(-one_of(columns_to_exclude))

na_count <- sum(is.na(dfcov_nowind_dir))  #qua non ci sono Na

str(dfcov_nowind_dir)
summary(dfcov_nowind_dir)



dfcov_nowind_dir <- dfcov_nowind_dir[order(dfcov_nowind_dir$LI_pigs), ] #there are still the NA of string type at the bottom of pigs and bovines
dfcov_nowind_dir$WE_temp_2m <- as.numeric(dfcov_nowind_dir$WE_temp_2m)
dfcov_nowind_dir$WE_wind_speed_10m_mean <- as.numeric(dfcov_nowind_dir$WE_wind_speed_10m_mean)
dfcov_nowind_dir$WE_wind_speed_10m_max <- as.numeric(dfcov_nowind_dir$WE_wind_speed_10m_max)
dfcov_nowind_dir$WE_tot_precipitation <- as.numeric(dfcov_nowind_dir$WE_tot_precipitation) #non so che problemi abbia non si trasforma in numeric (nel summary mi dice che è già numeric mentre negli altri avevo char type)
dfcov_nowind_dir$WE_surface_pressure <- as.numeric(dfcov_nowind_dir$WE_surface_pressure)
dfcov_nowind_dir$WE_rh_min <- as.numeric(dfcov_nowind_dir$WE_rh_min)
dfcov_nowind_dir$WE_rh_mean <- as.numeric(dfcov_nowind_dir$WE_rh_mean)
dfcov_nowind_dir$WE_rh_max <- as.numeric(dfcov_nowind_dir$WE_rh_max)
dfcov_nowind_dir$LI_pigs <- as.numeric(dfcov_nowind_dir$LI_pigs) #escono degli Na
dfcov_nowind_dir$LI_bovine <- as.numeric(dfcov_nowind_dir$LI_bovine)  #escono degli Na
dfcov_nowind_dir$LA_hvi <- as.numeric(dfcov_nowind_dir$LA_hvi)
dfcov_nowind_dir$LA_lvi <- as.numeric(dfcov_nowind_dir$LA_lvi)

rows_with_na <- dfcov_nowind_dir[!complete.cases(dfcov_nowind_dir), ]#442134 new Nas

dfcov_nowind_cleaned <- na.omit(dfcov_nowind_dir) #1481697 observations





#we do the mean of the covariates in the points for each year


#result_data <- dfcov_nowind_dir_nona_thistime_yes %>%
#  group_by(Latitude, Longitude, year) %>%
#  summarise(across(
#    .cols = -c(Time),  # Exclude 'Time' from the calculation
#    .fns = ~mean(., na.rm = TRUE)  # Use an anonymous function
#  ), .groups = "drop")


# Save the dataset as a CSV file
#write.csv(result_data, "meanperyear_data.csv", row.names = FALSE)

#result_data<- read.csv("meanperyear_data.csv", header=T)








#################### GENERATING AREAL DATA FOR THE GRID OF POINTS #######################

# Step 1: generate the squares


# Estract the grid just for a day
dfcov_data <- data.frame(dfcov[dfcov$Time == "2016-01-01", ])

# when you will do with the cleaned version of the data
#dfcov_data <- data.frame(dfcov_nowind_cleaned[dfcov_nowind_cleaned$Time == "2016-01-01", ])



library(sf)
library(dplyr)

# Create a simple feature (sf) object from your dataset
sf_data <- st_as_sf(dfcov_data, coords = c("Longitude", "Latitude"))

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

# Define side length of the square
side_length <- 0.1

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

# Now, result contains both the geometry of squares and covariates from sf_data

x11()
plot(squares_with_covariates["geometry"])


#2nd step of interpolation: intersections

print(st_crs(shp_data))  #The CRS is WGS 84 / UTM zone 32N, identified by the EPSG code 32632. This is a common projected CRS used for mapping in a specific region.

st_crs(squares_with_covariates) <- st_crs("EPSG:4326")


# Transform squares_data to match the CRS of shp_data
squares_with_covariates <- st_transform(squares_with_covariates, crs = st_crs(shp_data))




# Perform intersection
intersections <- st_intersection(shp_data, squares_with_covariates)



x11()
plot(intersections["geometry"])

########### step 3: compute area of the intersections

intersections$area <- st_area(intersections$geometry)


#just a trial

target_municipality <- intersections[intersections$NOME_COM == "ABBADIA CERRETO", ]

target_municipality$area <- st_area(target_municipality$geometry)

result <- aggregate(area ~ NOME_COM, data = target_municipality, sum)
print(result)



# Step 4: Compute the weighted sum of the intersections for each municipality
#For this step you need a cleaned dataset, without NAs and some critical covariates.
#Go back in SEZIONE DATA CLEANING and after that procedure recompute the previous steps



# Specify the column indices for the covariates (26 to 52)
covariate_indices <- 26:52



# Calculate the weighted sum of the intersections for each municipality


intersections <- intersections %>%
  select(-geometry)  # Remove the geometry column

weighted_sum <- intersections %>%
  group_by(NOME_COM) %>%
  summarise(
    sum_area = sum(area),  # Sum of the areas
    across(covariate_indices, ~ sum(. * area) / sum(area), .names = "weighted_sum_{.col}")
  )




# Print the result
print(weighted_sum)



