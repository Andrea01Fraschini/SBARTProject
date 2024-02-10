#  Model parameters
n_iterations <- 10000L # 10000L (Kim et al., 2020)
n_trees <- 200L # 200L (Kim et al., 2020)
warmup <- 1000L # 1000L (Kim et al., 2020)

# Model filename
model_filename <- "2018_full_year_model_30wind_10m_0"

# Range dates for data
date_begin <- "2018-01-01"
date_end <- "2018-12-31"

response_variable <- "AQ_pm25"
covariates_of_interest <- c("Altitude", "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_wind_speed_10m_max", "WE_tot_precipitation", "WE_surface_pressure", "WE_solar_radiation", "WE_rh_min", "WE_rh_mean", "WE_rh_max", "WE_wind_speed_100m_mean", "WE_wind_speed_100m_max", "WE_blh_layer_max", "WE_blh_layer_min", "EM_nh3_livestock_mm", "EM_nh3_agr_soils", "EM_nh3_agr_waste_burn", "EM_nh3_sum", "EM_nox_traffic", "EM_nox_sum", "EM_so2_sum", "LI_pigs", "LI_bovine", "LA_hvi", "LA_lvi", "LA_land_use") # nolint