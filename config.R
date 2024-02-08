#  Model parameters
n_iterations <- 2000L # 10000L (Kim et al., 2020)
n_trees <- 100L # 200L (Kim et al., 2020)
warmup <- 1000L # 1000L (Kim et al., 2020)

# Data
data_filename <- "input_data"

# Model filename
model_filename <- "2016_Gen_to_Aug_model_30wind_10m_0"

# Range dates for data
date_begin <- "2016-01-01"
date_end <- "2016-01-01"

response_variable <- "AQ_pm25"
covariates_of_interest <- c("Altitude", "LI_pigs", "LI_bovine")