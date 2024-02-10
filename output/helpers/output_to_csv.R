# Load data from R and save it preparing it for an analysis in Python

source("config.R")

# load input data
load("data/input_data.RData")
# load model output
load(paste0("output/", model_filename, ".RData"))

centroid_data <- read.csv("data/AreasCentroids.csv")

y_original <- merged_data$Y
y_complete <- model$y_predictions
x <- merged_data$X

Municipality_Name <- centroid_data$MUNICIPALITY_NAME

data_out <- cbind(y_original, y_complete, x, Municipality_Name)

colnames(data_out) <- c("PM25_original", "PM25_predictions", covariates_of_interest, "Municipality_Name")

covariates_inclusion <- model$covariates_selection_chain
colnames(covariates_inclusion) <- covariates_of_interest

write.csv(data_out, file = paste0("output/", model_filename, ".csv"), row.names = FALSE)
write.csv(covariates_inclusion, file = paste0("output/", model_filename, "_cov_inclusion", ".csv"), row.names = FALSE)
