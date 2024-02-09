
source("config.R")

# load input data
load("data/input_data.RData")
# load model output

centroid_data <- read.csv("data/MunicipalitiesCentroids.csv")

y_original <- merged_data$Y
x <- merged_data$X

Municipality_Name <- centroid_data$MUNICIPALITY_NAME

data_out <- cbind(y_original, x, Municipality_Name)

colnames(data_out) <- c("PM25_original", covariates_of_interest, "Municipality_Name")

write.csv(data_out, file = "data/formated_input_data.csv", row.names = FALSE)
