# library(readr)
source("config.R")

files <- list.files("output/", pattern = "*.RData")
order_data <- read.csv("python/AreasCentroids.csv")

all_data <- data.frame()

for (file in files) {
    if(file == "model.RData" || file == "Oct2017_March2018_30wind_10m_no_Milano_pt2.RData" || file == "Oct2017_March2018_30wind_10m_no_Milano.RData" || file == "Oct2017_March2018_30wind_10m.RData" || file == "2018_full_year_model_60wind_100m_0.RData") next
    print(paste0("Processing file: ", file)) # print the file name
    load(paste0("output/", file))
    covariates_inclusion <- model$covariates_selection_chain
    colnames(covariates_inclusion) <- covariates_of_interest
    all_data <- rbind(all_data, covariates_inclusion)
}

write.csv(all_data, file = paste0("output/", "all_covariates_inclusion", ".csv"), row.names = FALSE)