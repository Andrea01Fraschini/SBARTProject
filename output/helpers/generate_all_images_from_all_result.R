# library(readr)
source("config.R")
source("data/AgrImOnIA/main_by_range.R")

files <- list.files("output/", pattern = "*.RData")
order_data <- read.csv("python/AreasCentroids.csv")

for (file in files) {
    if(file == "model.RData" || file == "Oct2017_March2018_30wind_10m_no_Milano_pt2.RData" || file == "Oct2017_March2018_30wind_10m_no_Milano.RData" || file == "Oct2017_March2018_30wind_10m.RData") next
    print(paste0("Processing file: ", file)) # print the file name
    load(paste0("output/", file))

    if(file == "2017_low_season_model_30wind_10m_0.RData"){
        date_begin <- "2017-04-01"
        date_end <- "2017-10-31"
    } else if(file == "2018_full_year_model_30wind_10m_0.RData" || file == "2018_full_year_model_60wind_100m_0.RData"){
        date_begin <- "2018-01-01"
        date_end <- "2018-12-31"
    } else if(file == "2018_high_season_year_model_30wind_10m_0.RData" ){
        date_begin <- "2018-11-17"
        date_end <- "2019-04-24"
    } else if(file == "2018_low_season_model_30wind_10m_0.RData" || file == "2018_low_season_model_60wind_10m_0.RData"){
        date_begin <- "2018-04-01"
        date_end <- "2018-10-31"
    } else if(file == "2019_04_to_12_model_30wind_10m_0.RData"){
        date_begin <- "2019-04-01"
        date_end <- "2019-12-31"
    } else if(file == "2019_2020_sept_feb_model_60wind_100m_0.RData"){
        date_begin <- "2019-09-01"
        date_end <- "2020-02-29"
    } else if(file == "2019_march_august_model_60wind_100m_0.RData" ){
        date_begin <- "2019-03-01"
        date_end <- "2019-08-31"
    }else{
        print("Error: date not found")
    }

    generate_data(date_begin, date_end, response_variable, covariates_of_interest)
    load("data/input_data.RData")
    
    y_complete <- model$y_predictions
    y_original <- merged_data$Y

    Municipality_Name <- order_data$MUNICIPALITY_NAME
    data_out <- cbind(y_original, y_complete, Municipality_Name)

    colnames(data_out) <- c("PM25_original", "PM25_predictions", "Municipality_Name")

    covariates_inclusion <- model$covariates_selection_chain
    colnames(covariates_inclusion) <- covariates_of_interest

    # Remove from filename the extension
    file <- gsub("\\.RData", "", file)

    write.csv(data_out, file = paste0("output/", file, ".csv"), row.names = FALSE)
    write.csv(covariates_inclusion, file = paste0("output/", file, "_cov_inclusion", ".csv"), row.names = FALSE)
}

write.csv(all_data, file = paste0("output/", "all_covariates_inclusion", ".csv"), row.names = FALSE)