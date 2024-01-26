get_mean_by_year <- function(df) {
    if (!("Time" %in% colnames(df))) {
        stop("The 'Time' column does not exist")
    }

    df$year <- lubridate::year(as.Date(df$Time))

    result_data <- df %>%
        group_by(Latitude, Longitude, year) %>%
        summarise(across(
            .fns = ~mean(., na.rm = TRUE)
        ), .groups = "drop")
  
    return(result_data)
}