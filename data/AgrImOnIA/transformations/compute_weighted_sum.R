compute_weighted_sum <- function(df_intersected, covariates_of_interest) {
  # Specify the column indices for the covariates (26 to 51)
  covariate_indices <- match(covariates_of_interest, colnames(df_intersected))

  # Convert the indices to column names
  covariate_names <- colnames(df_intersected)[covariate_indices]

  # Create a new data frame to store the results
  df_result <- data.frame(NOME_COM = unique(df_intersected$NOME_COM))
  
  # Initialize covariate columns to zero
  for (covariate in covariate_names) {
    df_result[, covariate] <- 0 
  }
  
  for (municipality in df_result$NOME_COM) {
    # Subset the df_intersected data for the current municipality
    municipality_data <- df_intersected[df_intersected$NOME_COM == municipality, ]
    
    # Extract non-geographic data (assuming covariates are after geometry column)
    covariate_data <- as.data.frame(municipality_data[, c("area", covariate_names)])
    
    # Loop through each covariate and calculate the weighted sum
    for (covariate in covariate_names) {
      weighted_sum <- sum(covariate_data$area * covariate_data[, covariate], na.rm = TRUE) / sum(covariate_data$area, na.rm = TRUE)
      
      # Assign the result to the corresponding cell in the df_result
      df_result[df_result$NOME_COM == municipality, covariate] <- weighted_sum
    }
  }

  
  
  return(df_result)
}