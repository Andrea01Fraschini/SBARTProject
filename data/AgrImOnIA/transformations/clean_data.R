clean_data <- function(df, columns_to_remove) {
  # Remove rows with NA values
  # df_cleaned <- na.omit(df)

  # Remove all NAs that are strings
  # df_cleaned <- df_cleaned[!apply(df_cleaned, 1, function(row) any(grepl("(?i).*NA.*", row))), , drop = FALSE]

  df_cleaned <- df

  # Convert Time column to Date type
  df_cleaned$Time <- as.Date(df_cleaned$Time)
  
  # Exclude specified columns
  if (length(columns_to_remove) > 0) {
    df_cleaned <- df_cleaned %>% dplyr::select(-one_of(columns_to_remove))
  }
  
  # Convert all columns to numeric type
  for (col_name in setdiff(colnames(df_cleaned), c("Time"))) {
    df_cleaned[[col_name]] <- as.numeric(df_cleaned[[col_name]])
  }

  # Remove rows with NA values after conversion to numeric
  # df_cleaned <- na.omit(df_cleaned)
  
  return(df_cleaned)
}