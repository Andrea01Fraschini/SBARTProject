clean_data <- function(df, columns_to_remove) {
  # Remove rows with NA values
  df_cleaned <- na.omit(df)
  
  # Convert Time column to Date type
  if ("Time" %in% colnames(df_cleaned)) {
    df_cleaned$Time <- as.Date(df_cleaned$Time)
  }
  
  # Exclude specified columns
  if (length(columns_to_remove) > 0) {
    df_cleaned <- df_cleaned %>% select(-one_of(columns_to_remove))
  }
  
  # Convert all columns to numeric type
  for (col_name in colnames(df_cleaned)) {
    df_cleaned[[col_name]] <- as.numeric(df_cleaned[[col_name]])
  }
  
  # Remove rows with NA values after conversion to numeric
  df_cleaned <- na.omit(df_cleaned)
  
  return(df_cleaned)
}