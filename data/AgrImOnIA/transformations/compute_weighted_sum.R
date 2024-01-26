compute_weighted_sum <- function(df, covariate_indices) {
  # Remove the geometry column if it exists
  if ("geometry" %in% colnames(df)) {
    df <- df %>% select(-geometry)
  }
  
  # Calculate the weighted sum
  weighted_sum <- df %>%
    group_by(NOME_COM) %>%
    summarise(
      sum_area = sum(area),  # Sum of the areas
      across(covariate_indices, ~ sum(. * area) / sum(area), .names = "weighted_sum_{.col}")
    )
  
  # Return the result
  return(weighted_sum)
}