get_mean <- function (df, group_by_columns){
    # Remove Time column
    df <- df %>% select(-Time)

    df <- df %>%
        group_by(across(all_of(group_by_columns))) %>%
        summarise(across(everything(), mean, na.rm = TRUE))
    return(df)
}