transform_dt_list <- function(dt_list) {
  dt_list_transformed <- lapply(dt_list, function(dt) {
    dt$terminal <- ifelse(dt$Terminal == 1, TRUE, FALSE)
    dt$split <- dt$Split
    dt$value <- dt$Value
    dt$mu <- dt$MU

    dt$Terminal <- NULL
    dt$Split <- NULL
    dt$Value <- NULL
    dt$MU <- NULL

    temp <- dt$begin
    temp2 <- dt$end

    dt$begin <- NULL
    dt$end <- NULL

    dt$begin <- temp
    dt$end <- temp2
    
    return(dt)
  })
  
  return(dt_list_transformed)
}