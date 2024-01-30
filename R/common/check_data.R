check_data <- function(data){
    # Check that the diagonal of each weight matrix is all zero
    for(i in 1:length(data$ws)) {
        if (any(diag(data$ws[[i]]) != 0)) {
            return(
                list(
                    message = "The diagonal of each weight matrix must be all zero",
                    error = TRUE
                )
            )
        }
    }

    # Check that the weight matrices are symmetric
    for(i in 1:length(data$ws)) {
        if (any(data$ws[[i]] != t(data$ws[[i]]))) {
            return(
                list(
                    message = "The weight matrices must be symmetric",
                    error = TRUE
                )
            )
        }
    }
}