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

    # Check that data$y and data$x_predictors are of the same length
    if (length(data$y) != nrow(data$x_predictors)) {
        return(
            list(
                message = "The response variable and the predictors must be of the same length",
                error = TRUE
            )
        )
    }

    # Check that the missing indexes correspond to missing values in data$y
    if (any(is.na(data$y[data$missing_indexes]) == FALSE)) {
        return(
            list(
                message = "The missing indexes must correspond to missing values in the response variable",
                error = TRUE
            )
        )
    }

    # Check that the wind matrix is symmetric
    if (any(data$wind_matrix != t(data$wind_matrix))) {
        return(
            list(
                message = "The wind matrix must be symmetric",
                error = TRUE
            )
        )
    }

    # Check that the wind matrix is square
    if (nrow(data$wind_matrix) != ncol(data$wind_matrix)) {
        return(
            list(
                message = "The wind matrix must be square",
                error = TRUE
            )
        )
    }

    # Check that the wind matrix is binary
    if (any(data$wind_matrix != 0 & data$wind_matrix != 1)) {
        return(
            list(
                message = "The wind matrix must be binary",
                error = TRUE
            )
        )
    }

    # Check that the wind matrix has the same number of rows as the number of rows in data$x_predictors
    if (nrow(data$wind_matrix) != nrow(data$x_predictors)) {
        return(
            list(
                message = "The wind matrix must have the same number of rows as the number of rows in the predictors",
                error = TRUE
            )
        )
    }


    return (
        list(
            message = "Data is valid",
            error = FALSE
        )
    )   
}