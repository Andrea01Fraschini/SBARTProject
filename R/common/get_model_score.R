get_model_score <- function(y_predictions, y_actual, missing_indexes) {
    # Calculate the score using the mean relative error
    score <- 1 -  mean(abs(y_predictions[missing_indexes] - y_actual[missing_indexes]) / y_actual[missing_indexes])
    return(score)
}