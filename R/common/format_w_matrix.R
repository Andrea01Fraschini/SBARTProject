#' Format Weight Matrix
#'
#' This function checks the validity of the input weight matrix W and formats it into a triplet form. 
#' It also calculates the sum and the number of neighbours for each row of W, and the start and finish points for W updating.
#'
#' @param W A square numeric matrix representing the weight matrix. It should not contain any NA or negative values, and it should be symmetric.
#'
#' @return A list containing the following elements:
#'   - W: The original weight matrix.
#'   - W.triplet: The weight matrix in triplet form.
#'   - n.triplet: The number of rows in W.triplet.
#'   - W.triplet.sum: The sum of weights for each row of W.
#'   - n.neighbours: The number of neighbours for each row of W.
#'   - W.begfin: The start and finish points for W updating.
#'   - n.rows: The number of rows in W.
#'
#' @examples
#' W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
#' result = format_w_matrix(W)
#' @export
#'
format_w_matrix <- function(W) {
    # check matrix dimension
    if(!is.matrix(W)) stop("W is not a matrix.", call.=FALSE)
    
    n.rows <- nrow(W)
    if(ncol(W)!= n.rows) stop("W is not a square matrix.", call.=FALSE)    
    
    ## check validity of inputed W matrix
    if(sum(is.na(W)) > 0) stop("W has missing 'NA' values.", call.=FALSE)
    if(!is.numeric(W)) stop("W has non-numeric values.", call.=FALSE)
    if(min(W) < 0) stop("W has negative elements.", call.=FALSE)
    if(sum(W != t(W)) > 0) stop("W is not symmetric.", call.=FALSE)

    temp <- reshape::melt(t(W))
    temp <- temp[, c(2, 1, 3)]
    # remove all zeros 
    W.triplet <- subset(temp, value > 0) 
    
    names(W.triplet) <- NULL 
    
    W.triplet <- as.matrix(W.triplet)
    n.triplet <- nrow(W.triplet)
    W.triplet.sum <- tapply(W.triplet[, 3], W.triplet[, 1], sum)
    n.neighbours <- tapply(W.triplet[, 3], W.triplet[, 1], length)

    # create the start and finish points for W updating 
    W.begfin <- array(NA, c(n.rows, 2))
    temp <- 1 
    for (i in 1:n.rows) {
        W.begfin[i, ] <- c(temp, (temp + n.neighbours[i] - 1))
        temp <- temp + n.neighbours[i]
    }

    # return the critical quantities
    results <- list(
        W = W, 
        W.triplet = W.triplet, 
        n.triplet = n.triplet, 
        W.triplet.sum = W.triplet.sum, 
        n.neighbours = n.neighbours, 
        W.begfin = W.begfin, 
        n.rows = n.rows
    ) 

    return(results)
}