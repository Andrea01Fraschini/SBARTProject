#' Update residuals
#'
#' This function calculates the residuals for a given set of parameters.
#' The formula used is: R_{i,(-t)} = y_i - ∑^T_{r ≠ t} g(x_i; T_t, M_t) - θ_i
#'
#' @param y A numeric vector of target values.
#' @param trees A matrix where each column represents a tree.
#' @param t An integer representing the current tree index.
#' @param spatial_theta A numeric vector of spatial parameters.
#' @param missing_indexes A numeric vector of indexes where data is missing.
#'
#' @return A numeric vector of residuals.
#' @export
#'
update_residuals <- function(y, trees, t, spatial_theta, missing_indexes) {

  residuals <- y - rowSums(trees[, -t]) - spatial_theta[-missing_indexes]
  
  return(residuals)
}