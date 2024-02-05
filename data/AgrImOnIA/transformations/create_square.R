create_square <- function(center, side_length) {
  half_side <- side_length / 2
  square_coords <- matrix(
    c(center[1] - half_side, center[2] - half_side,
      center[1] + half_side, center[2] - half_side,
      center[1] + half_side, center[2] + half_side,
      center[1] - half_side, center[2] + half_side,
      center[1] - half_side, center[2] - half_side),
    ncol = 2,
    byrow = TRUE
  )
  st_polygon(list(square_coords))
}