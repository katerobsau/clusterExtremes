#' Generate a grid
#'
#' Function generates a grid to cover a set of given coordinates
#' (additional funcitonality to restrict the grid to inland areas of Australia)
#'
#' @param coords a data frame of coordinates with columns labelled x and y
#' @param grid_space the spacing for the grid, equal in both x and y directions
#' (defaults to 1)
#'
#' @return returns a data frame of gridded coordinates with columns labelled x and y
#' @export
#'
#' @examples
#' set.seed(1)
#' n.site <- 30
#' coords <-data.frame(x = runif(n.site, 0, 10), y = runif(n.site, 0, 10))
#' grid_output = generate_grid(coords = coords)
#' plot(grid_output)
#' points(coords, col = "red", pch = 20)
#'
generate_grid <- function(coords, grid_space = 1){

  # Create grid for classification
  long_range = range(coords$x) + c(-grid_space, grid_space)
  lat_range  = range(coords$y) + c(-grid_space, grid_space)
  long_seq = seq(min(long_range), max(long_range), by = grid_space)
  lat_seq = seq(min(lat_range), max(lat_range), by = grid_space)
  full_grid = expand.grid(x = long_seq, y = lat_seq)

  return(full_grid)

}
