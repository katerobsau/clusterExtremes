#' Generate a grid
#'
#' Function generates a grid to cover a set of given coordiantes
#' (additional funcitonality to restrict the grid to inland areas of Australia)
#'
#' @param coords a data frame of coordinates with columns labelled x and y
#' @param grid_space the spacing for the grid, equal in both x and y directions
#' (defaults to 1)
#' @param min_dist we restrict the grid to only cover the provided coordinates
#' any grid point greater than the min_dist away is removed (default is 1.5*grid_space)
#' @return returns a data frame of gridded coordinates with columns labelled x and y
#' @export
#'
#' @examples
#' set.seed(1)
#' n.site <- 30
#' coords <-data.frame(x = runif(n.site, 0, 10), y = runif(n.site, 0, 10))
#' grid_output = get_grid_for_classification(coords = coords)
#' plot(grid_output)
#' points(coords, col = "red", pch = 20)
#'
#' poly_df <- data.frame(x = c(1,5,6,2,1), y = c(1,2,8,7,1))
#' grid_restricted = get_grid_for_classification(coords = coords, poly_df = poly_df)
#' plot(grid_output, pch = 3)
#' points(grid_restricted, col = "blue", pch = 3)
#' points(coords, col = "red", pch = 20)
#'
get_grid_for_classification <- function(coords, grid_space = 1,
                                    min_dist){

  if(missing(min_dist)){min_dist = 1.5*grid_space}

  # Create grid for classification
  full_grid = generate_grid(coords = coords, grid_space = grid_space)
  print("Generated the grid")

  # Reduce points (fast)
  near_grid = utils_reduce_grid(coords = coords, full_grid = full_grid, min_dist = min_dist)
  print("Restricted the grid to be nearby points")

  # Check within australia (bit slow ~ 1 minute)
  grid_domain = utils_restrict_to_aus(near_grid = near_grid)
  print("Restricted the grid to within Australia")

  return(grid_domain)

}
