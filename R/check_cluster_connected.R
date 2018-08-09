#' Check if a regions is connected
#'
#' Takes a set of coordinates and overlays a grid. Using the grid, we check if
#' grid regions are connected. Referencing is done relative to the lower left
#' corner of the grid. If desired, this function returns as boolean classification
#' relative the location of a representative object.
#'
#' To reference coordinates we are interested in we use a boolean classification.
#' If we want to consider all points, then the classifcation is all 1.
#' If we want to consider a subset, set those points we aren't intersted in to 0,
#' those we are interested in to 1.
#'
#' @param coords data frame with columns x, y and class_id
#' @param grid a data frame that describes a grid, columns of x, y and class_id
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' coords = data.frame(x = runif(100)*10, y = runif(100)*10)
#' coords = coords %>%
#'   dplyr::mutate(class_id = 1) %>%
#'   dplyr::mutate(class_id = ifelse(x <= 3 & y <= 3, 0, class_id)) %>%
#'   dplyr::mutate(class_id = ifelse(x >= 8 & y >= 2, 0, class_id))
#'
#' coord_plot <-ggplot() +
#'   geom_point(data = coords, aes(x = x, y= y,
#'                                 col = as.factor(class_id)))
#' coord_plot
#'
#' grid_space = 1
#' x_range = range(coords[,1])
#' y_range = range(coords[,2])
#' grid = expand.grid(x = seq(x_range[1], x_range[2], by = grid_space),
#'                    y = seq(y_range[1], y_range[2], by = grid_space)) %>%
#'        as.data.frame() %>%
#'        dplyr::mutate(class_id = 1) %>%
#'        dplyr::mutate(class_id = ifelse(x <= 3 & y <= 3, 0, class_id)) %>%
#'        dplyr::mutate(class_id = ifelse(x >= 8 & y >= 2, 0, class_id))
#'
#' grid_plot <- coord_plot +
#'   geom_point(data = grid,
#'       aes(x = x, y = y, col = as.factor(class_id)), shape = 3)
#' grid_plot
#'
#' connected = check_clusters_connected(coords = coords, grid = grid)
#'
#' connected_plot <- ggplot(data = coords, aes(x = x, y = y)) +
#'   geom_point(aes(col = as.factor(connected)))
#'
#' connected_plot
#'
check_clusters_connected <- function(coords, grid){

  grid_space = min(dist(grid %>% select(x,y)))

  all_coords = rbind(coords %>% select(x,y),
                     grid %>% select(x,y))

  class_coords = rbind(coords %>% select(class_id),
                       grid %>% select(class_id))

  dd = dist(all_coords)
  dd_class  = dist(cbind(class_coords, class_coords))

  dd_sum = dd + dd_class
  db = dbscan::dbscan(dd_sum, eps = grid_space, minPts = 1)

  db_id = db$cluster[1:nrow(coords)]

  return(db_id)

}
