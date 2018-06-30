#' Check the classfied regions are connected
#'
#' Under the classification, regions can be disconnected. 
#' For fitting max-stable models, we do not want to group stations in 
#' disjoint regions. This function returns as boolean if the station is density 
#' connected to it's medoid
#'
#' For the two types of classification, we use 0 to denote the classfication 
#' we are interested in and 1 otherwise. 
#'
#' @param coords data frame with columns x, y and class_id
#' class_id takes boolean values of 0 and 1
#' @param medoid index of the medoid in the coords data frame
#' @param grid a data frame that describes a grid, columsn of x, y and class_id
#' @param grid_space spacing of the grid, equal in x and y
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
#' @examples
#'
#' coords = data.frame(x = runif(100)*10, y = runif(100)*10)
#' class_id = rep(1, nrow(coords))
#' coords = coords %>%
#'   mutate(class_id = class_id) %>%
#'   mutate(class_id = ifelse(x <= 3 & y <= 3, 0, class_id)) %>%
#'   mutate(class_id = ifelse(x >= 8 & y >= 2, 0, class_id))
#' 
#' medoid_coords = data.frame(x = 0.5, y = 0.5, class_id = 0)
#' coords = rbind(coords, medoid_coords)
#' medoid = nrow(coords)
#' 
#' coord_plot <-ggplot() +
#'   geom_point(data = coords, aes(x = x, y= y, 
#'                                 col = as.factor(class_id))) +
#'   geom_point(data = medoid_coords, aes(x=x, y =y))
#' 
#' coord_plot
#' 
#' grid_space = 1
#' x_range = range(coords[,1])
#' y_range = range(coords[,2])
#' grid = expand.grid(x = seq(x_range[1], x_range[2], by = grid_space),
#'                    y = seq(y_range[1], y_range[2], by = grid_space))
#' 
#' training_set = coords %>% select(x,y) %>% as.matrix()
#' labels = coords %>% select(class_id) %>% unlist()
#' knn_classification = knn(train = training_set, 
#'                          test = grid, 
#'                          cl = labels, 
#'                          k = 1)
#' 
#' grid = mutate(grid, class_id = knn_classification)
#' 
#' grid_plot <-ggplot() +
#'   geom_point(data = coords, aes(x = x, y= y,
#'                                 col = as.factor(class_id))) +
#'   geom_point(data = grid, aes(x = x, y = y,
#'                               col = as.factor(class_id)),
#'              shape = 3) +
#'   geom_point(data = medoid_coords, aes(x=x, y =y))
#' 
#' grid_plot
#' 
#' connected = check_clusters_connected(coords, medoid, grid)
#' 
#' connected_plot <- ggplot(data = coords, aes(x = x, y = y)) +
#'   geom_point(aes(col = as.factor(connected)))
#' 
#' connected_plot 

check_clusters_connected <- function(coords, medoid, 
                                     grid){
  grid_space = min(dist(grid))
  
  all_coords = rbind(coords %>% select(x,y), 
                     grid %>% select(x,y))
  
  class_coords = rbind(coords %>% select(class_id),
                       grid %>% select(class_id))
  
  dd = dist(all_coords)
  dd_class  = dist(cbind(class_coords, class_coords))
  
  dd_sum = dd + dd_class
  db = dbscan(dd_sum, eps = grid_space, minPts = 1)
  
  medoid_coords <- coords[medoid, ]
  medoid_db <- db$cluster[medoid]
  
  density_connected <- all_coords %>%
    mutate(db_id = db$cluster) %>%
    mutate(db_id = ifelse(db_id == medoid_db, 1, 0)) %>%
    select(db_id) %>%
    unlist()
  
  density_connected = density_connected[1:nrow(coords)]
  
  return(density_connected)
  
}
