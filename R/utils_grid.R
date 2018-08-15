#' @export

utils_reduce_grid <- function(coords, full_grid, min_dist = 1){

  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  list_grid = foreach(i = 1:nrow(coords), .packages = c("dplyr", "SDMTools")) %dopar%
    utils_get_nearby_cells(full_grid = full_grid, coords = coords, min_dist = min_dist)
  stopCluster(cl)

  grid_near_points <- do.call(rbind, list_grid) %>% distinct()

  return(grid_near_points)
}

