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
#' @param poly_df check if the coords fall within a polygon,
#' input is a data frame with columns x and y (default =  NULL)
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
                                    min_dist,
                                    poly_df = NULL){

  if(missing(min_dist)){min_dist = 1.5*grid_space}

  # Create grid for classification
  full_grid = generate_grid(coords, grid_space)

  # (optional) restrict grid to within australia
  if(!is.null(poly_df)){
    poly_bool = pnt.in.poly(full_grid, poly_df)$pip
    in_grid = full_grid[poly_bool == TRUE, ]
  }else{
    in_grid = full_grid
  }

  #Remove any point that is not within min distance to a station
  # print("PARALLELISE THIS PART!!")
  # theta = seq(0, 2*pi, length.out = 360)
  # pnt.check = rep(0, nrow(in_grid))
  # for(i in 1:nrow(coords)){
  #   print(i)
  #   pnt = as.numeric(coords[i,] %>% select(x,y))
  #   circle = cbind(min_dist*cos(theta) + pnt[1], min_dist*sin(theta) + pnt[2])
  #   pnt.in.circle = pnt.in.poly(in_grid, circle)$pip
  #   pnt.check = pnt.check + pnt.in.circle
  # }

  # restrict grid to a min_dist near our coordinates
  theta = seq(0, 2*pi, length.out = 360)
  circle = cbind(min_dist*cos(theta), min_dist*sin(theta))
  grid_loop_fun <- function(i, coords, circle, in_grid){
    pnt = c(coords$x[i],coords$y[i])
    pnt_circle = cbind(circle[,1] + pnt[1], circle[,2] + pnt[2])
    pnt.in.circle = pnt.in.poly(in_grid, pnt_circle)$pip
    return(pnt.in.circle)
  }

  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  pnt_in_circle = foreach(i = 1:nrow(coords),
                      .packages = c("SDMTools")) %dopar%
    grid_loop_fun(i = i, coords = coords, circle = circle,
                  in_grid = in_grid)
  pnt_in_circle = do.call(rbind, pnt_in_circle)
  pnt_check = colSums(pnt_in_circle)
  grid = in_grid[pnt_check > 0, ]

  return(grid)

}
