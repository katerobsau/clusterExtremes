get_classification_grid <- function(coords, grid_space, min_dist){

  if(missing(grid_space)) grid_space = 1
  if(missing(min_dist)) min_dist = 1.5*grid_space

  # Get Australian outline
  mainland_df <- utils_mainland()
  tas_df <- utils_tasmania()

  # Create grid for classification
  long = range(coords$x)
  lat  = range(coords$y)
  long.seq = seq(min(long), max(long), by = grid_space)
  lat.seq = seq(min(lat), max(lat), by = grid_space)
  full_grid = expand.grid(longitude = long.seq, latitude = lat.seq)

  # restrict grid to within australia
  mainland_i = pnt.in.poly(full_grid, mainland_df)$pip
  tas_i = pnt.in.poly(full_grid, tas_df)$pip
  in_grid = full_grid[which(mainland_i == 1 | tas_i == 1), ]

  #Remove any point that is not within min distance to a station
  print("PARALLELISE THIS PART!!")
  theta = seq(0, 2*pi, length.out = 360)
  pnt.check = rep(0, nrow(in_grid))
  for(i in 1:nrow(coords)){
    print(i)
    pnt = as.numeric(coords[i,] %>% select(x,y))
    circle = cbind(min_dist*cos(theta) + pnt[1], min_dist*sin(theta) + pnt[2])
    pnt.in.circle = pnt.in.poly(in_grid, circle)$pip
    pnt.check = pnt.check + pnt.in.circle
  }

  grid = in_grid[pnt.check > 0, ]

  return(grid)

}
