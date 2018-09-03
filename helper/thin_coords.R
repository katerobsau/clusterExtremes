thin_coords <- function(coords, thin_dx, thin_dy, max_per_grid){

  # thin the eastern australian cluster
  x_range = range(coords$x)
  y_range = range(coords$y)
  grd = expand.grid(x =  seq(x_range[1], x_range[2], by = thin_dx),
                    y =  seq(y_range[1], y_range[2], by = thin_dy))

  temp_coords = coords
  keep_coords = NULL
  for(i in 1:nrow(grd)){
    cell_coords <- filter(temp_coords,
                          x >= grd[i,1] &  x < grd[i,1] + thin_dx &
                           y >= grd[i,2] &  y < grd[i,2] + thin_dy)

    if(nrow(cell_coords) == 0) next

    # points(cell_coords$x, cell_coords$y, col = "blue", pch = 20)

    if(nrow(cell_coords) > max_per_grid){
      row_samp = sample(1:nrow(cell_coords), max_per_grid)
      cell_coords = cell_coords[row_samp, ]
      # points(cell_coords$x, cell_coords$y, col = "red", pch = 20)
    }

    keep_coords = rbind(keep_coords, cell_coords)

  }

  coords_thin = filter(coords, id %in% keep_coords$id)

  return(coords_thin)

}
