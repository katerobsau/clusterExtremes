#' @export
utils_restrict_to_aus <- function(near_grid){

  if(min(near_grid$y) < -40.5){
    tas_df <- utils_tasmania()
    tas_bool = pnt.in.poly(near_grid, tas_df)$pip
  }else{
    tas_bool = rep(FALSE, nrow(near_grid))
  }

  if(max(near_grid$y) > -40.5){
    mainland_df <- utils_mainland()
    mainland_bool = pnt.in.poly(near_grid, mainland_df)$pip
  }else{
    mainland_bool = rep(FALSE, nrow(near_grid))
  }

  poly_bool = tas_bool | mainland_bool
  in_grid = near_grid[poly_bool == TRUE, ]

  return(in_grid)

}
