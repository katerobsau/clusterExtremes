#' @export
#'
utils_get_nearby_cells <- function(i, full_grid, coords, min_dist = 1){

  pnt = c(coords$x[i],coords$y[i])

  # rough filter
  keep_cells <- full_grid %>%
    dplyr::filter(x > pnt[1] - min_dist & x < min_dist + pnt[1]) %>%
    dplyr::filter(y > pnt[2] - min_dist & y < min_dist + pnt[2])

  # restrict to points within a min_dist radius
  theta = seq(0, 2*pi, length.out = 360)
  circle = cbind(min_dist*cos(theta), min_dist*sin(theta))

  pnt_circle = cbind(circle[,1] + pnt[1], circle[,2] + pnt[2])
  pnt_in_circle = pnt.in.poly(keep_cells %>% select(x,y), pnt_circle)$pip

  keep_cells <- keep_cells[pnt_in_circle == TRUE, ]

  return(keep_cells)
}
