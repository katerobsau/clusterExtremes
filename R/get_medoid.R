#' Get a representative object for a set of points
#'
#' This function find the point that minimises the distances to all the other
#' coordinates. This point is returned as a representative object
#'
#' @param coords data frame with columns x, y
#' @param DD a distance object related to the coordinates
#'
#' @return Returns a representative coordinates
#' @export
#' @examples
#'
#'
#' coords = expand.grid(x = 1:9, y = 1:9)
#' DD = dist(coords, upper = TRUE, diag = FALSE)
#' medoid = get_medoid(coords, DD)
#' plot(coords)
#' points(medoid, col = "red", pch = 3)
#'
#' set.seed(1)
#' coords = data.frame(x = runif(100), y = runif(100))
#' DD = dist(coords, upper = TRUE, diag = FALSE)
#' medoid = get_medoid(coords, DD)
#' plot(coords)
#' points(medoid, col = "red", pch = 3)
#'
get_medoid <- function(coords, DD){

  DD_vec = DD %>%
    as.matrix() %>%
    as.vector()

  all_pnts = expand.grid(x = 1:nrow(coords), y = 1:nrow(coords)) %>%
    as.data.frame() %>%
    mutate(dist = DD_vec)

  relative_dist = rep(NA, nrow(coords))
  for(i in 1:nrow(coords)){
    relative_dist[i] <- all_pnts %>%
      filter(x == i) %>%
      summarise(total_dist = sum(dist)) %>%
      as.numeric
  }

  medoid_i = which.min(relative_dist)
  medoid = coords[medoid_i, ]

  return(medoid)

}
