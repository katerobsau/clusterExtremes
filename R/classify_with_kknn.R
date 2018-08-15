#' Classify a grid using weighted k nearest neighbours
#'
#' Function takes a set of points that have been clustered,
#' and then classifys the grid.
#'
#' @param coords a data frame of coordinates with columns
#' labelled x and y
#' @param cluster_ids the cluster_ids corresponding to the coords
#' the forumula for classification, is cluster_id ~ x + y + xy
#' @param points_classify a data frame of coordinates for classification,
#' with columns labelled x and y
#' @param knn_value the numbere of k nearest neighbours to consider
#' @param kernel_type the type of kernel used in kknn(), see documentation.
#' Default is "inv" for inverse distance weighted
#' @param distance parameter associated with the minokowski distance,
#' we use a default of 2 for euclidean
#'
#' @return points_classify with a new column
#' for the classification, class_id and a new columns giving the majority
#' probability for this classification
#'
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' n.site <- 60
#' coords <-data.frame(x = runif(n.site, 0, 10), y = runif(n.site, 0, 10))
#' grid_domain = get_grid_for_classification(coords = coords,
#'      restrict_aus = FALSE)
#'
#' cluster_ids = rep(1, nrow(coords))
#' cluster_ids[coords$x < 4] = 2
#'
#' grid_output = classify_with_kknn(coords = coords,
#'        cluster_ids = cluster_ids
#'        points_classify = grid_domain,
#'  knn_value = 3)
#'
#' ggplot() +
#'  geom_point(data = grid_output,
#'     aes(x=x, y =y, col = as.factor(class_id),
#'     alpha = prob_summary), shape = 3) +
#'  geom_point(data = coords,
#'     aes(x=x, y =y, col = as.factor(cluster_id)))
#'
classify_with_kknn <- function(coords,
                               cluster_ids,
                               points_classify,
                               knn_value,
                               kernel_type = "inv",
                               distance = 2){

  if(length(cluster_ids) != nrow(coords))
    stop("Error: number of cluster_ids does not match coords")

  set_learn <- coords %>%
    mutate(cluster_id = cluster_ids) %>%
    select(x,y,cluster_id)
  set_learn$cluster_id <- as.factor(set_learn$cluster_id)

  results_kknn <- kknn::kknn(cluster_id ~ ., train = set_learn,
                  test = points_classify, distance = distance,
                  kernel = kernel_type, k = knn_value)
  results_fit <- fitted(results_kknn)
  points_classify$class_id = results_fit

  prob_df <- results_kknn$prob
  max_j <- apply(prob_df, 1, which.max)
  prob_summary <- mapply(function(i,j,mat){mat[i,j]},
                         i = 1:nrow(prob_df), j = max_j,
                         MoreArgs = list(mat = prob_df))
  points_classify$prob_summary <- prob_summary

  return(points_classify)

}
