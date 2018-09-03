get_num_common_obs <- function(data){
  pair_count_mat <- count.pairwise(data)
  pair_count_dist <- pair_count_mat %>% as.dist(upper = TRUE, diag = FALSE)
  if(any(pair_count_dist > nrow(data) |
         pair_count_dist < 0) == 1)
    stop("ERROR: common count is incorrect")
  return(pair_count_dist)
}
