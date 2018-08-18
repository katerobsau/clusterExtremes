### Get regionalisation

grid_domain = get_grid_for_classification(coords = coords, grid_space = grid_space,
                            min_dist = min_dist)

# # Create grid for classification
# full_grid = generate_grid(coords = coords, grid_space = grid_space)
# print("Generated the grid")
#
# # Reduce points (fast)
# near_grid = utils_reduce_grid(coords = coords, full_grid = full_grid, min_dist = min_dist)
# print("Restricted the grid to be nearby points")
#
# # Check within australia (bit slow ~ 1 minute)
# grid_domain = utils_restrict_to_aus(near_grid = near_grid)
# print("Restricted the grid to within Australia")

# Get classificaiton for grid
cluster_ids_mat <- hclusters %>% select(-k, -h) %>% as.matrix()
cl <- makeCluster(detectCores())
registerDoParallel(cl)
# grid_classify = apply(hclusters %>% select(-k, -h), 1,
#                       classify_with_kknn,
#                       coords = coords,
#                       points_classify = grid_domain,
#                       knn_value = knn_value)
grid_classify <- foreach(i = 1:nrow(cluster_ids_mat),
                         .packages = c("clusterExtremes")) %dopar%
 classify_with_kknn(coords = coords,
                   cluster_ids =cluster_ids_mat[i,],
                   points_classify = grid_domain,
                   knn_value = knn_value)
stopCluster(cl)

rep_k <- rep(hclusters$k, each = nrow(grid_domain))
rep_h <- rep(hclusters$h, each = nrow(grid_domain))

grid_classify <- do.call(rbind, grid_classify)
grid_classify <- grid_classify %>%
  mutate(k = rep_k, h = rep_h)
