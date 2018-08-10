### Get regionalisation
grid_domain = get_grid_for_classification(coords = coords %>% select(x, y),
                                          grid_space = grid_space,
                                          min_dist = min_dist,
                                          restrict_aus = restrict_aus)
#
# # Create grid for classification
# long = range(coords$x) + c(-grid_space, grid_space)
# lat  = range(coords$y) + c(-grid_space, grid_space)
# long.seq = seq(min(long), max(long), by = grid_space)
# lat.seq = seq(min(lat), max(lat), by = grid_space)
# full_grid = expand.grid(x = long.seq, y = lat.seq)
#
# bool_pip = pnt.in.poly(full_grid,
#                        region_info %>% select(long, lat))$pip
# grid_domain = full_grid[bool_pip == TRUE, ]s

grid_classify = apply(hclusters %>% select(-k, -h), 1,
                      classify_with_kknn,
                      coords = coords,
                      points_classify = grid_domain,
                      knn_value = knn_value)

rep_k <- rep(hclusters$k, each = nrow(grid_domain))
rep_h <- rep(hclusters$h, each = nrow(grid_domain))

grid_classify <- do.call(rbind, grid_classify)
grid_classify <- grid_classify %>%
  mutate(k = rep_k, h = rep_h)
