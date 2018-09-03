#------------------------------------------------------------------------------
#
# # is disjoint ?? (if yes, create new id in class and cluster ids)
#
# grid_k <- grid_classify %>%
#   dplyr::filter(k == num_k)
#
# cluster_ids = hclusters %>%
#   dplyr::filter(k == num_k) %>%
#   dplyr::select(-k, -h) %>%
#   as.numeric()
#
# temp_coords <- coords %>%
#   dplyr::mutate(class_id = cluster_ids)
#
# connected = check_clusters_connected(coords = temp_coords, grid = grid_k)
#
# ## NEED TO USE GRID NOT COORDINATES (PRODUCTING TOO MANY DISCONNECTED REGIONS)
## JUST DOESN"T BLOODY WORK ***BLOWS RASPBERRY***
# connected_plot <- ggplot(data = coords, aes(x = x, y = y)) +
#    geom_point(aes(col = as.factor(connected)))
#
# connected_plot
#
# coords <- coords %>%
#   mutate(connected_id = paste(cluster_id, connected))

#------------------------------------------------------------------------------

print("This isn't working - isolated points are being classified as singletons,
      which is exactly how it should work, but doesn't help us here!")

# check low prob ?? (if yes, keep old ids, but create new ids for fitting)
# check high prob ?? (if yes, keep old ids, but create new ids for fitting)
#
# stn_classify = classify_with_kknn(coords = coords,
#                                   cluster_ids = cluster_ids,
#                                   points_classify = coords %>% select(x, y),
#                                   knn_value = knn_value) %>%
#   dplyr::full_join(coords, by = c("x", "y")) %>%
#   dplyr::mutate(cluster_id = cluster_ids)
#
# stn_classify <- stn_classify %>%
#   dplyr::mutate(original_cluster_id = cluster_id) %>%
#   dplyr::mutate(cluster_id = ifelse(
#     class_id == cluster_id & prob_summary < min_class_prob,
#     NA, cluster_id)) %>%
#   dplyr::mutate(cluster_id = ifelse(
#     class_id != cluster_id & prob_summary > high_class_prob,
#     NA, cluster_id))

#------------------------------------------------------------------------------
