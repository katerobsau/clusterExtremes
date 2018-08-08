### Get regionalisation

grid_domain = get_grid_for_classification(point_info %>% select(x, y),
                                          grid_space = grid_space,
                                          min_dist = min_dist,
                                          restrict_aus = restrict_aus)

grid_classify = classify_with_kknn(
  points_train = point_info %>% select(x,y,cluster_id),
  points_classify = grid_domain %>% select(x,y),
  knn_value = 20)

# Create a plot
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
kknn_plot <- ggplot() +
  geom_raster(data = grid_classify,
             aes(x=x, y=y,
                 fill = as.factor(class_id),
                 alpha = prob_summary)
                 ) +
  # geom_point(data = point_info, aes(x=x, y=y,
  #                        col = as.factor(cluster_id)), shape = 20) +
  coord_fixed() +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(grid_classify$x) + c(-min_dist, min_dist)) +
  scale_y_continuous(limits = range(grid_classify$y) + c(-min_dist, min_dist)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none")

kknn_plot


