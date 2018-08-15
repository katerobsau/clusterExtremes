print("HACKED IN CURRENTLY")
temp_tree = cutree(full_tree, h = 0.12)

# point_info <- data.frame(test_coords, cluster_id = hcluster_list[[18]])
point_info <- data.frame(coords, cluster_id = temp_tree)
names(point_info)[2:3] = c('x', 'y')

mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
cluster_plot <-  ggplot(data = point_info) +
  geom_point(aes(x=x , y = y, col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6)), size = 1, alpha = 0.75) +
  coord_fixed() +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(point_info$x) + c(-min_dist, min_dist)) +
  scale_y_continuous(limits = range(point_info$y) + c(-min_dist, min_dist)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none")

cluster_plot
