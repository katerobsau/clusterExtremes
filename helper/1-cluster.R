## Clustering
hclusters = hclust(clust_dist, method = linkage_method)
tree = cutree(hclusters, h = min_cut_height)
max_k = length(unique(tree))
k_values = unique(floor(seq(2, max_k)))
len = length(k_values)
hcluster_list = vector("list", len)
for(i in 1:len){
  hcluster_list[[i]] = cutree(hclusters, k = k_values[i])
}


print("HACKED IN CURRENTLY")
temp_tree = cutree(hclusters, h = 0.135)

# point_info <- data.frame(test_coords, cluster_id = hcluster_list[[18]])
point_info <- data.frame(test_coords, cluster_id = temp_tree)
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
