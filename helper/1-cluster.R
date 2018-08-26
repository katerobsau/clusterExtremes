## Clustering

# Get the Dendgrogam for average linkage
full_tree = hclust(clust_dist, method = linkage_method)
cut_heights = data.frame(h = full_tree$height,
                         k = (nrow(coords)-1):1)

# Get the Complete tree, gives an indication
# of the threshold for the number of clusters
full_complete_tree = hclust(clust_dist, method = "complete")
complete_max_k = nrow(coords) - max(which(full_complete_tree$height < 1/6))
print(paste("max_k from complete is", complete_max_k))

# Restrict clustering to a minimum cut height
min_cut_tree = cutree(full_tree, h = min_cut_height)
max_k = length(unique(min_cut_tree))
print(paste("max_k from from min_cut_height is", max_k))

# Cut the tree at all possible k values
k_values = seq(2, max_k)
subtree_info <- cut_heights %>%
  filter(k %in% k_values)
hclusters_mat = sapply(k_values, cutree, tree = full_tree)
hclusters = hclusters_mat %>% t() %>% as.data.frame()
names(hclusters) = names(max_data)

# Combine clustering, cut heights and number of clusters
hclusters = hclusters %>%
  mutate(k = k_values) %>%
  left_join(subtree_info, by = "k")

# Don't bother if clustering if there isn't a change in the larger cluster sizes
num_clusters = apply(hclusters  %>% select(-k,-h), 1, function(row, min_cluster_size){
  num_clusters = sum(table(row) > min_cluster_size)
}, min_cluster_size = 10)
temp_df <- data.frame(k = hclusters$k, h = hclusters$h, num_clusters = num_clusters) %>%
  mutate(lag = num_clusters - lag(num_clusters)) %>%
  filter(lag > 0 | is.na(lag))
hclusters <- hclusters %>%
  filter(k %in% temp_df$k)
