## Clustering

full_tree = hclust(clust_dist, method = linkage_method)

min_cut_tree = cutree(full_tree, h = min_cut_height)
max_k = length(unique(min_cut_tree))

k_values = seq(2, max_k)
tree_info <-  data.frame(h = full_tree$height, k = ncol(max_data):2 -1)
subtree_info <- tree_info %>%
  filter(k %in% k_values)

hclusters_mat = sapply(k_values, cutree, tree = full_tree)
hclusters = hclusters_mat %>% t() %>% as.data.frame()
names(hclusters) = names(max_data)
hclusters = hclusters %>%
  mutate(k = k_values) %>%
  full_join(subtree_info, by= "k")

