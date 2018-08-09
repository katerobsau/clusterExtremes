### remove for loop over model_list_by_cluster!!! (bad)

### Plot Ellipses
num_clusters = length(model_list_by_cluster)
all_ellipses <- vector("list", num_clusters)
names(all_ellipses) = names(model_list_by_cluster)
medoids = data.frame(x = NULL, y = NULL, region_id = NULL)
for(i in 1:num_clusters){

  region_id = names(model_list_by_cluster)[[i]]

  ells <- lapply(model_list_by_cluster[[i]], utils_get_ellipse)
  rep_val = lapply(ells, nrow) %>% unlist()
  ells <- do.call(rbind, ells) %>%
    as.data.frame() %>%
    mutate(sim_index = rep(1:length(model_list_by_cluster[[i]]), times = rep_val)) %>%
    mutate(region_id = region_id)

  # shift ellipse by the medoiod
  print("MEDOID HACK!!")
  region_info <- point_info %>%
    filter(cluster_id == region_id)
  median_x = region_info %>% select(x) %>% as.matrix() %>% median()
  median_y = region_info %>% select(y) %>% as.matrix() %>% median()
  medoids <- rbind(medoids, data.frame(x = median_x,
                                       y = median_y,
                                       region_id = region_id))
  ells$x = ells$x + median_x
  ells$y = ells$y + median_y

  all_ellipses[[i]] <- ells

}

# combine into a single data frame of the ellipses
all_ellipses_df <- do.call(rbind, all_ellipses) %>%
  mutate(plot_group = paste(region_id, sim_index, sep = "_"))

### ------------------------------------------------------------


