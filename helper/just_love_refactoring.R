### ---------------------------------------------------------------------------

point_info <- data.frame(test_coords, cluster_id = hcluster_list[[150]])
names(point_info)[2:3] = c('x', 'y')

cluster_plot <-  ggplot(data = point_info) +
  geom_point(aes(x=x , y = y, col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6)), size = 1, alpha = 0.75) +
  theme(legend.position = "none")

cluster_plot

# library(plotly)
# ggplotly(cluster_plot)

### ---------------------------------------------------------------------------

# use_id = 5
# var_name = "cluster_id"
#
# ### IDENTIFY CLUSTER/KNN ID FOR FITTING
# if(missing(var_name) | missing(use_id)){
#   point_info <- point_info %>%
#     dplyr::mutate(binary_clase = TRUE)
# }else{
#   point_info <- create_binary_class(point_info = point_info,
#                                     var_name = var_name, use_id = use_id)
# }
#
# if(!any(point_info$binary_class == TRUE))
#   stop("No binary class matched the use_id provided")
#
# fit_info <- point_info %>%
#   dplyr::filter(binary_class == TRUE)

### ---------------------------------------------------------------------------


# -----------------------------------------------------------------------

num_clusters = length(all_models)
all_ellipses <- vector("list", num_clusters)
names(all_ellipses) = names(all_models)
medoids = data.frame(x = NULL, y = NULL, region_id = NULL)
for(i in 1:num_clusters){

  region_id = names(all_models)[[i]]

  ells <- lapply(all_models[[i]], utils_get_ellipse)
  rep_val = lapply(ells, nrow) %>% unlist()
  ells <- do.call(rbind, ells) %>%
    as.data.frame() %>%
    mutate(sim_index = rep(1:length(all_models[[i]]), times = rep_val)) %>%
    mutate(region_id = region_id)

  # shift ellipse by the medoiod
  print("MEDOID HACK!!")
  region_info <- point_info %>%
    filter(cluster_id == region_id)
  mean_x = region_info %>% select(x) %>% as.matrix() %>% mean()
  mean_y = region_info %>% select(y) %>% as.matrix() %>% mean()
  medoids <- rbind(medoids, data.frame(x = mean_x,
                                       y = mean_y,
                                       region_id = region_id))
  ells$x = ells$x + mean_x
  ells$y = ells$y + mean_y

  all_ellipses[[i]] <- ells

}
# combine into a single data frame of the ellipses
all_ellipses_df <- do.call(rbind, all_ellipses) %>%
  mutate(plot_group = paste(region_id, sim_index, sep = "_"))

# plot the ellipses
ell_plot <-
  ggplot() +
  # cluster_plot +
  # geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = all_ellipses_df, aes(x=x, y= y, group = plot_group), alpha = 0.05) #+
  # scale_x_continuous(limits = c(135,155)) +
  # scale_y_continuous(limits = c(-40,-20))
ell_plot

