### ---------------------------------------------------------------------------

point_info <- data.frame(test_coords, cluster_id = hcluster_list[[20]])
names(point_info)[2:3] = c('x', 'y')

cluster_plot <-  ggplot(data = point_info) +
  geom_point(aes(x=x , y = y, col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6)))

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

all_ids <- unique(point_info$cluster_id)
convert = TRUE
frech_bool = TRUE
min_common_obs = 10
min_pairs = 10
cov_mod = "gauss"
fit_subsample = TRUE
sample_type = "percentage"
num_samples = 25
percentage = 75
# num_partitions = 3

num_ids = length(all_ids)
all_models <- vector("list", num_ids)
names(all_models) <- all_ids
for(i in 1:num_ids){

  use_id = all_ids[i]
  fit_info = point_info %>% filter(cluster_id == use_id)
  obs_data = test_max %>% select(fit_info$id)

  if(nrow(fit_info) < 10){
    all_models[[i]] = NA
    next
  }

  model_list = outer_wrapper_fitmaxstab(fit_info = fit_info,
                obs_data = obs_data, convert = convert,
                frech_bool = frech_bool, cov_mod = cov_mod,
                min_common_obs = min_common_obs, min_pairs = min_pairs,
                fit_subsample = fit_subsample,
                sample_type = sample_type,
                percentage = percentage, num_samples = num_samples)
                # num_partitions = num_partitions)

  all_models[[i]] <- model_list

}

na_entries = lapply(all_models,function(l){all(is.na(l))}) %>%
  unlist()

all_models = all_models[na_entries == FALSE]

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
ell_plot <- cluster_plot +
  # geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = all_ellipses_df, aes(x=x, y= y, group = plot_group))

ell_plot

