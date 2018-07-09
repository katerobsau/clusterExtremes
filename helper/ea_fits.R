knn_info <- knn_info %>%
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  mutate(cluster_id = Fmadogram) %>%
  mutate(knn_id = knn)

# data = test_max
frech_data = apply(test_max, 2, gev2frech_with_tryCatch)
data = frech_data

grid = knn_wa_grid %>%
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  mutate(knn_id = knn)

medoid_indexes = fmado_clusters$medoids
len = length(medoid_indexes)
all_models <- vector("list", len)
all_ellipses = NULL
for(i in 1:len){

  use_id = i
  model_list <- tryCatch({fit_smith_model(data = data, knn_info = knn_info,
                             medoid_indexes = medoid_indexes, use_id = i,
                             grid, min_class_prob = 0.5, high_class_prob = 0.75,
                             num_samps = 20, samp_size = 40,
                             min_common_obs = 10, min_pairs = choose(10,2),
                             ratio_threshold = 0.1, ellipse_alpha = 0.1,
                             max_iter = 3,
                             save_output = FALSE, output_dir = NULL,
                             reference_id = NULL)},
                         error = function(e){return(NULL)})

  if(is.null(model_list)) next

  all_models[[i]] = model_list

  ellipse_df <- get_ellipse_from_smith_model_list(model_list,
                                                  medoid = knn_info[medoid_indexes[use_id], ]) %>%
    mutate(region_id = use_id)

  all_ellipses = rbind(all_ellipses, ellipse_df)
}

all_ellipses <- all_ellipses %>%
  mutate(plot_index = paste(region_id, sim_index))

# plot result
ell_plot <- knn_wa_plot +
  geom_path(data = all_ellipses, aes(x=x, y =y, group = plot_index), alpha = 0.25) +
  theme_bw()
  ell_plot

ell_plot
