### Repeat fits

len = length(model_list_by_cluster)
cluster_names = names(model_list_by_cluster)

for(i in 1:len){

  model_list = model_list_by_cluster[[i]]

  if(all(is.na(model_list))) next

  use_id = cluster_names[i]

  fit_info = point_info %>% filter(cluster_id == use_id)
  obs_data = max_data %>% select(fit_info$id)

  if(nrow(fit_info) < min_stns_for_fitting){
    model_list_by_cluster[[i]] = NA
    next
  }

  set.seed(seed_value)
  fit_sample = switch(sample_type,
                      "random" = get_samples(n = nrow(fit_info),
                                             sample_type = sample_type,
                                             num_samples = num_samples,
                                             samp_size = samp_size),
                      "parition" = get_samples(n = nrow(fit_info),
                                               sample_type = sample_type,
                                               num_partitions = num_partitions),
                      "percentage" = get_samples(n = nrow(fit_info),
                                                 sample_type = sample_type,
                                                 num_samples = num_samples,
                                                 percentage = percentage))

  start_list = get_start_list(model_list)

  model_list = outer_wrapper_fitmaxstab(fit_info = fit_info,
                                        obs_data = obs_data,
                                        convert = convert,
                                        frech_bool = frech_bool,
                                        cov_mod = cov_mod,
                                        min_common_obs = min_common_obs,
                                        min_pairs = min_pairs,
                                        sample_bool = sample_bool,
                                        fit_sample = fit_sample,
                                        start = start_list)

  model_list_by_cluster[[i]] <- model_list

}

# Remove the clusters with too few observations for fitting
na_entries = lapply(model_list_by_cluster,function(l){all(is.na(l))}) %>%
  unlist()

model_list_by_cluster = model_list_by_cluster[na_entries == FALSE]

