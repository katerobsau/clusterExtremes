### Fitting
print("Warning: passing point_info in at the moment")
print("Need to couple this piece of code, with a proper subset")

all_ids <- 1:num_k
model_list_by_cluster <- vector("list", num_k)
names(model_list_by_cluster) <- all_ids

cluster_ids <- hclusters %>%
  filter(k == num_k) %>%
  select(-h, -k)

cluster_ids <- data.frame(id = names(cluster_ids),
                          cluster_id = cluster_ids[1,] %>% as.numeric())

point_info <- coords %>%
  full_join(cluster_ids, by = "id")

for(i in 1:num_k){

  print(paste("Cluster", i, "of", num_k))

  use_id = all_ids[i]
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

  model_list = outer_wrapper_fitmaxstab(fit_info = fit_info,
                                        obs_data = obs_data,
                                        convert = convert,
                                        frech_bool = frech_bool,
                                        cov_mod = cov_mod,
                                        min_common_obs = min_common_obs,
                                        min_pairs = min_pairs,
                                        sample_bool = sample_bool,
                                        fit_sample = fit_sample)

  model_list_by_cluster[[i]] <- model_list

}

# Remove the clusters with too few observations for fitting
na_entries = lapply(model_list_by_cluster,function(l){all(is.na(l))}) %>%
  unlist()

model_list_by_cluster = model_list_by_cluster[na_entries == FALSE]
