### remove for loop overa all_models!!! (bad)

### Fitting
print("Warning: passing point_info in at the moment")
print("Need to couple this piece of code, with a proper subset")

all_ids <- 1:num_k
all_models <- vector("list", num_k)
names(all_models) <- all_ids

cluster_ids <- hclusters %>%
  filter(k == num_k) %>%
  select(-h, -k)

cluster_ids <- data.frame(id = names(cluster_ids),
                          cluster_id = cluster_ids[1,] %>% as.numeric())

point_info <- coords %>%
  full_join(cluster_ids, by = "id")

for(i in 1:num_k){

  use_id = all_ids[i]
  fit_info = point_info %>% filter(cluster_id == use_id)
  obs_data = max_data %>% select(fit_info$id)

  if(nrow(fit_info) < min_stns_for_fitting){
    all_models[[i]] = NA
    next
  }

  model_list = outer_wrapper_fitmaxstab(fit_info = fit_info,
                                        obs_data = obs_data,
                                        convert = convert,
                                        frech_bool = frech_bool,
                                        cov_mod = cov_mod,
                                        min_common_obs = min_common_obs,
                                        min_pairs = min_pairs,
                                        fit_subsample = fit_subsample,
                                        sample_type = sample_type,
                                        percentage = percentage,
                                        num_samples = num_samples)

  all_models[[i]] <- model_list

}

# Remove the clusters with too few observations for fitting
na_entries = lapply(all_models,function(l){all(is.na(l))}) %>%
  unlist()

all_models = all_models[na_entries == FALSE]
