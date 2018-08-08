### remove for loop overa all_models!!! (bad)

### Fitting

print("Warning: passing point_info in at the moment")
print("Need to couple this piece of code, with a proper subset")
test_max <- max_data

all_ids <- unique(point_info$cluster_id)
num_ids = length(all_ids)
all_models <- vector("list", num_ids)
names(all_models) <- all_ids
for(i in 1:num_ids){

  use_id = all_ids[i]
  fit_info = point_info %>% filter(cluster_id == use_id)
  obs_data = max_data %>% select(fit_info$id)

  if(nrow(fit_info) < min_stns_for_fitting){
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
