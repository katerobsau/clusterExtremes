### Check Fits
model_list <- all_models[[3]]

# caluclate ratio of elliptical curves
ratio_values = lapply(model_list, utils_check_cov_ratio) %>%
  unlist()

# set models to NA that need to be rerun
rerun_ratio = which(ratio_values < ratio_threshold)
model_list[rerun_ratio] = NA

# identify any suspect ellipses that have large parameter values
check_ellipses <- utils_flag_ellipses(model_list, alpha = ellipse_alpha)
rerun_param = check_ellipses$sim_index
model_list[rerun_param] = NA

