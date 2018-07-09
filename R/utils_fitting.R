# # Function gets pair weights for max-stable model
# # If there are insuffienct common observations, weight is set to zero
# utils_get_pair_weights <- function(data, min_common_obs = 10){
#
#   num_stns = ncol(data)
#
#   common_count <- get_num_common_obs(data)
#
#   pairs = expand.grid(j = 1:num_stns, i = 1:num_stns) %>%
#     as.data.frame() %>%
#     filter(i < j) %>%
#     mutate(pair_weights = ifelse(i == 1 | j == 1, 0, 1)) %>%
#     mutate(common = common_count) %>%
#     mutate(pair_weights = ifelse(common < min_common_obs, 0, pair_weights))
#
#   return(pairs$pair_weights)
#
# }

# This function takes a fitted maxstable model from the spatial extremes
# package with a Gaussian dependence structure
# From this model, calculate the ratio of axes for the elliptical level curve
# THe default for the level curve is 0.99
utils_check_cov_ratio <- function(maxstable_model, level = 0.99){

  if(any(is.na(maxstable_model))){
    return(NA)
  }

  param = maxstable_model$fitted.values
  cov11 = param[1]
  cov12 = param[2]
  cov22 = param[3]
  cov_mat = matrix(c(cov11, cov12, cov12, cov22),
                     nrow = 2, byrow = TRUE)
  eigen_vals = eigen(cov_mat)$values
  r_values = 2*sqrt(eigen_vals*qchisq(level, 2))
  radius_ratio = r_values[2]/r_values[1]

  return(radius_ratio)
}

# This function takes a fitted maxstable model from the spatial extremes
# package with a Gaussian dependence structure
# From the covariance matrix a dataframe for the elliptical level curve
# is returned
# Default level curve is 0.99
utils_get_ellipse <- function(maxstable_model, level = 0.99){

  if(any(is.na(maxstable_model))){

    segments = 51
    ellipse = matrix(NA, 51, 2)
    colnames(ellipse) <- c("x", "y")
    return(ellipse)

  }

  cov11 = maxstable_model$fitted.values[1]
  cov12 = maxstable_model$fitted.values[2]
  cov22 = maxstable_model$fitted.values[3]
  cov_mat = matrix(c(cov11, cov12, cov12, cov22),
                 nrow = 2, byrow = TRUE)

  segments = 51
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))
  shape = cov_mat
  Q <- chol(shape, pivot = TRUE)
  order <- order(attr(Q, "pivot"))
  center = c(0,0)
  radius = sqrt(-2*log(1 - level))
  ellipse <- t(center + radius * t(unit.circle %*% Q[, order]))
  colnames(ellipse) <- c("x", "y")

  return(ellipse)
}

# This function gets the index i paramter from the max-stable model
utils_get_par_fun <- function(l, i){
  if(any(is.na(l))) return(NA)
     l$param[i]
}

# This function helps identify any suspect ellipses
utils_flag_ellipses <- function(maxstable_model_list, alpha = 0.05){

  # get parameters from list
  cov11 = lapply(maxstable_model_list, utils_get_par_fun, i = 1) %>% unlist()
  cov12 = lapply(maxstable_model_list, utils_get_par_fun, i = 2) %>% unlist()
  cov22 = lapply(maxstable_model_list, utils_get_par_fun, i = 3) %>% unlist()
  param_info <- data.frame(sim_index = 1:length(maxstable_model_list),
                           cov11, cov12, cov22)

  # check ellipses
  check_ellipses <- param_info %>%
    filter(cov11 < quantile(param_info$cov11, alpha/2, na.rm  = T) |
             cov11 > quantile(param_info$cov11, 1 - alpha/2, na.rm  = T) |
             cov22 < quantile(param_info$cov22, alpha/2, na.rm  = T) |
             cov22 > quantile(param_info$cov22, 1- alpha/2, na.rm  = T) |
             cov12 < quantile(param_info$cov12, alpha/2, na.rm  = T) |
             cov12 > quantile(param_info$cov12, 1- alpha/2, na.rm  = T))

  return(check_ellipses)

}

# utils_wrapper_fit <- function(filter_id,
#                               all_cluster_info,
#                               fmado_data,
#                               min_common_years){
#
#   print("WARNING: HARD CODED RULE HERE")
#   cluster_info <- all_cluster_info %>%
#     filter(knn == filter_id & prob > 0.75)
#
#   print("NAUGHTY NAUGHTY HACK")
#   if(nrow(cluster_info) > 30){
#     set.seed(1)
#     cluster_info = cluster_info[sample(1:nrow(cluster_info), 30), ]
#   }
#
#   print("WARNING: HARD CODED RULE HERE")
#   if(nrow(cluster_info) < 10){return(NA)}
#
#   coord = cluster_info %>%
#     select(longitude, latitude) %>%
#     as.matrix()
#
#   data = fmado_data %>%
#     select(cluster_info$id) %>%
#     as.matrix()
#
#   print("WARNING: HARD CODED RULE HERE")
#   pair_weights = get_pair_weights(data, min_common_years)
#   if(sum(pair_weights) < choose(10,2)){return(NA)}
#
#   print("WARNING: HARD CODED PARAMETERS HERE")
#   fitM <- tryCatch({
#     fitmaxstab(data = data,
#                coord = coord,
#                loc.form <- loc ~ 1,
#                scale.form <- scale ~ 1,
#                shape.form <- shape ~ 1,
#                marg.cov = NULL,
#                cov.mod = "gauss",
#                iso = FALSE,
#                weights = pair_weights);
#   }, warning = function(w){
#     # cat("WARNING :",conditionMessage(e), "\n");
#     return(NA)
#   }, error = function(e){
#     # cat("ERROR :",conditionMessage(e), "\n");
#     return(NA)
#   })
#
#   return(fitM)
#
# }
#
# utils_wrapper_fit_repeat <- function(filter_id,
#                               all_cluster_info,
#                               fmado_data,
#                               min_common_years,
#                               num_samps,
#                               prop = 2/3,
#                               min_num_stns = 30){
#
#   print("WARNING: HARD CODED RULE HERE")
#   filter_cluster_info <- all_cluster_info %>%
#     filter(knn == filter_id & prob > 0.5)
#
#   print("WARNING: HARD CODED RULE HERE")
#   if(nrow(filter_cluster_info) < min_num_stns){return(NA)}
#
#   print("NAUGHTY NAUGHTY HACK")
#   num_stns = floor(prop*nrow(filter_cluster_info))
#   sample_stns = replicate(num_samps,
#                           sample(1:nrow(filter_cluster_info),
#                                  num_stns))
#
#   fit_list = vector("list", num_samps)
#   for(i in 1:num_samps){
#
#     cluster_info = filter_cluster_info[sample_stns[,i], ]
#
#     coord = cluster_info %>%
#       select(longitude, latitude) %>%
#       as.matrix()
#
#     data = fmado_data %>%
#       select(cluster_info$id) %>%
#       as.matrix()
#
#     print("WARNING: HARD CODED RULE HERE")
#     pair_weights = utils_get_pair_weights(data, min_common_years)
#     if(sum(pair_weights) < choose(10, 2)){return(NA)}
#
#     print("WARNING: HARD CODED PARAMETERS HERE")
#     fitM <- tryCatch({
#       fitmaxstab(data = data,
#                coord = coord,
#                loc.form <- loc ~ 1,
#                scale.form <- scale ~ 1,
#                shape.form <- shape ~ 1,
#                marg.cov = NULL,
#                cov.mod = "gauss",
#                iso = FALSE,
#                weights = pair_weights);
#     }, warning = function(w){
#       # cat("WARNING :",conditionMessage(e), "\n");
#       return(NA)
#     }, error = function(e){
#       # cat("ERROR :",conditionMessage(e), "\n");
#       return(NA)
#     })
#
#     if(length(fitM) == 1){
#       print("Bad luck")
#     }else{
#       print(fitM$fitted.values)
#     }
#     fit_list[[i]] = fitM
#
#   }
#
#   return(fit_list)
#
# }
