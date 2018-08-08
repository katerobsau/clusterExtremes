library(extRemes)
library(RaingleExtremes)
library(SpatialExtremes)
library(dplyr)
library(SDMTools)
library(oz)
library(dbscan)
library(class)
library(ggplot2)
library(psych)

use_id = 2
grid_space = 0.25
min_dist_to_stn = 0.5
k_nbrs = 20

# sampling
num_samps = 50
# want_samps = 25
samp_size = 30

# possible stns
min_class_prob = 0.5
ellipse_alpha = 0.1

# weights
min_common_obs = 10
min_num_stns = 10

#ellipses
ratio_threshold = 0.1

#refit
max_iter = 3

# save
output_dir = "helper/"

# read in cluster data
load("Data/test_Data.RData")
load("Data/medoid_indexes.RData")

# read in the maximum data
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"
data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = "")) %>%
  select(knn_info$id)

# covert data from GEV to Frechet for fitting
frech_data = apply(data, 2, gev2frech_with_tryCatch)

# get the grid
grid = get_classification_grid(coord = fit_info %>% select(x,y),
                               grid_space = grid_space, min_dist = min_dist_to_stn)
names(grid) = c("x", "y")

# -----------------------------------------------------------------------------

for(use_id in 1:length(medoid_indexes)){

# fit_smith_model <- function(knn_info, medoid_indexes, use_id,
#          k_nbrs, min_class_prob, num_samps, samp_size,
#          min_common_obs,
#          ratio_threshold, ellipse_alpha,
#          max_iter,
#          reference_id){

  # create a new class_id that is binary
  fit_info <- knn_info %>%
    mutate(class_id = (knn_id == use_id))

  # classify the grid points
  stn_train = fit_info %>% select(x, y)
  stn_test = grid %>% select(x, y)
  stn_label = fit_info$cluster_id
  knn_stations = knn(train = stn_train, test = stn_test,
                   cl = stn_label, k = k_nbrs,
                   prob = TRUE)
  grid <- grid %>%
    mutate(knn_id = knn_stations, prob = attr(knn_stations, "prob")) %>%
    mutate(class_id = knn_id == use_id)

  # check the region is connected
  connection_check = check_clusters_connected(
                         coord = fit_info %>% select(x,y,class_id),
                         medoid = medoid_indexes[use_id],
                         grid = grid)

  # # check plot
  # ggplot(fit_info, aes(x=x, y =y)) + geom_point(aes(col = as.factor(connection_check)))

  # -----------------------------------------------------------------------------

  # get possible stations for fitting
  possible_stns = which(connection_check == TRUE &
                        fit_info$prob > min_class_prob &
                        fit_info$cluster_id == use_id) %>%
    as.numeric()

  print("NEED TO ADD ERROR HANDLING")

  # -----------------------------------------------------------------------------

  model_list = vector("list", num_samps)
  for(i in 1:num_samps){

    set.seed(i)
    sample_stns = sample(possible_stns, samp_size, replace = FALSE)

    # data_fit = frech_data[ , sample_stns] %>%
    #   as.matrix()
    data_fit = data[ , sample_stns] %>%
      as.matrix()

    coord_fit = fit_info[sample_stns, ] %>%
      select(x,y) %>%
    as.matrix()

    pair_weights = get_pair_weights(data_fit, min_common_obs)

    fitM <- tryCatch({
      # fitmaxstab(data = data_fit,
      #            coord = coord_fit,
      #            marg.cov = NULL,
      #            cov.mod = "gauss",
      #            iso = FALSE,
      #            weights = pair_weights,
      #            fit.marge = FALSE);
      fitmaxstab(data = data_fit,
               coord = coord_fit,
               loc.form <- loc ~ 1,
               scale.form <- scale ~ 1,
               shape.form <- shape ~ 1,
               marg.cov = NULL,
               cov.mod = "gauss",
               iso = FALSE,
               weights = pair_weights);
    }, warning = function(w){
      # cat("WARNING :",conditionMessage(e), "\n");
      return(NA)
    }, error = function(e){
      # cat("ERROR :",conditionMessage(e), "\n");
      return(NA)
    })

    model_list[[i]] = fitM

  }

# -----------------------------------------------------------------------------

  # caluclate ratio of elliptical curves
  ratio_values = lapply(model_list, utils_check_cov_ratio) %>%
    unlist()

  rerun_i = which(ratio_values < ratio_threshold)
  model_list[rerun_i] = NA

# -----------------------------------------------------------------------------

# identify any suspect ellipses that do not fail ratio
  check_ellipses <- utils_flag_ellipses(model_list, alpha = ellipse_alpha)
  rerun_i = check_ellipses$sim_index
  model_list[rerun_i] = NA

# -----------------------------------------------------------------------------

  # get start values
  cov11 = lapply(model_list, utils_get_par_fun, i = 1) %>% unlist()
  cov12 = lapply(model_list, utils_get_par_fun, i = 2) %>% unlist()
  cov22 = lapply(model_list, utils_get_par_fun, i = 3) %>% unlist()
  locCoeff1 = lapply(model_list, utils_get_par_fun, i = 4) %>% unlist()
  scaleCoeff1 = lapply(model_list, utils_get_par_fun, i = 5) %>% unlist()
  shapeCoeff1 = lapply(model_list, utils_get_par_fun, i = 6) %>% unlist()
  start_cov11 = median(cov11, na.rm = TRUE)
  start_cov12 = median(cov12, na.rm = TRUE)
  start_cov22 = median(cov22, na.rm = TRUE)
  start_loc = median(locCoeff1, na.rm = TRUE)
  start_scale = median(scaleCoeff1, na.rm = TRUE)
  start_shape = median(shapeCoeff1, na.rm = TRUE)

  start_list = list(cov11 = start_cov11, cov12 = start_cov12, cov22 = start_cov22,
                  locCoeff1 = start_loc, scaleCoeff1 = start_scale, shapeCoeff1 = start_shape)

# -----------------------------------------------------------------------------

  convergence_issue = lapply(model_list, function(l){all(is.na(l))}) %>% unlist() %>% sum()
  print(paste(convergence_issue, "samples with suspect convergence"))

  iter = 1
  old_convergence_number = convergence_issue

  while(iter < max_iter){

    for(i in 1:num_samps){

      if(!any(is.na(model_list[[i]])) == TRUE) next

      set.seed(i)
      sample_stns = sample(possible_stns, samp_size, replace = FALSE)

      # assume constant parameters across the space
      # less uncertainty that transforming to frechet
      # data_fit = frech_data[ , sample_stns] %>%
      #   as.matrix()
      data_fit = data[ , sample_stns] %>%
        as.matrix()

      coord_fit = fit_info[sample_stns, ] %>%
        select(x,y) %>%
        as.matrix()

      pair_weights = get_pair_weights(data_fit, min_common_obs)

      fitM <- tryCatch({
        # fitmaxstab(data = data_fit,
        #            coord = coord_fit,
        #            marg.cov = NULL,
        #            cov.mod = "gauss",
        #            iso = FALSE,
        #            weights = pair_weights,
        #            fit.marge = FALSE);
        fitmaxstab(data = data_fit,
                   coord = coord_fit,
                   loc.form <- loc ~ 1,
                   scale.form <- scale ~ 1,
                   shape.form <- shape ~ 1,
                   marg.cov = NULL,
                   cov.mod = "gauss",
                   iso = FALSE,
                   weights = pair_weights,
                   start = start_list);
      }, warning = function(w){
        # cat("WARNING :",conditionMessage(e), "\n");
        return(NA)
      }, error = function(e){
        # cat("ERROR :",conditionMessage(e), "\n");
        return(NA)
      })

      model_list[[i]] = fitM

    }

    # -----------------------------------------------------------------------------

    # caluclate ratio of elliptical curves
    ratio_values = lapply(model_list, utils_check_cov_ratio) %>%
      unlist()

    rerun_i = which(ratio_values < ratio_threshold)
    model_list[rerun_i] = NA

    # -----------------------------------------------------------------------------

    # Don't want this step in the repeat iterations

    # # identify any suspect ellipses that do not fail ratio
    # check_ellipses <- utils_flag_ellipses(model_list, alpha = 0.1)
    # rerun_i = check_ellipses$sim_index
    # model_list[rerun_i] = NA

    # -----------------------------------------------------------------------------

    # get start values
    cov11 = lapply(model_list, utils_get_par_fun, i = 1) %>% unlist()
    cov12 = lapply(model_list, utils_get_par_fun, i = 2) %>% unlist()
    cov22 = lapply(model_list, utils_get_par_fun, i = 3) %>% unlist()
    locCoeff1 = lapply(model_list, utils_get_par_fun, i = 4) %>% unlist()
    scaleCoeff1 = lapply(model_list, utils_get_par_fun, i = 5) %>% unlist()
    shapeCoeff1 = lapply(model_list, utils_get_par_fun, i = 6) %>% unlist()
    start_cov11 = median(cov11, na.rm = TRUE)
    start_cov12 = median(cov12, na.rm = TRUE)
    start_cov22 = median(cov22, na.rm = TRUE)
    start_loc = median(locCoeff1, na.rm = TRUE)
    start_scale = median(scaleCoeff1, na.rm = TRUE)
    start_shape = median(shapeCoeff1, na.rm = TRUE)

    start_list = list(cov11 = start_cov11, cov12 = start_cov12, cov22 = start_cov22,
                      locCoeff1 = start_loc, scaleCoeff1 = start_scale, shapeCoeff1 = start_shape)


    convergence_issue = lapply(model_list, function(l){all(is.na(l))}) %>% unlist() %>% sum()
    print(paste(convergence_issue, "samples with suspect convergence"))

    if(old_convergence_number == convergence_issue) iter = max_iter

    old_convergence_number = convergence_issue

    iter = iter + 1

  }

  file_name = paste(output_dir, "maxstable_region_", use_id, ".rds", sep = "")
  saveRDS(model_list, file_name)

}

#
# # -----------------------------------------------------------------------------
#
# # get ellipses
# ellipse_list = lapply(model_list, utils_get_ellipse)
# ellipse_df <- do.call(rbind, ellipse_list)
#
# # combine all simulation informaiton for plotting
# num_rows = lapply(ellipse_list, nrow) %>% unlist()
# sim_index = rep(1:length(ellipse_list), times = num_rows)
# temp <- data.frame(sim_index = 1:100, ratio = ratio_values)
# ellipse_df <- ellipse_df %>%
#   as.data.frame() %>%
#   mutate(sim_index) %>%
#   left_join(temp, by = "sim_index")
#
# # plot elliptical curves
# ell_plot <- ggplot(data = ellipse_df) +
#   geom_path(aes(x=x, y =y, group = sim_index, col = ratio < 0.1), alpha = 0.25) +
#   theme_bw()
# ell_plot
#
# # -----------------------------------------------------------------------------
#
# check_ellipses <- left_join(check_ellipses, ellipse_df, by = "sim_index")
#
# # plot elliptical curves
# ell_plot <- ggplot() +
#   geom_path(data = ellipse_df, aes(x=x, y =y, group = sim_index), alpha = 0.25) +
#   geom_path(data = check_ellipses, aes(x=x, y =y,
#                                        group = sim_index, col = as.factor(sim_index))) +
#   theme_bw()
# ell_plot
#
# library(plotly)
# ggplotly(ell_plot)
