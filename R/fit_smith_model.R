#' Fit smith max-stable model to bootstrap samples
#'
#' We have classified a gridded region based on a clustering outpput. The classification
#' step was necessary to identify region boundaries. This function fits a max-stable model
#' to obtain the dependence structure for the region. We sample from suitable stations to
#' obtain multiple realisations of the dependence structure.
#'
#' Of note, under the classification it is possible to have disjoint regions
#' classified similarly. For sampling, we only consider stations that are classified
#' similiarly and connected to the medoid identified in clustering. We also only
#' consider stations that were assigned to that region with a minimum probability.
#' After the initial fitting, we consider two basic diagnostics to check if the
#' optimisation converged. We compare the ratio of the two axes of the ellipses.
#' If the ratio is too small, we initial the start values and repeat the fitting.
#' We also repeat the fitting if the fitted parameter is 'too far'
#' from what we consider 'normal'.
#'
#' @param data a data frame with columns labelled by the station id, and
#' rows corresponding to block maximum observation at that station
#' @param knn_info data frame with columns x, y, cluster_id, knn_id and prob.
#' The set of cluster_id and knn_id must be the same. The column prob is
#' the probability of assigning that station to under the classification.
#' @param medoid_indexes index of the medoid in the knn_info data frame.
#' should update this so it is a data frame for reference not an index.
#' @param use_id the cluster_id/knn_id of the region we are considering
#' @param grid a data frame that describes a grid, columns of x, y and knn_id.
#' Must cover the region of interest
#' @param min_class_prob for the station to be considered in the fitting it must
#' have been assigned under the classification with this minimum probability
#' @param num_samps number of times to repeat the fitting of the smith model
#' @param samp_size number of stations used in fitting
#' @param ratio_threshold ratio of the elliptical axes must exceed this,
#'  or convergence is supsected to have failed
#' @param ellipse_alpha if paramter values are outside this alpha level,
#' we chose to repeat fitting with better initialisation
#' @param max_iter (integer) maximum number of times to repeat fitting with improved
#' initialisation
#' @param save_output (boolean) default is FALSE, save out the model fits
#' @param output_dir directory to save output (default is NULL)
#' @param reference_id if reference_id is NULL defaults to user id, is an index to
#' use in the filename for referencing the output
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
fit_smith_model <- function(data, knn_info, medoid_indexes, use_id,
                            grid, min_class_prob,
                            num_samps, samp_size, min_common_obs,
                            ratio_threshold = 0.1, ellipse_alpha,
                            max_iter,
                            save_output = FALSE, output_dir = NULL,
                            reference_id = NULL){

  id_check = all(knn_info$cluster_id %in% knn_info$knn_id) &
    all(knn_info$knn_id %in% knn_info$cluster_id)
  if(id_check == FALSE) stop("Error: cluster_id and knn_id does not match")

  id1_check = all(knn_info$knn_id %in% grid$knn_id) &
    all(grid$knn_id %in% knn_info$knn_id)
  if(id1_check == FALSE) stop("Error: grid_id and knn_id does not match")

  range_check = max(grid$x) >= knn_info$x & min(grid$x) <= knn_info$x &
    max(grid$y) >= knn_info$y & min(grid$y) <= knn_info$y
  if(range_check == FALSE) stop("Error: grid does not cover range of station data")

  if(is.integer(max_iter)) stop("Error: max_iter must be an integer")

  # create a new class_id that is binary
  fit_info <- knn_info %>%
    mutate(class_id = (knn_id == use_id))
  grid <- grid %>%
    mutate(class_id = knn_id == use_id)

  # check the region is connected
  connection_check = check_clusters_connected(
    coord = fit_info %>% select(x, y, class_id),
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

  if(nrow(possible_stns) < samp_size + 1){
    stop("Error: too few stations for sampling, reduce samp_size")
  }

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
    if(sum(pair_weights) < choose(10,2)){
      warning("Warning: too few common pairs for fitting, reduce sample size")
    }

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

  if(save_output == TRUE){
    if(is.null(reference_id)){ reference_id = use_id }
    file_name = paste(output_dir, "maxstable_region_", reference_id, ".rds", sep = "")
    saveRDS(model_list, file_name)
    return(NULL)
  }else{
    return(model_list)
  }

}
