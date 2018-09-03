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
#' @param num_stns minimum number of stations needed for fitting of the smith model (default = 10)
#' @param min_common_obs minimum number of common observation for get_pair_weights()
#' @param min_pairs minimum number of pairs recommonded for fitting
#' @param ratio_threshold ratio of the elliptical axes must exceed this,
#'  or convergence is supsected to have failed (default is 0.1, for 1:10)
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
#'
fit_smith_model1 <- function(data, knn_info, medoid_indexes, use_id,
                            grid, min_class_prob,
                            min_stns = 10,
                            min_common_obs, min_pairs = choose(10,2),
                            ratio_threshold = 0.1){

  id_check = all(knn_info$cluster_id %in% knn_info$knn_id) &
    all(knn_info$knn_id %in% knn_info$cluster_id)
  if(id_check == FALSE) stop("Error: cluster_id and knn_id does not match")

  id1_check = all(knn_info$knn_id %in% grid$knn_id) &
    all(grid$knn_id %in% knn_info$knn_id)
  if(id1_check == FALSE) stop("Error: grid_id and knn_id does not match")

  range_check = all(max(grid$x) >= knn_info$x) &
    all(min(grid$x) <= knn_info$x) &
    all(max(grid$y) >= knn_info$y) &
    all(min(grid$y) <= knn_info$y)

  if(range_check == FALSE) warning("Warning: grid does not cover range of station data")

  if(min_class_prob > 1 | min_class_prob < 0)
    stop("Error: incorrect value for min_class_prob specified")

  if(ncol(data) != nrow(knn_info))
    stop("Error: There data and knn_info have incompatible dimensions")

  # create a new class_id that is binary
  fit_info <- knn_info %>%
    mutate(class_id = (knn_id == use_id))
  grid <- grid %>%
    mutate(class_id = (knn_id == use_id))

  # check the region is connected
  connection_check = check_clusters_connected(
    coord = fit_info %>% select(x, y, class_id),
    medoid = medoid_indexes[use_id],
    grid = grid)

  # get possible stations for fitting
  possible_stns = which(connection_check == TRUE &
                          fit_info$prob > min_class_prob) %>%
    as.numeric()

  if(length(possible_stns) < min_stns){
    warning("Warning: too few stations for fitting")
    return(NA)
  }

  # -----------------------------------------------------------------------------

  data_fit = data[ , possible_stns] %>%
    as.matrix()

  coord_fit = fit_info[possible_stns, ] %>%
    select(x,y) %>%
    as.matrix()

  pair_weights = get_pair_weights(data_fit, min_common_obs)
  if(sum(pair_weights) < min_pairs){
    warning("Warning: too few common pairs for fitting, reduce sample size")
  }

  fitM <- tryCatch({
      fitmaxstab(data = data_fit,
                 coord = coord_fit,
                 marg.cov = NULL,
                 cov.mod = "gauss",
                 iso = FALSE,
                 weights = pair_weights,
                 fit.marge = FALSE);
      # fitmaxstab(data = data_fit,
      #           coord = coord_fit,
      #           loc.form <- loc ~ 1,
      #           scale.form <- scale ~ 1,
      #           shape.form <- shape ~ 1,
      #           marg.cov = NULL,
      #           cov.mod = "gauss",
      #           iso = FALSE,
      #           weights = pair_weights);
    }, warning = function(w){
      # cat("WARNING :",conditionMessage(e), "\n");
      return(NA)
    }, error = function(e){
      # cat("ERROR :",conditionMessage(e), "\n");
      return(NA)
    })

  # # caluclate ratio of elliptical curves
  # ratio_values = utils_check_cov_ratio(fitM)
  # if(ratio_values < ratio_threshold)
  #   fitM = NA

  if(save_output == TRUE){
    if(is.null(reference_id)){ reference_id = use_id }
    file_name = paste(output_dir, "maxstable_region_", reference_id, ".rds", sep = "")
    saveRDS(model_list, file_name)
    return(NULL)
  }else{
    return(fitM)
  }

}
