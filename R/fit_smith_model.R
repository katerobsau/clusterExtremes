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
#' @param high_class_prob for the station to be considered in the fitting it must
#' not be different to the rest of the stations in the region, if the probability of
#' classificaiton is high, but the cluster id and classification id are different
#' we discard the station
#' @param frech_bool (default = TRUE) is the data frechet distributed, otherwise we
#' assume loc, scale and shape parameters constant
#' @param num_samps number of times to repeat the fitting of the smith model
#' @param samp_size number of stations used in fitting
#' @param min_common_obs minimum number of common observation for get_pair_weights()
#' @param min_pairs minimum number of pairs recommonded for fitting
#' @param ratio_threshold ratio of the elliptical axes must exceed this,
#'  or convergence is supsected to have failed (default is 0.1, for 1:10)
#' @param ellipse_alpha if paramter values are outside this alpha level,
#' we chose to repeat fitting with better initialisation, two-sided, therefore
#' must take values in range 0 to 0.5
#' @param max_iter (integer) maximum number of times to repeat fitting with improved
#' initialisation
#' @param save_output (boolean) default is FALSE, save out the model fits
#' @param output_dir directory to save output (default is NULL)
#' @param reference_id if reference_id is NULL defaults to user id, is an index to
#' use in the filename for referencing the output
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
#'
#' @examples
#' # generate an arbitary set of points
#' set.seed(1)
#' n.site <- 600
#' locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
#' colnames(locations) <- c("lon", "lat")
#'
#' # create a medoid
#' medoid_coords = c(3,3)
#' location = c(locations, rbind(medoid_coords))
#' medoid = 601
#'
#' # create a test clustering and classificaiton dataframe
#' # make two disjoint regions
#' # make some of the probabilities less than 0.5
#' knn_info <- data.frame(x = locations[,1], y = locations[,2]) %>%
#'  mutate(knn_id = 1) %>%
#'  mutate(cluster_id = 1) %>%
#'  mutate(prob = 1) %>%
#'  mutate(knn_id = if_else((x < 6 & y < 6) | (x > 7 & y > 7), 2, knn_id)) %>%
#'  mutate(cluster_id = if_else((x < 6 & y < 6) | (x > 7 & y > 7), 2, cluster_id)) %>%
#'  mutate(prob = if_else(x < 5.5 & y < 5.5, prob, 0.4))
#'
#' # generate some data
#' data <- rmaxstab(100, locations, cov.mod = "gauss", cov11 = 1, cov12 = 0, cov22 = 1)
#'
#' # create grid covering the coordiantes
#' grid <- get_grid_for_classification(coords = knn_info %>% select(x,y),
#'                                    grid_space = 0.25, restrict_aus = FALSE)
#' names(grid) = c("x", "y")
#'
#' # mimic the classificaiton of the knn_info
#' grid <- grid %>%
#'  mutate(knn_id = 1) %>%
#'  mutate(knn_id = if_else((x < 6 & y < 6) | (x > 7 & y > 7), 2, knn_id))
#'
#' # fit the smith model
#' model_list <- fit_smith_model(data = data, knn_info = knn_info,
#'                              medoid_indexes = c(100, 601), use_id = 2,
#'                              grid, min_class_prob = 0.5, high_class_prob = 0.75,
#'                              frech_bool = TRUE,
#'                              num_samps = 10, samp_size = 20,
#'                              min_common_obs = 0, min_pairs = 0,
#'                              ratio_threshold = 0.1, ellipse_alpha= 0.05,
#'                              max_iter = 3,
#'                              save_output = FALSE, output_dir = NULL,
#'                              reference_id = NULL)
#'
#' # visualise the result
#' medoid_coords = data.frame(x=3, y=3)
#' ellipse_df <- get_ellipse_from_smith_model_list(model_list, medoid = medoid_coords)
#'
#' # plot result
#' ell_plot <- ggplot(data = ellipse_df) +
#'  geom_path(aes(x=x, y =y, group = sim_index), alpha = 0.25) +
#'  theme_bw()
#' ell_plot
#'
fit_smith_model <- function(data, knn_info, medoid_indexes, use_id,
                            grid, min_class_prob, high_class_prob,
                            frech_bool = TRUE,
                            num_samps, samp_size,
                            min_common_obs, min_pairs = choose(10,2),
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

  range_check = all(max(grid$x) >= knn_info$x) &
    all(min(grid$x) <= knn_info$x) &
    all(max(grid$y) >= knn_info$y) &
    all(min(grid$y) <= knn_info$y)

  if(range_check == FALSE) warning("Warning: grid does not cover range of station data")

  if(is.integer(num_samps)) stop("Error: num_samps must be an integer")
  if(is.integer(samp_size)) stop("Error:samp_size must be an integer")
  if(is.integer(max_iter)) stop("Error: max_iter must be an integer")

  if(min_class_prob > 1 | min_class_prob < 0)
    stop("Error: incorrect value for min_class_prob specified")

  if(ellipse_alpha > 0.5 | ellipse_alpha < 0)
    stop("Error: incorrect value for ellipse_alpha specified")

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

  # # check plot
  # ggplot(fit_info, aes(x=x, y =y)) + geom_point(aes(col = as.factor(connection_check)))

  # -----------------------------------------------------------------------------

  # get possible stations for fitting
  possible_stns = which(connection_check == TRUE &
                          fit_info$prob > min_class_prob &
                          !((fit_info$cluster_id != fit_info$knn_id) & fit_info$prob > high_class_prob)) %>%
    as.numeric()

  if(length(possible_stns) < samp_size + 1){
    warning("Warning: too few stations for sampling, reduce samp_size")
    samp_size = length(possible_stns) - 1
  }

  # -----------------------------------------------------------------------------

  print("PARALLELISE ME")
  # cl <- parallel::makeCluster(detectCores())
  # doParallel::registerDoParallel(cl)
  # model_list <-
  #   foreach(i = 1:num_samps, .packages = c('SpatialExtremes', 'clusterExtremes',
  #                                          'dplyr')) %dopar% {
  model_list = vector("list", num_samps)
  for(i in 1:num_samps){

    set.seed(1)
    sample_stns = sample(possible_stns, samp_size, replace = FALSE)

    data_fit = data[ , sample_stns] %>%
      as.matrix()

    coord_fit = fit_info[sample_stns, ] %>%
      select(x,y) %>%
      as.matrix()

    pair_weights = get_pair_weights(data_fit, min_common_obs)
    if(sum(pair_weights) < min_pairs){
      warning("Warning: too few common pairs for fitting, reduce sample size")
    }

    fitM <- tryCatch({
      if(frech_bool == TRUE){
        fitM = fitmaxstab(data = data_fit,
                 coord = coord_fit,
                 marg.cov = NULL,
                 cov.mod = "gauss",
                 iso = FALSE,
                 weights = pair_weights,
                 fit.marge = FALSE)
        }else{
        fitM = fitmaxstab(data = data_fit,
                 coord = coord_fit,
                 loc.form <- loc ~ 1,
                 scale.form <- scale ~ 1,
                 shape.form <- shape ~ 1,
                 marg.cov = NULL,
                 cov.mod = "gauss",
                 iso = FALSE,
                 weights = pair_weights)
        }
      fitM;
    }, warning = function(w){
      cat("WARNING :",conditionMessage(w), "\n");
      return(NA)
    }, error = function(e){
      cat("ERROR :",conditionMessage(e), "\n");
      return(NA)
    })

    model_list[[i]] = fitM
    # fitM

  }
  # parallel::stopCluster(cl)
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
  start_list = get_start_list(model_list, frech_bool)

  # -----------------------------------------------------------------------------

  convergence_issue = lapply(model_list, function(l){all(is.na(l))}) %>% unlist() %>% sum()
  print(paste(convergence_issue, "samples with suspect convergence"))

  iter = 1
  old_convergence_number = convergence_issue

  while(iter < max_iter){
    print("PARALLELISE ME")
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
        if(frech_bool == TRUE){
          fitM = fitmaxstab(data = data_fit,
                            coord = coord_fit,
                            marg.cov = NULL,
                            cov.mod = "gauss",
                            iso = FALSE,
                            weights = pair_weights,
                            fit.marge = FALSE)
        }else{
          fitM = fitmaxstab(data = data_fit,
                            coord = coord_fit,
                            loc.form <- loc ~ 1,
                            scale.form <- scale ~ 1,
                            shape.form <- shape ~ 1,
                            marg.cov = NULL,
                            cov.mod = "gauss",
                            iso = FALSE,
                            weights = pair_weights)
        }
        fitM;
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
    start_list = get_start_list(model_list, frech_bool)

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
