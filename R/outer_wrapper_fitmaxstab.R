#' Fit smith max-stable model
#'
#' @param fit_info a data frame which must have columns id, x, y
#' @param obs_data a data frame with columns labelled by the station id, and
#' rows corresponding to block maximum observation at that station,
#' default assumption is this data is standard frechet
#' @param convert boolean to convert the data to standard frechet from GEV
#' distributed data (default = FALSE)
#' @param frech_bool (default = TRUE) is the data frechet distributed, otherwise we
#' assume loc, scale and shape parameters constant
##' @param cov_mod A character string corresponding to the covariance model in the
#' max-stable representation. See SpatialExtremes::fitmaxstab() documentation.
#' @param min_common_obs minimum number of common pairwise observations,
#' weight is set to zero in fitting otherwise (default is 10)
#' @param min_pairs set a minimum number of pairs needed for fitting (default is choose(10,2))
#' @param sample_bool repeat fitting for samples of stations (default is FALSE)
#' @param fit_sample a matrix for columns for the different samples to fit,
#' the rows are the indexes of stations from fit_info to sample
#'
#' @return Returns a list of fitted max-stable models
#' @export
#'
#' @examples
#' n.site <- 60
#' locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
#' id <- paste("ID", as.character(1:n.site), sep = "")
#' fit_info <- data.frame(x = locations[,1],
#'                        y = locations[,2],
#'                        id,
#'                        stringsAsFactors = FALSE)
#' sim_data <- SpatialExtremes::rmaxstab(100, locations, cov.mod = "gauss",
#'                   cov11 = 1, cov12 = 0, cov22 = 1)
#' sim_data <- data.frame(sim_data)
#' names(sim_data) <- id
#'
#' fitM = outer_wrapper_fitmaxstab(fit_info = fit_info,
#'                 obs_data = sim_data,
#'                 cov_mod = "gauss")
#'
#' start_list = list(cov11=1, cov12 = 0, cov22 = 1)
#' fitM_start = outer_wrapper_fitmaxstab(fit_info = fit_info,
#'                 obs_data = sim_data,
#'                 cov_mod = "gauss",
#'                 start = start_list)
#'
#' fit_sample = get_samples(n = nrow(fit_info),
#'                  sample_type = "partition",
#'                  num_partitions = 2)
#'
#' eg1_model_list = outer_wrapper_fitmaxstab(fit_info = fit_info, obs_data = sim_data,
#'                 cov_mod = "gauss", sample_bool = TRUE,
#'                 fit_sample = fit_sample, start = start_list)
#'
#' fit_sample = get_samples(n = nrow(fit_info),
#'                  sample_type = "percentage",
#'                  num_samples = 2, percentage = 80)
#'
#' eg2_model_list = outer_wrapper_fitmaxstab(fit_info = fit_info, obs_data = sim_data,
#'                 cov_mod = "gauss", sample_bool = TRUE,
#'                 fit_sample = fit_sample)
#'
outer_wrapper_fitmaxstab <- function(fit_info,
                                       obs_data, convert = FALSE,
                                       frech_bool = TRUE, cov_mod,
                                       min_common_obs = 10, min_pairs = choose(10,2),
                                       sample_bool = FALSE,
                                       fit_sample = NULL, ...){

  ### ---------------------------------------------------------------------------

  ### CONVERT DATA TO STANDARD FRECHET
  if(convert == TRUE){
    data = apply(obs_data, 2, gev2frech_with_tryCatch) %>%
      as.data.frame()
    frech_bool = TRUE
  }else{
    data = obs_data %>% as.data.frame()
  }

  ### ---------------------------------------------------------------------------

  ### PREPROCESS THE DATA FOR FITTING
  print("Caution: Coherced IDs to strings")
  fit_info$id = as.character(fit_info$id)

  if(!all(fit_info$id %in% names(data)))
    stop("IDs for of fitting information and data do not match!")

  data_fit = data %>%
    select(fit_info$id)

  coord_fit = fit_info %>%
    select(x,y)

  if(!all(fit_info$id == names(data_fit)))
    stop("Order of IDs of fitting information and data do not match!")

  ### ---------------------------------------------------------------------------

  if(sample_bool == FALSE){

    fitM = inner_wrapper_fitmaxstab(data_fit = data_fit, coord_fit = coord_fit,
                                    cov_mod = cov_mod,
                                    min_common_obs = min_common_obs,
                                    min_pairs = min_pairs, ...)

    model_list = list(fitM)

  }else{

    model_list =
      foreach(i = 1:ncol(fit_sample), .packages = c("clusterExtremes")) %do%
      loop_function(i, fit_sample, data_fit, coord_fit,
                    cov_mod, min_common_obs, min_pairs, ...)
  }

  return(model_list)

}

# -----------------------------------------------------------------------------

### FIT THE MAXSTABLE MODEL
loop_function <- function(i, fit_sample, data_fit, coord_fit, cov_mod,
                          min_common_obs, min_pairs, ...){

  sample_stns = fit_sample[,i]
  sample_data_fit = data_fit[ ,sample_stns]
  sample_coord_fit = coord_fit[sample_stns, ]

  fitM = inner_wrapper_fitmaxstab(data_fit = sample_data_fit,
                                  coord_fit = sample_coord_fit,
                                  cov_mod = cov_mod,
                                  min_common_obs = min_common_obs,
                                  min_pairs = min_pairs, ...)

  return(fitM)

}
