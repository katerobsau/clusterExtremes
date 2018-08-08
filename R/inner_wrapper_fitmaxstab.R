#' Fit a max-stable model
#'
#' Here we set a minimum number of overlapping observations before we
#' will consider pairs for fitting, otherwise the weight is set to 0.
#'
#' @param data_fit dataframe or matrix with rows of observations, and columns referencing locations.
#' @param coord_fit dataframe or matrix of locations coordinates
#' @param cov_mod A character string corresponding to the covariance model in the
#' max-stable representation. See SpatialExtremes::fitmaxstab() documentation.
#' @param frech_bool (default = TRUE) is the data frechet distributed, otherwise we
#' assume loc, scale and shape parameters constant
#' @param min_common_obs minimum number of common pairwise observations,
#' weight is set to zero in fitting otherwise (default is 10)
#' @param min_pairs set a minimum number of pairs needed for fitting (default is 10)
#' @param start_list an option list of start values to initilise
#' the fitmaxstab() model (default = NULL)

#'
#' @return Returns a fitted maxstable model from the package SpatialExtremes,
#' the reason for the wrapper is so that an NA is returned if there was an error or warning.
#'
#' @export
#' @examples
#'
#' ##Define the coordinate of each location
#' library(SpatialExtremes)
#' n.site <- 30
#' locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
#' colnames(locations) <- c("lon", "lat")
#'
#' ## Simulate a max-stable process - with unit Frechet margins
#' sim_data <- SpatialExtremes::rmaxstab(40, locations, cov.mod = "whitmat",
#'                  nugget = 0, range = 3, smooth = 0.5)
#'
#'##Fit a max-stable process using the Schlather's model
#' m =  SpatialExtremes::fitmaxstab(sim_data, locations, "whitmat")
#' m_wrapper = inner_wrapper_fitmaxstab(data_fit = sim_data,
#'                  coord_fit = locations, cov_mod = "whitmat")
#' all(m$fitted.values == m_wrapper$fitted.values)
#'
#'## Pass a start value
#'start_list = as.list(m$fitted.values)
#'
#' m_start = inner_wrapper_fitmaxstab(data_fit = sim_data,
#'                  coord_fit = locations, cov_mod = "whitmat",
#'                  start = start_list)
#'
inner_wrapper_fitmaxstab <- function(data_fit, coord_fit, cov_mod = "gauss",
                                     frech_bool = TRUE, min_common_obs = 10,
                                     min_pairs = 10, ...){

  data_fit = as.matrix(data_fit)
  coord_fit = as.matrix(coord_fit)

  pair_weights = get_pair_weights(data_fit, min_common_obs)
  if(sum(pair_weights) < min_pairs){
    warning("Warning: too few common pairs for fitting, reduce sample size")
  }

  fitM <- tryCatch({
    if(frech_bool == TRUE){
      fitM = fitmaxstab(data = data_fit,
                        coord = coord_fit,
                        marg.cov = NULL,
                        cov.mod = cov_mod,
                        iso = FALSE,
                        weights = pair_weights,
                        fit.marge = FALSE, ...)
    }else{
      fitM = fitmaxstab(data = data_fit,
                        coord = coord_fit,
                        loc.form <- loc ~ 1,
                        scale.form <- scale ~ 1,
                        shape.form <- shape ~ 1,
                        marg.cov = NULL,
                        cov.mod = cov_mod,
                        iso = FALSE,
                        weights = pair_weights, ...)
    }
    fitM;
  }, warning = function(w){
    cat("WARNING :",conditionMessage(w), "\n");
    return(NA)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n");
    return(NA)
  })

  return(fitM)

}
