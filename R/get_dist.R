#' @export
#'
get_dist <- function(x, coords, min_common_years, max_euclid){

  print("Hard coded function directory in get_dist")
  source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/utils_dist.R")
  print("Defined DD_Common twice fix")
  # update fmado distances for clustering
  DD_fmado <- get_fmado_dist(x)

  # cap the maximum fmado distances
  DD_fmado_cap <- cap_fmado_dist(DD_fmado = DD_fmado)

  # count the overlapping observations between pairs
  DD_common <- get_num_common_obs(x)

  # if overlapping observations are too few, set to NA
  DD_fmado_min <- apply_min_obs(DD_fmado = DD_fmado_cap,
                              DD_common = DD_common,
                              min_common_years = min_common_years)

  # get euclid for the region
  DD_euclid = dist(coords, diag = TRUE, upper = TRUE)

  # restrict the range of distances to the theoretical range
  DD_fmado_range <- range_infill_missing_fmado(DD_euclid = DD_euclid,
                                             DD_fmado = DD_fmado_min,
                                             max_euclid = max_euclid)

  # # infill the data using a smart estimator
  DD_fmado_infill <- DD_fmado_range
  # DD_fmado_infill[missing_ind] = fill_values

  # infill the remaining data using a crude estimator
  # if(any(is.na(DD_fmado_infill))){
  DD_fmado_all <- crude_infill_missing_fmado(DD_euclid = DD_euclid,
                                           DD_fmado = DD_fmado_infill,
                                           max_euclid = max_euclid)

  return(DD_fmado_all)
}
