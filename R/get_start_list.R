#' Get start list for fitmaxstab()
#'
#' Takes a list of models, and finds a set of starting values
#'
#' @param model_list a list containing max-stable models
#' @param frech_bool (default = TRUE) are the marginals standard frechet
#' @return returns a list of starting values for fitmaxstab()
#' @export
#'
#' @examples
get_start_list <- function(model_list, frech_bool = TRUE){

  cov11 = lapply(model_list, utils_get_par_fun, i = 1) %>% unlist()
  cov12 = lapply(model_list, utils_get_par_fun, i = 2) %>% unlist()
  cov22 = lapply(model_list, utils_get_par_fun, i = 3) %>% unlist()
  start_cov11 = median(cov11, na.rm = TRUE)
  start_cov12 = median(cov12, na.rm = TRUE)
  start_cov22 = median(cov22, na.rm = TRUE)

  if(frech_bool == FALSE){
    locCoeff1 = lapply(model_list, utils_get_par_fun, i = 4) %>% unlist()
    scaleCoeff1 = lapply(model_list, utils_get_par_fun, i = 5) %>% unlist()
    shapeCoeff1 = lapply(model_list, utils_get_par_fun, i = 6) %>% unlist()
    start_loc = median(locCoeff1, na.rm = TRUE)
    start_scale = median(scaleCoeff1, na.rm = TRUE)
    start_shape = median(shapeCoeff1, na.rm = TRUE)
    start_list = list(cov11 = start_cov11, cov12 = start_cov12, cov22 = start_cov22,
                      locCoeff1 = start_loc, scaleCoeff1 = start_scale, shapeCoeff1 = start_shape)
  }else{
    start_list = list(cov11 = start_cov11, cov12 = start_cov12, cov22 = start_cov22)
  }

  return(start_list)

}
