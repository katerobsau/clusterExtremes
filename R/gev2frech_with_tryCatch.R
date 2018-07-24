#' Converts GEV distrubted data to unit Frechet data
#' All functions are wrapped to handle any errors
#'
#' @param x a vector of block maximum observations
#'
#' @return returns x transformed to unit frechet data
#' @export
#'
#' @examples
#' x = revd(100, 0, 1, 0)
#' x_frech = gev2frech_with_tryCatch(x)
#' gev_par = fit_fevd_with_tryCatch(x)
#' x_frech1 = gev2frech_with_tryCatch(x, gev_par)

gev2frech_with_tryCatch <- function(x, gev_par = NULL){
  print("PARALLELISE ME")
  if(is.null(gev_par))
    gev_par = fit_fevd_with_tryCatch(x)

  x_frech = tryCatch({
    if(length(gev_par) != 3)
      stop("Three GEV parameters were not provided")
    gev2frech(x = x, loc = gev_par[1], gev_par[2], gev_par[3])
    },
    error = function(e){return(rep(NA,length(x)))}
  )

  return(x_frech)

}


