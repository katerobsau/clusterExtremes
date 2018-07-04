#' Converts GEV distrubted data to unit Frechet data
#' All functions are wrapped to handle any errors
#'
#' @param model_list list of smith max-stable model fits
#' @param medoid a data frame with the x and y coordinates of the centre
#' of the ellipses, defualt is a data.frame(x = 0, y = 0)
#' @return returns a data frame with columns sim_index, x, and y
#' the sim_index references each the ellipse associated with each max-stable model
#' @export
#'
#' @examples
#' n.site <- 30
#' locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
#' colnames(locations) <- c("lon", "lat")
#' data1 <- rmaxstab(40, locations, cov.mod = "gauss", cov11 = 1, cov12 = 0, cov22 = 1)
#' fit1 <- fitmaxstab(data1, locations, "gauss", loc.form <- loc ~ 1, scale.form <- scale ~ 1,
#' shape.form <- shape ~ 1,)
#' data2 <- rmaxstab(40, locations, cov.mod = "gauss", cov11 = 0.5, cov12 = 0, cov22 = 0.5)
#' fit2 <- fitmaxstab(data2, locations, "gauss", loc.form <- loc ~ 1, scale.form <- scale ~ 1,
#' shape.form <- shape ~ 1,)
#' model_list = list(fit1, fit2)
#' ellipse_list = get_ellipse_from_smith_model_list(model_list, medoid = data.frame(x = 1, y = 1))
#' ellipse_df = ellipse_list %>% as.data.frame()
#' ggplot(ellipse_df, aes(x = x, y=y, group = sim_index)) + geom_path()
#'
get_ellipse_from_smith_model_list <- function(model_list,
                                              medoid = data.frame(x = 0, y = 0)){

  # get ellipses
  ellipse_list = lapply(model_list, utils_get_ellipse)
  ellipse_df <- do.call(rbind, ellipse_list) %>%
    as.data.frame()

  # shift the ellipses
  ellipse_df$x = ellipse_df$x + medoid$x
  ellipse_df$y = ellipse_df$y + medoid$y

  # combine all simulation informaiton (convienent for plottting)
  num_rows = lapply(ellipse_list, nrow) %>% unlist()
  sim_index = rep(1:length(ellipse_list), times = num_rows)
  temp <- data.frame(sim_index = 1:length(ellipse_list))
  ellipse_df <- ellipse_df %>%
    as.data.frame() %>%
    mutate(sim_index) %>%
    left_join(temp, by = "sim_index")

  return(ellipse_df)

}
