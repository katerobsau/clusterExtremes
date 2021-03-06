# plot the ellipses
 plot_ellipses <- function(base_plot, all_ellipses_df, alpha = 0.5, plot_medoids = TRUE){

  ell_plot <-
    base_plot +
    geom_path(data = all_ellipses_df,
            aes(x=x, y= y, group = plot_group),
            alpha = alpha)
  if(plot_medoids)
    ell_plot + geom_point(data = medoids,
             aes(x=x, y =y))

  return(ell_plot)

}
