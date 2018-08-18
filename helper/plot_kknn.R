# Create a plot
plot_kknn <- function(plot_coords, grid_plot, show_legend = FALSE){

  num_k = length(unique(plot_coords$cluster_id))

  mainland_df <- utils_mainland()
  tas_df <- utils_tasmania()

  kknn_plot <- ggplot() +
    geom_raster(data = grid_plot,
                aes(x=x, y=y,
                    fill = as.factor(class_id),
                    alpha = prob_summary)) +
    geom_point(data = plot_coords, aes(x=x, y=y,
                                  fill = as.factor(cluster_ids)), shape = 21) +
    # scale_color_discrete(name = "Region", limits = 1:num_k) +
    scale_fill_discrete(name = "Region", limits = 1:num_k) +
    coord_fixed() +
    geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
    geom_path(data = tas_df, aes(x = Long, y = Lat)) +
    scale_x_continuous(limits = range(grid_plot$x) + c(-min_dist, min_dist)) +
    scale_y_continuous(limits = range(grid_plot$y) + c(-min_dist, min_dist)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() #+

  if(show_legend == FALSE){
    kknn_plot <- kknn_plot +
      theme(legend.position = "none")
  }

  return(kknn_plot)
}


