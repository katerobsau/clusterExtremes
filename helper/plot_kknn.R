# Create a plot
plot_kknn <- function(hclusters, grid_classify, num_k, cut_h){

  if(missing(cut_h)){
    cluster_ids <- hclusters %>% filter(k == num_k) %>% select(-k, -h)
    grid_plot <- grid_classify %>% filter(k == num_k)
  }
  if(missing(num_k)){
    cluster_ids <- hclusters %>% filter(h == cut_h) %>% select(-k, -h)
    grid_plot <- grid_classify %>% filter(h == cut_h)
  }
  mainland_df <- utils_mainland()
  tas_df <- utils_tasmania()
  kknn_plot <- ggplot() +
    geom_raster(data = grid_plot,
                aes(x=x, y=y,
                    fill = as.factor(class_id),
                    alpha = prob_summary)) +
    geom_point(data = coords, aes(x=x, y=y,
                                  fill = as.factor(cluster_ids)), shape = 21) +
    # scale_color_discrete(name = "Region", limits = 1:num_k) +
    scale_fill_discrete(name = "Region", limits = 1:num_k) +
    coord_fixed() +
    geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
    geom_path(data = tas_df, aes(x = Long, y = Lat)) +
    scale_x_continuous(limits = range(grid_classify$x) + c(-min_dist, min_dist)) +
    scale_y_continuous(limits = range(grid_classify$y) + c(-min_dist, min_dist)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() #+
  # theme(legend.position = "none")

  return(kknn_plot)
}


