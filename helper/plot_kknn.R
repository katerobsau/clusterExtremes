# Create a plot
plot_kknn <- function(plot_coords, grid_plot, show_legend = FALSE){

  text.type.large <- element_text(size = 12)
  text.type.small <- element_text(size = 11)

  num_k = length(unique(plot_coords$cluster_id))

  mainland_df <- utils_mainland()
  tas_df <- utils_tasmania()

  shape_code = rep(0:14, times = ceiling(num_k/15))[1:num_k]

  kknn_plot <- ggplot() +
    geom_raster(data = grid_plot,
                aes(x=x, y=y,
                    fill = as.factor(class_id),
                    alpha = prob_summary)) +
    geom_point(data = plot_coords, aes(x=x, y=y,
                                  fill = as.factor(cluster_ids)
                                  ),
               shape = as.factor(cluster_ids%%14), size = 0.85) +
    # scale_color_discrete(name = "Region", limits = 1:num_k) +
    # scale_fill_discrete(name = "Region", limits = 1:num_k) +
    coord_fixed() +
    geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
    geom_path(data = tas_df, aes(x = Long, y = Lat)) +
    scale_x_continuous(limits = range(grid_plot$x) + c(-min_dist, min_dist)) +
    scale_y_continuous(limits = range(grid_plot$y) + c(-min_dist, min_dist)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(axis.text = text.type.small,
          plot.title = text.type.large,
          axis.title = text.type.large)

  if(show_legend == FALSE){
    kknn_plot <- kknn_plot +
      theme(legend.position = "none")
  }

  return(kknn_plot)
}

# # cut_h = 0.1156991 #0.12
# use_k = TRUE
# num_k = cut_heights %>%
#   filter(h > cut_h) %>%
#   select(k) %>%
#   max() + 1
#
# if(use_k == TRUE){
#   cluster_ids <- hclusters %>% filter(k == num_k) %>% select(-k, -h)
#   grid_plot <- grid_classify %>% filter(k == num_k)
#   plot_coords = cbind(coords, cluster_id = cluster_ids)
# }else{
  cluster_ids <- hclusters %>% filter(h == cut_h) %>%
    select(-k, -h) %>% as.numeric()
  grid_plot <- grid_classify %>% filter(h == cut_h)
  num_k = length(unique(cluster_ids))
  plot_coords = coords %>% mutate(cluster_id = cluster_ids)
# }

classify_plot = plot_kknn(plot_coords = plot_coords,
                          grid_plot = grid_plot,
                          show_legend = FALSE) +
  ggtitle(paste("H =", round(cut_h, 3), ", K =", num_k))

print(classify_plot)

# file_name = paste("plots/Summary/classify_", region_name, "_cut_h_", cut_h, ".rds",sep="")
# saveRDS(classify_plot,file =  file_name)
#
# }
#
# # Plot the contours
# source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_contour.R")
# poly_plot <- plot_grid_polygons(grid_input = grid_plot)
