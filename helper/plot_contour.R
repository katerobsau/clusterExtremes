plot_grid_polygons <- function(grid_input){

  grid_info = grid_input %>%
    mutate(z = class_id %>% as.numeric(), xc = x, yc=y) %>%
    select(-class_id)

  z = grid_info$z %>% as.numeric()
  x = grid_info$xc %>% as.numeric()
  y = grid_info$yc %>% as.numeric()

  grd <- data.frame(z = z, xc = x, yc = y)
  sp::coordinates(grd) <- ~ xc + yc
  sp::gridded(grd) <- TRUE
  grd <- as(grd, "SpatialGridDataFrame")
  plys <- inlmisc::Grid2Polygons(grd, level = TRUE, at = at)

  # get the classes from the map data
  map <- plys
  map@data$id = rownames(map@data)
  map.points = fortify(map) #, region="id")
  map.df = full_join(map.points, map@data, by="id")

  # plot the classes
  # cluster_ids <- hclusters %>% filter(k == 28) %>% select(-k,-h) %>%
  #   as.numeric()
  # tas_df <- utils_tasmania()
  # mainland_df <- utils_mainland()
  class_plot <- ggplot() +
    # # scale_color_discrete(name = "Region", limits = 1:num_k) +
    # scale_fill_discrete(name = "Region", limits = 1:num_k)
    geom_raster(data = grid_info,
              aes(x=x, y=y,
                  fill = as.factor(z),
                  alpha = prob_summary)) +
    # geom_point(data = coords, aes(x=x, y=y,
    #                               fill = as.factor(cluster_ids)), shape = 21) +
    geom_polygon(data = map.df, aes(long,lat, group = group),
               color= "black", fill = NA) +
    # geom_path(data = mainland_df, aes(x = Long, y = Lat), col = "black", size = 0.25) +
    # geom_path(data = tas_df,  aes(x = Long, y = Lat), col = "black", size = 0.25) +
    coord_equal() +
    # scale_fill_discrete("Class Label") +
    theme_bw() +
    theme(legend.position = "none") +
    # theme(axis.text = text.type.small,
    #       plot.title = text.type.large,
    #       axis.title = text.type.large) +
    xlab("Longitude") +
    ylab("Latitude")

  return(class_plot)

}



####------------------------------------------

# image(grd, col = gray.colors(30), axes = TRUE)
# # grid(col = "black", lty = 1)
# # points(x = x, y = y, pch = 16)
# # text(cbind(xc, yc), labels = z)
# # text(cbind(x = x + 0.1, y = rev(y + 0.1)),
# #      labels = 1:((m + 1) * (n + 1)), cex = 0.6)
# at <- 1:ceiling(max(z, na.rm = TRUE))
# plys <- inlmisc::Grid2Polygons(grd, level = TRUE, at = at)
# cols <- rainbow(length(plys), alpha = 0.3)
# sp::plot(plys, add = TRUE, col = cols)
# zz <- plys[[1]]
# legend("top", legend = zz, fill = cols, bty = "n", xpd = TRUE,
#        inset = c(0, -0.1), ncol = length(plys))
# CAN'T USE CONTOUR PLOT AS ADJACENT REGIONS HAVE DIFFERENT CONTOURS
# EG 1 - 15, or 1 - 2
# grd = grid_classify %>% filter(k == 28) %>% select(x,y,class_id) %>%
#   mutate(z = class_id, xc = x, yc=y) %>%
#   select(-class_id)
#
# library(reshape2) # for melt
# volcano3d <- melt(volcano)
# names(volcano3d) <- c("x", "y", "z")
#
# # Basic plot
# v <- ggplot(volcano3d, aes(x, y, z = z))
# v + stat_contour(binwidth = 10)
#
# ggplot(volcano3d, aes(x, y, z = z)) +
#   stat_contour(breaks=c(120,140,160))
#
# ggplot(data = grd, aes(x = x, y =y , z = z %>% as.numeric())) +
#   geom_raster(data = grd,
#                aes(x = x, y =y , fill = z)) +
#   stat_contour(breaks=c(1:num_k - 0.5)) +
#   coord_fixed()
