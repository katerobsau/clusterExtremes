print("WARNING: MANUALLY SHIFTED THE POLYGONS TO TRY AND ADDRESS PROJECTION DIFFERENCES")
print("WARNING: FOR RASTER CAN USE HFILL AND VFILL I THINK MAYBE")
print("WARNINGS: MANUALLY CHOSEN CUT HEIGHTS")

# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")

region_names = c("TAS", "EA", "SEA", "SWWA", "NA", "R")
full_region_names = c("Tasmania", "Eastern Australia",
                      "Southeast Australia",
                      "Southwest Western Australia",
                      "Northern Australia",
                      "Regional Australia")
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)
data_dir = "Data/"
thesis_dir = "/Users/saundersk1/Dropbox/Hard Drive/Thesis/"
chap_dir = "chapters/06_cluster/sections/img/"

map_info <- get_nrm_clusters(cluster_type = nrm_cluster_type, shape_dir = shape_dir)
for(r in 1:length(region_names)){

  region_name = region_names[r]
  grid_classify = readRDS(file = paste(data_dir, "classify_", region_name, ".rds", sep = ""))
  hclusters = readRDS(file = paste(data_dir, "cluster_", region_name, ".rds", sep = ""))

  if(region_name %in% c("TAS", "SEA", "EA")){
    cut_near_h = 0.115
  }else{
    # SWWA, R, NA
    cut_near_h = 0.133
  }

  all_h_values = hclusters$h
  cut_i = which.min(abs(all_h_values - cut_near_h))
  h_val = all_h_values[cut_i]

  grid_input = grid_classify %>% filter(h == h_val)
  k_val = grid_input$k[1]

    grid_info = grid_input %>%
      mutate(z = class_id %>% as.numeric(), xc = x, yc=y) %>%
      select(-class_id)

    z = grid_info$z %>% as.numeric()
    x = grid_info$xc %>% as.numeric()
    y = grid_info$yc %>% as.numeric()

    grd <- data.frame(z = z, xc = x, yc = y)
    sp::coordinates(grd) <-~ xc + yc
    sp::gridded(grd) <- TRUE
    grd <- as(grd, "SpatialGridDataFrame")
    at <- 1:ceiling(max(z, na.rm = TRUE))
    plys <- inlmisc::Grid2Polygons(grd, level = FALSE, at = at)

    # get the classes from the map data
    map <- plys
    map@data$id = rownames(map@data)
    map.points = fortify(map) #, region="id")
    map.df = full_join(map.points, map@data, by="id")
    map.df = map.df %>%
      mutate(h = h_val, k = k_val)

    poly_plot_df = map.df %>%
      mutate(plot_group = paste(group, h)) %>%
      mutate(facet_title = paste("h = ", round(h, 4), " k = ", k))

    # Portraite vs Landscape
width = 8.27; height = 11.64
if(region_name %in% c("SEA", "NA", "R")){
  width = 11.64; height = 8.27
}

# Base Map
if(region_name == "EA")
  base_map <- get_map(location = c(150,-28), zoom = 5,
                     maptype = "satellite")

if(region_name == "SEA")
  base_map <- get_map(location = c(142.5,-36), zoom = 5,
                     maptype = "satellite")

if(region_name == "SWWA")
  base_map <- get_map(location = "Perth", zoom = 6,
                    maptype = "satellite")

if(region_name == "TAS")
  base_map <- get_map(location = "Tasmania", zoom = 7,
                     maptype = "satellite")

if(region_name == "NA")
  base_map <- get_map(location = "Katherine", zoom = 4,
                      maptype = "satellite")

if(region_name == "R")
  base_map <- get_map(location = "Uluru", zoom = 4,
                      maptype = "satellite")

  # Coordinte data
  region_info <- get_region_from_nrm(region_id = region_name, map_info)
  bool_pip = SDMTools::pnt.in.poly(region_coords %>% select(x,y),
                                 region_info %>% select(long,lat))$pip
  coords = region_coords[bool_pip == TRUE,]

  # Pnt Size
  pnt_size = 0.1; pnt_alpha = 0.2
  if(region_name %in% c("TAS", "SWWA", "NA", "R")){
    pnt_size = 1; pnt_alpha = 0.5
  }

satellite_plot <- ggmap(base_map) +
  coord_fixed() +
  coord_map(xlim = range(grid_classify$x) + c(-0.5, 0.5),
            ylim = range(grid_classify$y) + c(-0.5, 0.5)) +
  geom_polygon(data = poly_plot_df %>%
                 mutate(x = long - grid_space/2, y = lat - grid_space/2),
               aes(x, y, group = group),
               color = "white", fill = NA, size = 0.2) +
  geom_point(data = coords, aes(x=x, y = y),
             col = "white", size = pnt_size, shape = 3, alpha = pnt_alpha) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(full_region_names[r]) +
  theme(axis.text = text.type.small,
        plot.title = text.type.large,
        axis.title = text.type.large)

  file_name = paste(plot_dir, "satellite_", region_name, ".pdf",sep="")
  pdf(file = file_name, width = width, height = height)
    print(satellite_plot)
  dev.off()

}

#
#   ea_plot <- ggmap(ea_map) +
#     coord_fixed() +
#     coord_map(xlim = range(coords$x) + c(-0.5, 0.5),
#               ylim = range(coords$y) + c(-0.5, 0.5)) +
#     geom_polygon(data = poly_plot_df %>%
#                    filter(k == 195) %>%
#                    mutate(x = long - grid_space/2, y = lat - grid_space/2),
#                  aes(x, y, group = group),
#                  color = "white", fill = NA, size = 0.2) +
#     geom_point(data = coords, aes(x=x, y = y),
#                col = "white", size = 0.1, shape = 3, alpha = 0.2) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     ggtitle("Eastern Australia") +
#     theme(axis.text = text.type.small,
#           plot.title = text.type.large,
#           axis.title = text.type.large)
#   file_name = paste(plot_dir, region_name, "_satellite.pdf",sep="")
#   pdf(file = file_name, width = 6, height = 8)
#     print(ea_plot)
#   dev.off()
# }
#
#
# if(region_name == "SEA"){
#
#   sea_map <- get_map(location = c(142.5,-36), zoom = 5,
#                     maptype = "satellite")
#
#   sea_plot <- ggmap(sea_map) +
#     coord_fixed() +
#     coord_map(xlim = range(coords$x) + c(-0.5, 0.5),
#               ylim = range(coords$y) + c(-0.5, 0.5)) +
#     geom_polygon(data = poly_plot_df %>%
#                    filter(k == 188) %>%
#                    mutate(x = long - grid_space/2, y = lat - grid_space/2),
#                  aes(x, y, group = group),
#                  color = "white", fill = NA, size = 0.2) +
#     geom_point(data = coords, aes(x=x, y = y),
#                col = "white", size = 0.1, shape = 3, alpha = 0.2) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     ggtitle("Southeast Australia") +
#     theme(axis.text = text.type.small,
#           plot.title = text.type.large,
#           axis.title = text.type.large)
#
#   file_name = paste(plot_dir, region_name, "_satellite.pdf",sep="")
#   pdf(file = file_name, width = 8, height = 6)
#     print(sea_plot)
#   dev.off()
# }
#
#
# if(region_name == "SWWA"){
#
#   swwa_map <- get_map(location = "Perth", zoom = 6,
#                     maptype = "satellite")
#
#   swwa_plot <- ggmap(swwa_map) +
#     coord_fixed() +
#     coord_map(xlim = range(coords$x) + c(-0.5, 0.5),
#             ylim = range(coords$y) + c(-0.5, 0.5)) +
#     ggtitle("SWWA") +
#     geom_polygon(data = poly_plot_df %>%
#                  filter(k == 53) %>%
#                  mutate(x = long - grid_space/2, y = lat - grid_space/2),
#                aes(x, y, group = group),
#                color = "white",  size = 0.5, fill = NA) +
#     geom_point(data = coords, aes(x=x, y = y),
#                col = "white", size = 1, shape = 3, alpha = 0.5) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     ggtitle("Southwest Western Australia") +
#     theme_bw() +
#     theme(axis.text = text.type.small,
#           plot.title = text.type.large,
#           axis.title = text.type.large)
#   file_name = paste(plot_dir, region_name, "_satellite.pdf",sep="")
#   pdf(file = file_name, width = 6, height = 8)
#     print(swwa_plot)
#   dev.off()
#
# }
#
# if(region_name == "TAS"){
#   tas_map <- get_map(location = "Tasmania", zoom = 7,
#                     maptype = "satellite")
#   tas_df <- utils_tasmania()
#   tas_plot <-
#     ggmap(tas_map) +
#     coord_fixed() +
#     coord_map(xlim = range(tas_df$Long) + c(-0.5, 0.5),
#             ylim = range(tas_df$Lat) + c(-0.5, 0.5)) +
#     geom_polygon(data = poly_plot_df %>%
#                  filter(k == 36) %>%
#                  mutate(x = long - grid_space/2, y = lat - grid_space/2),
#                aes(x, y, group = group),
#                color = "white", size = 0.5, fill = NA) +
#     geom_point(data = coords, aes(x=x, y = y),
#                col = "white", size = 1, shape = 3, alpha = 0.5) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     ggtitle("Eastern Australia") +
#     theme(axis.text = text.type.small,
#           plot.title = text.type.large,
#           axis.title = text.type.large)
#     file_name = paste(plot_dir, region_name, "_satellite.pdf",sep="")
#     pdf(file = file_name, width = 6, height = 8)
#       print(tas_plot)
#     dev.off()
# }
#
# }
