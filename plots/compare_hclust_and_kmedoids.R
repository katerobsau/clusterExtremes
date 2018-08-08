library(maptools)
library(ggplot2)
library(ggmap)
library(broom)
library(tidyr)
library(oz)
library(dplyr)

# File details
thesis_dir = "/Users/saundersk1/Dropbox/Hard Drive/Thesis/"
chap_dir = "chapters/06_cluster/sections/img/"
plot_dir = paste(thesis_dir, chap_dir, sep = "")
thin_medoids_plot = "thin_medoids_plot.pdf"
thin_hclust_plot = "thin_hclust_plot.pdf"
thin_medoids_path = paste(plot_dir, thin_medoids_plot, sep = "")
thin_hclust_path = paste(plot_dir, thin_hclust_plot, sep = "")

# shape file names
shape_filenames = shape_folders =
  c("NRM_clusters", "NRM_super_clusters", "NRM_sub_clusters")
shape_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/NRM_Clusters/"

# plot sizes
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 10)

# Gets the Australian coastline from the package Oz for plotting
mainland_region = ozRegion(sections = c(1:5,7))
mainland_xPnts = mainland_yPnts = NULL
for(i in mainland_region$lines){
  mainland_xPnts = c(mainland_xPnts, i$x)
  mainland_yPnts = c(mainland_yPnts, i$y)
}
mainland_df = data.frame(Long = mainland_xPnts, Lat = mainland_yPnts)

tas_region = ozRegion(sections = 6)
tas_xPnts = tas_yPnts = NULL
for(i in tas_region$lines){
  tas_xPnts = c(tas_xPnts, i$x)
  tas_yPnts = c(tas_yPnts, i$y)
}
tas_df = data.frame(Long = tas_xPnts, Lat = tas_yPnts)

# gets the plots
plot_list = vector("list", 3)
table_info = NULL
i = 2

shape_filename = shape_filenames[i]
shape_folder = shape_folders[i]

# read in the clusters
map <- rgdal::readOGR(paste(shape_dir, shape_folder, sep = ""),
                        shape_filename)

# thin the polys
map = map %>% thinnedSpatialPoly(tolerance = 0.005)

# get the classes from the map data
map@data$id = rownames(map@data)
map.points = fortify(map, region="id")
map.df = full_join(map.points, map@data, by="id")
table_info = rbind(table_info, map.df %>%
                       select(code, label))

# plot the classes
class_plot <- ggplot(map.df) +
    geom_polygon(aes(long,lat, group=group, fill=code), color= "white") +
    geom_path(data = mainland_df, aes(x = Long, y = Lat), col = "black", size = 0.25) +
    geom_path(data = tas_df,  aes(x = Long, y = Lat), col = "black", size = 0.25) +
    coord_equal() +
    scale_fill_discrete("Class Label") +
    theme_bw() +
    theme(axis.text = text.type.small,
          plot.title = text.type.large,
          axis.title = text.type.large) +
    xlab("Longitude") +
    ylab("Latitude")

class_plot

#------------------------------------------------------------------------------

# Station locations
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))
region_coords = do.call(rbind, region_coords)

# Get EA
EA_info <- map.df %>% filter(C_code == "EA")
EA_poly <- EA_info %>% select(long,lat) %>% as.matrix()
stn_coords <- region_coords %>% select(-id) %>% as.matrix()
pip <- pnt.in.poly(stn_coords, EA_poly)$pip %>% as.numeric()
ea_coords <- region_coords %>%
  filter(pip == 1) %>%
  filter(latitude < -24 & latitude > -30)

# Fmado data
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))
test_coords = ea_coords
test_max = select(fmado_data, test_coords$id)
dim(test_max); dim(test_coords)

# Thinning
max_per_grid = 1
thin_dx = 0.1
thin_dy = 0.1

# thin the eastern australian cluster
print("Warning: Hard Coded grid")
x_range = range(test_coords$longitude)
y_range = range(test_coords$latitude)
grid = expand.grid(x =  seq(x_range[1], x_range[2], by = thin_dx),
                   y =  seq(y_range[1], y_range[2], by = thin_dy))

# plot(grid, pch = 3, cex = 0.2)
# points(test_coords$longitude, test_coords$latitude, cex = 0.5)

temp_coords = test_coords
keep_coords = NULL
for(i in 1:nrow(grid)){
  print(i)
  cell_coords <- filter(temp_coords,
                        longitude >= grid[i,1] &  longitude <= grid[i,1] + thin_dx &
                          latitude >= grid[i,2] &  latitude <= grid[i,2] + thin_dy)
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "blue")

  if(nrow(cell_coords) == 0) next

  if(nrow(cell_coords) > max_per_grid){
    set.seed(1)
    row_samp = sample(1:nrow(cell_coords), max_per_grid)
    cell_coords = cell_coords[row_samp, ]
  }
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "red")
  keep_coords = rbind(keep_coords, cell_coords)

}

test_coords_thin1 = filter(test_coords, id %in% keep_coords$id)

# --------------------------------------------------------

max_per_grid = 1
thin_dx = 0.2
thin_dy = 0.2

# thin the eastern australian cluster
print("Warning: Hard Coded grid")
x_range = range(test_coords$longitude)
y_range = range(test_coords$latitude)
grid = expand.grid(x =  seq(x_range[1], x_range[2], by = thin_dx),
                   y =  seq(y_range[1], y_range[2], by = thin_dy))

# plot(grid, pch = 3, cex = 0.2)
# points(test_coords$longitude, test_coords$latitude, cex = 0.5)

temp_coords = test_coords
keep_coords = NULL
for(i in 1:nrow(grid)){
  print(i)
  cell_coords <- filter(temp_coords,
                        longitude >= grid[i,1] &  longitude <= grid[i,1] + thin_dx &
                          latitude >= grid[i,2] &  latitude <= grid[i,2] + thin_dy)
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "blue")

  if(nrow(cell_coords) == 0) next

  if(nrow(cell_coords) > max_per_grid){
    set.seed(1)
    row_samp = sample(1:nrow(cell_coords), max_per_grid)
    cell_coords = cell_coords[row_samp, ]
  }
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "red")
  keep_coords = rbind(keep_coords, cell_coords)

}

test_coords_thin2 = filter(test_coords, id %in% keep_coords$id)

# --------------------------------------------------------

max_per_grid = 1
thin_dx = 0.3
thin_dy = 0.3

# thin the eastern australian cluster
print("Warning: Hard Coded grid")
x_range = range(test_coords$longitude)
y_range = range(test_coords$latitude)
grid = expand.grid(x =  seq(x_range[1], x_range[2], by = thin_dx),
                   y =  seq(y_range[1], y_range[2], by = thin_dy))

# plot(grid, pch = 3, cex = 0.2)
# points(test_coords$longitude, test_coords$latitude, cex = 0.5)

temp_coords = test_coords
keep_coords = NULL
for(i in 1:nrow(grid)){
  print(i)
  cell_coords <- filter(temp_coords,
                        longitude >= grid[i,1] &  longitude <= grid[i,1] + thin_dx &
                          latitude >= grid[i,2] &  latitude <= grid[i,2] + thin_dy)
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "blue")

  if(nrow(cell_coords) == 0) next

  if(nrow(cell_coords) > max_per_grid){
    set.seed(1)
    row_samp = sample(1:nrow(cell_coords), max_per_grid)
    cell_coords = cell_coords[row_samp, ]
  }
  # points(cell_coords$longitude, cell_coords$latitude, cex = 0.5, col = "red")
  keep_coords = rbind(keep_coords, cell_coords)

}

test_coords_thin3 = filter(test_coords, id %in% keep_coords$id)

# --------------------------------------------------------

#### For distance interpolation

# Inputs
min_common_years = 20
max_euclid = 1
coord_list = list(test_coords, test_coords_thin1, test_coords_thin2, test_coords_thin3)
len = length(coord_list)
DD_list = vector("list", len)
for(i in 1:len){

  coords = coord_list[[i]]
  x = test_max %>% select(coords$id)

  # update fmado distances for clustering
  DD_fmado <- get_fmado_dist(x)

  # cap the maximum fmado distances
  DD_fmado_cap <- cap_fmado_dist(DD_fmado)

  # count the overlapping observations between pairs
  DD_common <- get_num_common_obs(x)

  # if overlapping observations are too few, set to NA
  DD_fmado_min <- apply_min_obs(DD_fmado = DD_fmado_cap,
                                DD_common, min_common_years)

  # get euclid for the region
  DD_euclid = dist(coords %>% select(-id), diag = TRUE, upper = TRUE)

  # restrict the range of distances to the theoretical range
  DD_fmado_range <- range_infill_missing_fmado(DD_euclid = DD_euclid,
                                               DD_fmado = DD_fmado_min, max_euclid)

  DD_fmado_infill <- DD_fmado_range

  DD_fmado_all <- crude_infill_missing_fmado(DD_euclid = DD_euclid,
                                             DD_fmado = DD_fmado_infill,
                                             max_euclid)

  DD_list[[i]] = DD_fmado_all

}

# --------------------------------------------------------

## Clustering
cluster_method = "Hierarchical"
linkage_method = "average"
h = 0.145
# k = 30
k_values = rep(NA, 4)
# min_cluster = 5
hcluster_list = vector("list", 4)
dendro_list = vector("list", 4)
for(i in 1:4){
  hclusters = hclust(DD_list[[i]], method = linkage_method)
  dendro_list[[i]] = hclusters
  # ggdendro_list[[i]] = ggdendrogram(hclusters, rotate = FALSE, leaf_labels = FALSE) +
  #   scale_y_continuous(limits = c(0.05, 0.16))
  cut_tree = cutree(hclusters, h = h)
  # table_cut_tree = table(cut_tree)
  # k_values[i] = length(which(table_cut_tree > 5))
  k_values[i] = length(unique(cut_tree))
  hcluster_list[[i]] = cut_tree
}

# --------------------------------------------------------

# Tidy for plot
hplot_list <- vector("list", 4)
thin_name = c("No Thin", "Thin 0.1", "Thin 0.2", "Thin 0.3")
thin_name = paste(thin_name, "with", rep("K =", 4), k_values)
for(i in 1:4){
  cluster_ids = hcluster_list[[i]]
  # new_levels = table(cluster_ids) %>% order(decreasing = TRUE)
  # cluster_ids = as.factor(cluster_ids)
  # levels(cluster_ids) = new_levels
  df = data.frame(coord_list[[i]], cluster_id = cluster_ids) %>%
    mutate(distance_type = thin_name[i])
  hplot_list[[i]] = df
}
hplot_df = do.call(rbind, hplot_list)

# --------------------------------------------------------

hplot_coords <- hplot_df
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
hplot_title = paste(cluster_method, " Clustering with h = ", h, " and ", linkage_method, " linkage method", sep ="")

hcluster_plot <- ggplot() +
  geom_point(data = hplot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = distance_type),
             size = 0.7, alpha = 0.7) +
  # geom_point(data = medoid_coords, aes(x = longitude, y = latitude,
  #                                      group = distance_type)) +
  coord_fixed() +
  facet_wrap(~distance_type, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(hplot_coords$longitude) + c(-0.1, 0.1),
                     labels = c(146, 148, 150, 152, "")) +
  scale_y_continuous(limits = range(hplot_coords$latitude) + c(-0.1, 0.1)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(hplot_title) +
  theme(legend.position = "none",
        legend.text = text.type.small,
        strip.text.x = text.type.large,
        axis.text = text.type.small,
        plot.title = text.type.large,
        axis.title = text.type.large)

hcluster_plot

## ----------------------------------------------------------------------------

## Clustering
cluster_method = "Kmedoids" #"Hierarchical"
# k = 10
cluster_list = vector("list", 4)
for(i in 1:4){
  cluster_list[[i]] = pam(DD_list[[i]], k = k_values[i], diss = TRUE, medoids = NULL)
}

# --------------------------------------------------------

# Tidy for plot
cluster_method = "K-Medoids"
plot_list <- vector("list", 4)
for(i in 1:4){
  df = data.frame(coord_list[[i]], cluster_id = cluster_list[[i]]$clustering) %>%
    mutate(medoid = "") %>%
    mutate(distance_type = thin_name[i])
  df$medoid[cluster_list[[i]]$medoids] = "M"
  plot_list[[i]] = df
}
plot_df = do.call(rbind, plot_list)

# --------------------------------------------------------

plot_coords <- plot_df
medoid_coords <- plot_coords %>% filter(medoid == "M")
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
plot_title = paste(cluster_method, " Clustering")

cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = distance_type), size = 0.7, alpha = 0.7) +
  geom_point(data = medoid_coords, aes(x = longitude, y = latitude,
                                       group = distance_type)) +
  coord_fixed() +
  facet_wrap(~distance_type, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1),
                     labels = c(146, 148, 150, 152, "")) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(plot_title) +
  theme(legend.position = "none",
        legend.text = text.type.small,
        strip.text.x = text.type.large,
        axis.text = text.type.small,
        plot.title = text.type.large,
        axis.title = text.type.large)

cluster_plot

# --------------------------------------------------------

# save out the plots
pdf(file = thin_medoids_path, width = 10, height = 5)
cluster_plot
dev.off()

pdf(file = thin_hclust_path, width =10, height = 5)
hcluster_plot
dev.off()

# --------------------------------------------------------
