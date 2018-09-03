wa_id = 2; wa_k = 7
ea_id = 3; ea_k = 40
tas_id = 4; tas_k = 6

region_id = wa_id
k_values = 6:17 #seq(30,45,5)
thin_region = FALSE
hclust_method = "average"

# plot the clusters
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)

### ---------------------------------------------------------------------------

if(region_id == tas_id) k = tas_k
if(region_id == wa_id) k = wa_k
if(region_id == ea_id) k = ea_k

k_nbrs = 20
min_common_years = 20
max_euclid = 1
grid_dx = 0.1
grid_dy = 0.1

max_per_grid = 1
thin_dx = 0.1
thin_dy = 0.1

min_dist = 0.3#grid_dx*1.5

working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))

source("/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/R/utils_dist.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")

### ---------------------------------------------------------------------------

# thin the eastern australian cluster
test_coords = region_coords[[region_id]]

if(thin_region == TRUE){

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

  test_coords = filter(region_coords[[region_id]], id %in% keep_coords$id)

}# end thin code

### ---------------------------------------------------------------------------

# Get region
test_max = select(fmado_data, test_coords$id)
dim(test_max); dim(test_coords)

# Get distances
x = test_max

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
DD_euclid = dist(test_coords %>% select(-id), diag = TRUE, upper = TRUE)

# restrict the range of distances to the theoretical range
DD_fmado_range <- range_infill_missing_fmado(DD_euclid = DD_euclid,
                                             DD_fmado = DD_fmado_min, max_euclid)

# # infill the data using a smart estimator
# pair_dd = combine_dist(DD_euclid, DD_fmado_range)
# missing_ind = which(is.na(DD_fmado_range))
# missing_pairs = pair_dd[missing_ind, ] %>% select(p1, p2)
# missing_list = vector("list", nrow(missing_pairs))
# for(j in 1:nrow(missing_pairs)){
#   missing_list[[j]] = missing_pairs[j,]
# }
# time1 = Sys.time()
# fill_values <- mclapply(missing_list, smart_infill_missing_fmado, pair_dd)
# time2 = Sys.time()
# print(paste("Time taken to infill:", round(time2 - time1, 2)))
#
DD_fmado_infill <- DD_fmado_range
# DD_fmado_infill[missing_ind] = fill_values

# infill the remaining data using a crude estimator
# if(any(is.na(DD_fmado_infill))){
DD_fmado_all <- crude_infill_missing_fmado(DD_euclid = DD_euclid,
                                           DD_fmado = DD_fmado_infill,
                                           max_euclid)

# -----------------------------------------------------------------

mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
### combine distances in a data frame
dist_df <- data.frame(
  euclid = DD_euclid %>% as.numeric(),
  fmado = DD_fmado %>% as.numeric(),
  cap = DD_fmado_cap %>% as.numeric(),
  min = DD_fmado_min %>% as.numeric(),
  range = DD_fmado_range %>% as.numeric(),
  all = DD_fmado_all %>% as.numeric())

# cluster the data
# euclid_clusters = pam(DD_euclid, k = k, diss = TRUE, medoids = NULL)

fmado_list = vector("list", length(k_values))
fmado_cluster_list = vector("list", length(k_values))
fmado_medoid_list = vector("list", length(k_values))
for(i in 1:length(k_values)){
  k = k_values[i]

  hclusters = hclust(DD_fmado_all, method = hclust_method)
  hclusters_cut = cutree(hclusters, k = k)
  # fmado_list[[i]] = fmado_clusters
  # fmado_clusters = pam(DD_fmado_all, k = k, diss = TRUE, medoids = NULL)
  fmado_cluster_list[[i]] = cbind(test_coords,
                                  cluster_id = hclusters_cut) %>%
    mutate(num_clusters = k)
  # fmado_cluster_list[[i]] = cbind(test_coords, cluster_id = fmado_clusters$clustering) %>%
  #   mutate(num_clusters = k)
  # fmado_medoid_list[[i]] = test_coords[fmado_clusters$medoids, ] %>%
  #   mutate(num_clusters = k)
}

plot_coords <- do.call(rbind, fmado_cluster_list)
medoid_coords <- do.call(rbind, fmado_medoid_list)

# plot the clusters
cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude, group = num_clusters,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6)), size = 0.25) +
  # geom_point(data = medoid_coords,
  #            aes(x = longitude, y = latitude, group = num_clusters)) +
  coord_fixed() +
  facet_wrap(~num_clusters, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = text.type.small,
        axis.text = text.type.small,
        plot.title = text.type.large,
        axis.title = text.type.large) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste("K-medoids Clustering"))
cluster_plot

# -----------------------------------------------------------------

# Create grid for classification
long = test_coords$longitude
lat  =test_coords$latitude
long.seq = seq(min(long), max(long), by = grid_dx) # 0.2
lat.seq = seq(min(lat), max(lat), by = grid_dy) # 0.2
full_grid = expand.grid(longitude = long.seq, latitude = lat.seq)

# restrict grid to within australia
mainland_i = pnt.in.poly(full_grid, mainland_df)$pip
tas_i = pnt.in.poly(full_grid, tas_df)$pip
region_grid = full_grid[which(mainland_i == 1 | tas_i == 1), ]

#Remove any point that is not within min distance to a station
theta = seq(0, 2*pi, length.out = 360)
pnt.check = rep(0, nrow(region_grid))
for(i in 1:nrow(test_coords)){
  print(i)
  pnt = as.numeric(test_coords[i,] %>% select(longitude, latitude))
  circle = cbind(min_dist*cos(theta) + pnt[1], min_dist*sin(theta) + pnt[2])
  # lines(circle)
  pnt.in.circle = pnt.in.poly(region_grid, circle)$pip
  pnt.check = pnt.check + pnt.in.circle
}
grid = region_grid[pnt.check > 0, ]
region_grid = grid

# -----------------------------------------------------------------

knn_grid_list <- vector("list", length(k_values))
knn_info_list <- vector("list", length(k_values))
for(i in 1:length(k_values)){

  knn_info <- test_coords %>%
    mutate(Fmadogram = fmado_cluster_list[[i]]$cluster_id)

  # KNN Classifications: (1) Station Check and (2) Boundaries
  stn_train = knn_info %>% select(longitude, latitude)
  stn_test = stn_train
  stn_label = knn_info$Fmadogram
  knn_stations = knn(train = stn_train, test = stn_test,
                     cl = stn_label, k = k_nbrs,
                     prob = TRUE)

  knn_info <- knn_info %>%
    mutate(knn = knn_stations, prob = attr(knn_stations, "prob"))

  knn_info_list[[i]] = knn_info %>%
    mutate(num_clusters = k_values[i])

  knn_grid_labels = knn(train = stn_train, test = region_grid,
                        cl = stn_label, k = k_nbrs,
                        prob = TRUE)

  knn_grid <- region_grid %>%
    mutate(knn = knn_grid_labels, prob = attr(knn_grid_labels, "prob"))

  knn_grid_list[[i]] <- knn_grid %>%
    mutate(num_clusters = k_values[i])

}
knn_grid_info <- do.call(rbind, knn_grid_list)
knn_info <- do.call(rbind, knn_info_list)

knn_plot <- ggplot() +
  geom_raster(data = knn_grid_info,
              aes(x = longitude, y = latitude,
                  fill = as.factor(knn), alpha = prob, group = num_clusters),
              hjust = 0.5, vjust = 0.5) +
  # geom_point(data = plot_coords %>% filter(dist_type == "Fmadogram"),
  #            aes(x = longitude, y = latitude, group = dist_type,
  #                col = as.factor(cluster_id),
  #                shape = as.factor(cluster_id%%6))) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  # geom_point(data = fmado_medoid_coords, aes(x = longitude, y = latitude)) +
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitute") +
  facet_wrap(~num_clusters, ncol = 4) +
  ggtitle(paste("Hierarchical Clustering using", hclust_method)) +
  theme_bw() +
  theme(#legend.position = "none",
    legend.text = text.type.small,
    axis.text = text.type.small,
    plot.title = text.type.large,
    axis.title = text.type.large)

knn_plot

# -----------------------------------------------------------------
#
# # data = test_max
# frech_data = apply(test_max, 2, gev2frech_with_tryCatch)
# data = frech_data
#
# for(i in 1:length(k_values)){
#
# knn_info <- knn_info_list[[i]] %>%
#   mutate(x = longitude) %>%
#   mutate(y = latitude) %>%
#   mutate(cluster_id = Fmadogram) %>%
#   mutate(knn_id = knn)
#
# grid = knn_grid_list[[i]] %>%
#   mutate(x = longitude) %>%
#   mutate(y = latitude) %>%
#   mutate(knn_id = knn)
#
# medoid_indexes = fmado_clusters$medoids
# len = length(medoid_indexes)
# all_models <- vector("list", len)
# all_ellipses = NULL
# samp_size = 30
# for(i in 1:len){
#   print(i)
#   use_id = i
#   # samp_size = floor(max(sum(knn_info$knn == use_id)*2/3, 10))
#   model_list <- tryCatch({fit_smith_model(data = data, knn_info = knn_info,
#                                           medoid_indexes = medoid_indexes, use_id = i,
#                                           grid, min_class_prob = 0.5, high_class_prob = 0.75,
#                                           num_samps = 50, samp_size = samp_size,
#                                           min_common_obs = 10, min_pairs = choose(10,2),
#                                           ratio_threshold = 0.1, ellipse_alpha = 0.1,
#                                           max_iter = 3,
#                                           save_output = FALSE, output_dir = NULL,
#                                           reference_id = NULL)},
#                          error = function(e){return(NULL)})
#
#   if(is.null(model_list)) next
#
#   all_models[[i]] = model_list
#
#   ellipse_df <- get_ellipse_from_smith_model_list(model_list,
#                                                   medoid = knn_info[medoid_indexes[use_id], ]) %>%
#     mutate(region_id = use_id)
#
#   all_ellipses = rbind(all_ellipses, ellipse_df)
# }
#
# all_ellipses <- all_ellipses %>%
#   mutate(plot_index = paste(region_id, sim_index)) %>%
#   filter(!is.na(x))
