# Pseudo code

# Get a region
#    - need coordinates
#    - need data

# Get the distances
#    - needs interpolation

# For that region apply a clustering method
#    - either Kmedoids or Hierarchical

# From that clustering, create a regionalisation
#    - create grid
#    - knn

# From that regionalisation produce ellipses
#    - if clustering heirarchical, need to create centres!

### ---------------------------------------------------------------------------

# Inputs
source("/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/R/utils_dist.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")

#
# # for classificaiton
# k_nbrs = 20
#
# # for thinning
# max_per_grid = 1
# thin_dx = 0.1
# thin_dy = 0.1

# --------------------------------------------------------

### Get region coords and data

# inputs needed
wa_id = 2; #wa_k = 7
ea_id = 3; #ea_k = 40
tas_id = 4; #tas_k = 6
region_id = wa_id
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"

# data read
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))

# data processing
test_coords = region_coords[[region_id]]
test_max = select(fmado_data, test_coords$id)
dim(test_max); dim(test_coords)

# --------------------------------------------------------

#### For distance interpolation

# Inputs
min_common_years = 20
max_euclid = 1

# get distances
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

DD_fmado_infill <- DD_fmado_range

DD_fmado_all <- crude_infill_missing_fmado(DD_euclid = DD_euclid,
                                           DD_fmado = DD_fmado_infill,
                                           max_euclid)

print("This interpolation definitiely needs review")

# --------------------------------------------------------

cluster_method = "Kmedoids" #"Hierarchical"
linkage_method = "average"
k = 7

if(cluster_method == "Kmedoids"){
  fmado_clusters = pam(DD_fmado_all, k = k, diss = TRUE, medoids = NULL)
  cluster_ids = fmado_clusters$clustering
  cluster_medoids = test_coords[fmado_clusters$medoids, ]
}

if(cluster_method == "Hierarchical"){
  hclusters = hclust(DD_fmado_all, method = linkage_method)
  cluster_ids = cutree(hclusters, k = k)
  cluster_medoids = data.frame(longitude = 1, latitude = 1)
  print("Warning: cluster_medoids not assigned for Hierarchical case")
}

### ---------------------------------------------------------------------------

### Plot the clustering

plot_coords <- data.frame(test_coords, cluster_id = cluster_ids)
medoid_coords <- cluster_medoids
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()

if(cluster_method == "Kmedoids")
  plot_title = paste(cluster_method, " with k=", k, sep ="")
if(cluster_method == "Hierarchical")
  plot_title = paste(cluster_method, " with k=", k, "and linkage ",
                                           linkage_method, sep ="")

cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6)), size = 0.5)

if(cluster_method == "Kmedoids")
  cluster_plot <- cluster_plot + geom_point(data = medoid_coords,
             aes(x = longitude, y = latitude))

cluster_plot <- cluster_plot +
  coord_fixed() +
  # facet_wrap(~num_clusters, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(plot_title)

cluster_plot

### ---------------------------------------------------------------------------

# for gridding
grid_dx = 0.1
grid_dy = 0.1
min_dist = 0.3 #grid_dx*1.5

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

### ---------------------------------------------------------------------------

# KNN Classifications: (1) Station Check and (2) Boundaries
k_nbrs = 20

knn_info <- test_coords %>%
  mutate(Cluster_id = cluster_ids)

stn_train = knn_info %>% select(longitude, latitude)
stn_test = stn_train
stn_label = knn_info$Cluster_id
knn_stations = knn(train = stn_train, test = stn_test,
                   cl = stn_label, k = k_nbrs,
                   prob = TRUE)
knn_info <- knn_info %>%
  mutate(knn = knn_stations, prob = attr(knn_stations, "prob"))

knn_grid_labels = knn(train = stn_train, test = region_grid,
                      cl = stn_label, k = k_nbrs,
                      prob = TRUE)

knn_grid <- region_grid %>%
  mutate(knn = knn_grid_labels, prob = attr(knn_grid_labels, "prob"))

### ---------------------------------------------------------------------------

knn_plot <- ggplot() +
  geom_raster(data = knn_grid,
              aes(x = longitude, y = latitude,
                  fill = as.factor(knn), alpha = prob), hjust = 0.5, vjust = 0.5) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat))

if(cluster_method == "Kmedoids"){
  knn_plot <- knn_plot +
    geom_point(data = medoid_coords, aes(x = longitude, y = latitude))
}

knn_plot <- knn_plot +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitute") +
  ggtitle(paste("KNN Classification using", k_nbrs, "neighbours")) +
  theme_bw() #+
  # theme(legend.position = "none")

knn_plot

### ---------------------------------------------------------------------------

if(cluster_method == "Kmedoids"){

### Fit smith models
frech_data = apply(test_max, 2, gev2frech_with_tryCatch)
data = frech_data

knn_info <- knn_info %>%
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  mutate(cluster_id = Cluster_id) %>%
  mutate(knn_id = knn)

grid = knn_grid %>%
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  mutate(knn_id = knn)

print("Warning: Hard coded inputs for fitting")
medoid_indexes = fmado_clusters$medoids
len = length(medoid_indexes)
all_models <- vector("list", len)
all_ellipses = NULL
samp_size = 20
for(i in 1:len){
  print(i)
  use_id = i
  # samp_size = floor(max(sum(knn_info$knn == use_id)*2/3, 10))
  model_list <- tryCatch({fit_smith_model(data = data, knn_info = knn_info,
                                          medoid_indexes = medoid_indexes, use_id = i,
                                          grid, min_class_prob = 0.5, high_class_prob = 0.75,
                                          num_samps = 30, samp_size = samp_size,
                                          min_common_obs = 10, min_pairs = choose(10,2),
                                          ratio_threshold = 0.1, ellipse_alpha = 0.1,
                                          max_iter = 3,
                                          save_output = FALSE, output_dir = NULL,
                                          reference_id = NULL)},
                         error = function(e){return(NULL)})

  if(is.null(model_list)) next

  all_models[[i]] = model_list

  ellipse_df <- get_ellipse_from_smith_model_list(model_list,
                                                  medoid = knn_info[medoid_indexes[use_id], ]) %>%
    mutate(region_id = use_id)

  all_ellipses = rbind(all_ellipses, ellipse_df)
}

}

### ---------------------------------------------------------------------------

print("Must have a knn_plot and have a region_id")

# plot ellipses
all_ellipses <- all_ellipses %>%
  mutate(plot_index = paste(region_id, sim_index)) %>%
  filter(!is.na(x))

# plot result
ell_plot <- knn_plot +
  geom_path(data = all_ellipses, aes(x=x, y =y, group = plot_index), alpha = 0.25) +
  theme_bw()

ell_plot

### ---------------------------------------------------------------------------
