### Plot to compare distances

# --------------------------------------------------------

# File details
thesis_dir = "/Users/saundersk1/Dropbox/Hard Drive/Thesis/"
chap_dir = "chapters/06_cluster/sections/img/"
plot_dir = paste(thesis_dir, chap_dir, sep = "")
compare_distance_plot = "compare_distance_plot.pdf"
compare_parameter_distance_plot = "compare_parameter_distance_plot.pdf"
compare_distance_path = paste(plot_dir, compare_distance_plot, sep = "")
compare_parameter_distance_path = paste(plot_dir,
                                        compare_parameter_distance_plot, sep = "")

# --------------------------------------------------------

# for plotting
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)
source("/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/R/utils_dist.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()

# --------------------------------------------------------

### Get region coords and data

# inputs needed
wa_id = 2; #wa_k = 7
print("Warning: May need to update region_id if we update the clustering in the working directory!!!")
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

# Get other distances

# Euclidean
meta_data = readRDS(paste(working_dir, "Data/AS_meta_data.rds", sep = ""))
coords = meta_data %>% filter(id %in% test_coords$id) %>% select(longitude, latitude)
DD_euclid = dist(coords, method = "manhattan", diag = TRUE, upper = TRUE)

# Location and Scale
gev_pars = apply(test_max, 2, fit_fevd_with_tryCatch)
loc = gev_pars[1,] %>% as.numeric()
scale = gev_pars[2,] %>% as.numeric()
shape = gev_pars[3,] %>% as.numeric()
DD_loc = dist(loc, method = "manhattan", diag = TRUE, upper = TRUE)
DD_scale = dist(scale, method = "manhattan", diag = TRUE, upper = TRUE)
DD_shape = dist(shape, method = "manhattan", diag = TRUE, upper = TRUE)

# --------------------------------------------------------

## Clustering
cluster_method = "Kmedoids" #"Hierarchical"
k = 7

euclid_clusters = pam(DD_euclid, k = k, diss = TRUE, medoids = NULL)
fmado_clusters = pam(DD_fmado_all, k = k, diss = TRUE, medoids = NULL)
loc_clusters = pam(DD_loc, k = k, diss = TRUE, medoids = NULL)
scale_clusters = pam(DD_scale, k = k, diss = TRUE, medoids = NULL)
shape_clusters = pam(DD_shape, k = k, diss = TRUE, medoids = NULL)

# --------------------------------------------------------

# tidy cluster info
euclid_df = data.frame(test_coords, cluster_id = euclid_clusters$clustering) %>%
  mutate(medoid = "") %>%
  mutate(distance_type = "Euclidean")
euclid_df$medoid[euclid_clusters$medoids] = "M"

fmado_df = data.frame(test_coords, cluster_id = fmado_clusters$clustering) %>%
  mutate(medoid = "") %>%
  mutate(distance_type = "F-madogram")
fmado_df$medoid[fmado_clusters$medoids] = "M"

loc_df = data.frame(test_coords, par = loc, cluster_id = loc_clusters$clustering) %>%
  mutate(medoid = "") %>%
  mutate(distance_type = "Location")
loc_df$medoid[loc_clusters$medoids] = "M"

scale_df = data.frame(test_coords, par = scale, cluster_id = scale_clusters$clustering) %>%
  mutate(medoid = "") %>%
  mutate(distance_type = "Scale")
scale_df$medoid[scale_clusters$medoids] = "M"

shape_df = data.frame(test_coords, par = shape, cluster_id = shape_clusters$clustering) %>%
  mutate(medoid = "") %>%
  mutate(distance_type = "Shape")
shape_df$medoid[shape_clusters$medoids] = "M"

### ---------------------------------------------------------------------------

### Plot the euclidean vs fmadogram clustering
plot_coords <- rbind(euclid_df, fmado_df)
medoid_coords <- plot_coords %>% filter(medoid == "M")

plot_title = paste(cluster_method, " with K =", k, sep ="")

cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                     group = distance_type)) +
  geom_point(data = medoid_coords,
                      aes(x = longitude, y = latitude,
                                       group = distance_type)) +
  coord_fixed() +
  facet_wrap(~distance_type, ncol = 2) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
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

### ---------------------------------------------------------------------------

### Plot the euclidean vs fmadogram clustering
plot_coords <- rbind(loc_df, scale_df, shape_df)
medoid_coords <- plot_coords %>% filter(medoid == "M")

plot_title = paste(cluster_method, " with K =", k, sep ="")

par_cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = distance_type)) +
  geom_point(data = medoid_coords,
             aes(x = longitude, y = latitude,
                 group = distance_type)) +
  coord_fixed() +
  facet_wrap(~distance_type, ncol = 3) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
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

par_cluster_plot

### ---------------------------------------------------------------------------

# save out the plots

pdf(file = compare_distance_path, width = 5, height = 3)
cluster_plot
dev.off()

pdf(file = compare_parameter_distance_path, width = 5, height = 3)
par_cluster_plot
dev.off()

# ### ---------------------------------------------------------------------------
# plot_coords <- rbind(loc_df, scale_df, shape_df)
#
# par_plot <- ggplot() +
#   geom_point(data = plot_coords,
#              aes(x = longitude, y = latitude,
#                  col = par %>% as.numeric(),
#                  group = distance_type))+
#   coord_fixed() +
#   facet_wrap(~distance_type, ncol = 3) +
#   geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
#   geom_path(data = tas_df, aes(x = Long, y = Lat)) +
#   scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
#   scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
#   theme_bw() +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   ggtitle(plot_title) +
#   theme(legend.position = "none",
#         legend.text = text.type.small,
#         strip.text.x = text.type.large,
#         axis.text = text.type.small,
#         plot.title = text.type.large,
#         axis.title = text.type.large)
#
# par_plot
