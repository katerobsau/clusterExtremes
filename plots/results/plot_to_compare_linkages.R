### Plot to compare distances
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)
source("/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/R/utils_dist.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")
# --------------------------------------------------------

### Get region coords and data

# inputs needed
ea_id = 3; #wa_k = 7
print("Warning: May need to update region_id if we update the clustering in the working directory!!!")
region_id = ea_id
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"

# data read
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))

# data processing
test_coords = do.call(rbind, region_coords) %>%
  filter(latitude > -35 & latitude < -25 & longitude > 145)
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

## Clustering
cluster_method = "Hierarchical"
linkage_method = c("ward.D", "ward.D2", "single", "complete",  "average", "mcquitty", "median", "centroid")
k = 10
len = length(linkage_method)
hcluster_list = vector("list", len)
for(i in 1:len){
  hclusters = hclust(DD_fmado_all, method = linkage_method[i])
  hcluster_list[[i]] = cutree(hclusters, k = k)
}

# --------------------------------------------------------

# Tidy for plot
hplot_list <- vector("list", len)
for(i in 1:len){
  df = data.frame(test_coords, cluster_id = hcluster_list[[i]]) %>%
    mutate(linkage = linkage_method[i])
  hplot_list[[i]] = df
}
hplot_df = do.call(rbind, hplot_list)

# --------------------------------------------------------

hplot_coords <- hplot_df
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
hplot_title = paste(cluster_method, " with K =", k, sep ="")

hcluster_plot <- ggplot() +
  geom_point(data = hplot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = linkage)) +
  # geom_point(data = medoid_coords, aes(x = longitude, y = latitude,
  #                                      group = distance_type)) +
  coord_fixed() +
  facet_wrap(~linkage, ncol = 3) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(hplot_coords$longitude) + c(-0.1, 0.1)) +
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
