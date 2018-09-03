print("HACKED THIS IN ALONG WITH dists in R folders")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")
print("Warning got two count functions!!!")

# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_dendrogram.R")


# -----------------------------------------------------------------------------

# Get Region
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_nrm_clusters.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_region_from_nrm.R")

map_info <- get_nrm_clusters(cluster_type = nrm_cluster_type, shape_dir = shape_dir)
region_name = "EA"
num_examples = 4
dendro_list = vector("list", num_examples)

cut_h = 0
y_range = c(0, 1/6)

region_info <- get_region_from_nrm(region_id=region_name, map_info)

# -----------------------------------------------------------------------------

### Get data for fitting

# Coordinte data
bool_pip = SDMTools::pnt.in.poly(region_coords %>% select(x,y),
                                 region_info %>% select(long,lat))$pip

coords = region_coords[bool_pip == TRUE,] %>%
  filter(y > -30 & y< -25)

# Maximum data
max_data = select(fmado_data, coords$id)
dim(max_data); dim(coords)
if(ncol(max_data) != nrow(coords)){
  stop("Dimensions of max_data and coords do not match")
}

# Get cluster distance
print("Dist file reference!!!")
clust_dist <- get_dist(x = max_data,
                       coords = coords %>% select(-id),
                       min_common_years = min_common_years,
                       max_euclid = max_euclid)

# -----------------------------------------------------------------------------
# Thin the coordinates

source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/thin_coords.R")

thin_coords_1 <- thin_coords(coords = coords,
                             thin_dx = 0.1, thin_dy = 0.1, max_per_grid = 1)

thin_dist_1 <- get_dist(x = max_data %>% select(thin_coords_1$id),
                       coords = thin_coords_1 %>% select(-id),
                       min_common_years = min_common_years,
                       max_euclid = max_euclid)

thin_coords_15 <- thin_coords(coords = coords,
                              thin_dx = 0.15, thin_dy = 0.15, max_per_grid = 1)
thin_dist_15 <- get_dist(x = max_data %>% select(thin_coords_15$id),
                        coords = thin_coords_15 %>% select(-id),
                        min_common_years = min_common_years,
                        max_euclid = max_euclid)

thin_coords_2 <- thin_coords(coords = coords,
                             thin_dx = 0.2, thin_dy = 0.2, max_per_grid = 1)
thin_dist_2 <- get_dist(x = max_data %>% select(thin_coords_2$id),
                        coords = thin_coords_2 %>% select(-id),
                        min_common_years = min_common_years,
                        max_euclid = max_euclid)

# -----------------------------------------------------------------------------

DD_list = list(no_thin = clust_dist, thin_1 = thin_dist_1,
               thin_15 = thin_dist_15, thin_2 = thin_dist_2)

# -----------------------------------------------------------------------------

## Clustering
linkage_method = "mcquitty"
# Generate dendrograms
dendro_list = vector("list", 4)
for(i in 1:4){
  hclusters = hclust(DD_list[[i]], method = linkage_method)
  dendro_list[[i]] = hclusters
}

# Cut the tree
cut_h = 0.15
num_k = 25
cutree_vec = NULL
for(i in 1:4){
    cluster_ids = cutree(dendro_list[[i]], k = num_k)
    cutree_vec = c(cutree_vec, cluster_ids)
}

# PAM
pam_df = NULL
for(i in 1:4){
  pam_output = pam(DD_list[[i]], k = num_k, diss = TRUE, medoids = NULL)
  medoid_vec = rep("", attr(DD_list[[i]], "Size"))
  medoid_vec[pam_output$medoids] = "M"
  temp_df = data.frame(cluster_id = pam_output$clustering,
                       medoids = medoid_vec)
  pam_df = rbind(pam_df, temp_df)
}

# -----------------------------------------------------------------------------

# Tidy for plotting
thin_names = c("None", "Thin 0.1", "Thin 0.15", "Thin 0.2")
len = lapply(DD_list, function(l){attr(l, "Size")}) %>% as.numeric()
thin_rep = rep(thin_names, times = len)

hclust_info = rbind(coords, thin_coords_1, thin_coords_15, thin_coords_2) %>%
  mutate(cluster_id = cutree_vec) %>%
  mutate(clust_type = "Hierarchical") %>%
  mutate(thin_type = thin_rep)

num_rows = nrow(hclust_info)
medoid_ind = medoid_list %>% unlist() %>% as.numeric()
kclust_info = rbind(coords, thin_coords_1, thin_coords_15, thin_coords_2) %>%
  cbind(pam_df) %>%
  mutate(clust_type = "K-medoids") %>%
  mutate(thin_type = thin_rep)

# -----------------------------------------------------------------------------

# Plot K - Medoids
plot_coords <- kclust_info
medoid_coords <- plot_coords %>% filter(medoids == "M")
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
plot_title = paste("K-medoids with K =", num_k, sep ="")

kcluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = x, y = y,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = thin_type)) +
  geom_point(data = medoid_coords, aes(x = x, y = y,
                                       group = thin_type)) +
  coord_fixed() +
  facet_wrap(~thin_type, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$x) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$y) + c(-0.1, 0.1)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(plot_title) +
  theme(legend.position = "none")#,
  #       legend.text = text.type.small,
  #       strip.text.x = text.type.large,
  #       axis.text = text.type.small,
  #       plot.title = text.type.large,
  #       axis.title = text.type.large)

kcluster_plot

# -----------------------------------------------------------------------------

# Plot Hierarhical

plot_coords <- hclust_info
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
plot_title = paste("Hierarchical Clustering with h = ", cut_h, sep ="")

hcluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = x, y = y,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = thin_type)) +
  coord_fixed() +
  facet_wrap(~thin_type, ncol = 4) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$x) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$y) + c(-0.1, 0.1)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(plot_title) +
  theme(legend.position = "none")#,
#       legend.text = text.type.small,
#       strip.text.x = text.type.large,
#       axis.text = text.type.small,
#       plot.title = text.type.large,
#       axis.title = text.type.large)

hcluster_plot


#------------------------------------------------------------------------------

grid_domain = get_grid_for_classification(coords = coords,
                                          grid_space = 0.1,
                                          min_dist = 0.5)

cluster_ids = cutree(dendro_list[[1]], k = num_k)
# min_cluster_size = 10
# table_ids = sort(table(cluster_ids))
# keep_ids_i = which(table_ids > min_cluster_size)
# keep_ids = names(table_ids)[keep_ids_i]
plot_coords <- cbind(coords, cluster_ids = cluster_ids) #%>%
#   filter(cluster_ids %in% keep_ids)
# cluster_ids = cluster_ids[cluster_ids %in% keep_ids]
grid_classify = classify_with_kknn(coords = plot_coords,
                   cluster_ids = cluster_ids,
                   points_classify = grid_domain,
                   knn_value = 20)

# Plot the classification
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_kknn.R")
classify_plot = plot_kknn(plot_coords = plot_coords,
                          grid_plot = grid_classify,
                          show_legend = FALSE)
classify_plot

# Get Polygons
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_contour.R")
poly_plot <- plot_grid_polygons(grid_input = grid_classify)
poly_plot

