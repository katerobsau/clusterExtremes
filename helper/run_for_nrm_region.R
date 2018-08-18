print("HACKED THIS IN ALONG WITH dists in R folders")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")
print("Warning got two count functions!!!")

# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")

# -----------------------------------------------------------------------------

# Get Region
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_nrm_clusters.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_region_from_nrm.R")

map_info <- get_nrm_clusters(cluster_type = nrm_cluster_type, shape_dir = shape_dir)
region_name = "TAS"
region_info <- get_region_from_nrm(region_id=region_name, map_info)

# -----------------------------------------------------------------------------

### Get data for fitting

# Coordinte data
bool_pip = SDMTools::pnt.in.poly(region_coords %>% select(x,y),
                                 region_info %>% select(long,lat))$pip

coords = region_coords[bool_pip == TRUE,]

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
# Plot points
coord_plot <- ggplot() +
  geom_path(data = region_info, aes(x=long, y=lat)) +
  geom_point(data = coords, aes(x=x, y=y))

coord_plot

# -----------------------------------------------------------------------------

# Cluster
print(paste("Using linkage method", linkage_method))
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/1-cluster.R")
plot(full_tree)
abline(h = min_cut_height, col ="red", lty = "dashed")

# -----------------------------------------------------------------------------

# Classify
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/2-classify.R")

# Plot the classification
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_kknn.R")

# h_values = c(0.12,0.125,0.13,0.135,0.14,0.145,0.15)
# abline(h = cut_h, col ="red", lty = "dashed")
# for(cut_h in h_values){

cut_h = 0.12
use_k == TRUE
num_k = cut_heights %>%
  filter(h > cut_h) %>%
  select(k) %>%
  max() + 1

if(use_k == TRUE){
  cluster_ids <- hclusters %>% filter(k == num_k) %>% select(-k, -h)
  grid_plot <- grid_classify %>% filter(k == num_k)
  plot_coords = cbind(coords, cluster_id = cluster_ids)
}else{
  cluster_ids <- hclusters %>% filter(h == cut_h) %>% select(-k, -h)
  grid_plot <- grid_classify %>% filter(h == cut_h)
  num_k = length(unique(cluster_ids))
  plot_coords = cbind(coords, cluster_id = cluster_ids)
}

classify_plot = plot_kknn(plot_coords = plot_coords,
                          grid_plot = grid_plot,
                          show_legend = FALSE) +
  ggtitle(cut_h)

print(classify_plot)

# file_name = paste("plots/Summary/classify_", region_name, "_cut_h_", cut_h, ".rds",sep="")
# saveRDS(classify_plot,file =  file_name)
#
# }
# -----------------------------------------------------------------------------

# Subset
print("SUBSET NOT WORKING CURRENTLY - PRODUCING TOO MANY DISCONNECTED REGIONS")
# source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/3-subset.R")

# -----------------------------------------------------------------------------
# for(cut_h in h_values){
#
#   num_k = cut_heights %>%
#     filter(h > cut_h) %>%
#     select(k) %>%
#     max() + 1

# Fitting
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/4-fitting.R")

# -----------------------------------------------------------------------------

# Check fits
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/5-check_fits.R")

# -----------------------------------------------------------------------------

# Repeat fits
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/6-repeat_fits.R")

# -----------------------------------------------------------------------------

# Ellipse
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/7-ellipses.R")


source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_ellipses.R")

  # classify_plot = plot_kknn(hclusters = hclusters,
  #                           grid_classify = grid_classify,
  #                           num_k = num_k, show_legend = FALSE) +
  #   ggtitle(cut_h)

base_plot <- classify_plot
# base_plot <- ggplot() +
#   geom_path(data = mainland_df, aes(x= Long, y = Lat)) +
#   scale_x_continuous(limits = range(coords$x)) +
#   scale_y_continuous(limits = range(coords$y))
ell_plot = plot_ellipses(base_plot = base_plot, all_ellipses_df, alpha = 0.25) +
  ggtitle(num_k)
print(ell_plot)

# file_name = paste("plots/Summary/ellipses_", region_name, "_cut_h_", cut_h, ".rds", sep="")
# saveRDS(ell_plot, file =  file_name)
#
# }
