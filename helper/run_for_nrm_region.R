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
region_id = "NA" # c('TAS', "SWWA", "SEA", "EA', "NA", "R")
region_info <- get_region_from_nrm(region_id, map_info)

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

linkage_method = "ward.D2"

# Cluster
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/1-cluster.R")
plot(full_tree)
abline(h = min_cut_height, col ="red", lty = "dashed")

cut_h = 0.32
abline(h = cut_h, col ="red", lty = "dashed")
cut_heights = data.frame(h = full_tree$height, k = (nrow(coords)-1):1)

num_k = cut_heights %>%
  filter(h > cut_h) %>%
  select(k) %>%
  max() + 1

# -----------------------------------------------------------------------------

# Classify
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/2-classify.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_kknn.R")

classify_plot = plot_kknn(hclusters = hclusters,
                          grid_classify = grid_classify,
                          num_k = num_k)
classify_plot

# -----------------------------------------------------------------------------

# Subset
print("SUBSET NOT WORKING CURRENTLY - PRODUCING TOO MANY DISCONNECTED REGIONS")
# source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/3-subset.R")

# -----------------------------------------------------------------------------

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
# base_plot <- classify_plot
tas_df <- utils_tasmania()
base_plot <- ggplot() + geom_path(data = tas_df, aes(x= Long, y = Lat))
ell_plot = plot_ellipses(base_plot = base_plot, all_ellipses_df, alpha = 0.25)
ell_plot
