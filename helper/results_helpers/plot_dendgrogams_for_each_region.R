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
region_names = c("TAS", "SWWA", "SEA", "EA", "NA", "R")
num_regions = length(region_names)
dendro_list = vector("list", num_regions)

cut_h = 0.12
y_range = c(0.1, 1/6)

for(i in 1:num_regions){
  region_name = region_names[i]
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

# -----------------------------------------------------------------------------

source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_dendrogram.R")
print(paste("Using linkage method", linkage_method))
full_tree = hclust(clust_dist, method = linkage_method)
plot_title = paste(
  "Upper tree for", region_name, "with a cut at h =", cut_h)
plot_obj <- plot_upper_tree(full_tree,
                            plot_title, y_range, cut_h)
dendro_list[[i]] = plot_obj

}


