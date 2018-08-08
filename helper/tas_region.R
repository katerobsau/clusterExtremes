# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")

# -----------------------------------------------------------------------------

# Region ID
region_id = "TAS"

# Tasmania data
coords = region_coords %>%
  filter(y < -40.5)

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

# -----------------------------------------------------------------------------

# Cluster
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/1-cluster.R")

# Classify
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/2-classify.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_kknn.R")

# -----------------------------------------------------------------------------

# Determine k
num_k = 10
classify_plot = plot_kknn(hclusters, grid_classify, num_k)
classify_plot

# -----------------------------------------------------------------------------

# Subset
print("NEED TO ADD SUBSET FUNCTIONS!!")
# source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/3-subset.R")

# Fitting
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/4-fitting.R")

# Ellipse
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/7-ellipses.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_ellipses.R")
ell_plot = plot_ellipses(base_plot = classify_plot, all_ellipses_df)
ell_plot
