#### Initialise parameters


### ---------------------------------------------------------------------------
print("HACKED THIS IN")
# Station locations
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))
region_coords = do.call(rbind, region_coords) %>%
  mutate(x = longitude, y = latitude) %>%
  select(-longitude, - latitude)

# Fmado data
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))

### ---------------------------------------------------------------------------

# Get Distances
min_common_years = 20
max_euclid = 1

### ---------------------------------------------------------------------------

# Cluster step
cluster_method = "Hierarchical"
linkage_method = "mcquitty"
min_cut_height = 0.125

### ---------------------------------------------------------------------------

# Classify step
 # grid specifics
grid_space = 0.05
min_dist = 0.3
restrict_aus = TRUE
  # number of neighbours
knn_value = 15

### ---------------------------------------------------------------------------

# Fitting step
  #restriction
min_stns_for_fitting = 10
  #fitting parameters
convert = TRUE
frech_bool = TRUE
min_common_obs = 10
min_pairs = 10
cov_mod = "gauss"
fit_subsample = TRUE
sample_type = "percentage"
num_samples = 10
percentage = 80
# num_partitions = 3

### ---------------------------------------------------------------------------

### Check fits
ratio_threshold = 0.1
ellipse_alpha = 0.1

### ---------------------------------------------------------------------------
