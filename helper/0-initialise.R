#### Initialise parameters

nrm_cluster_type = "super"
### ---------------------------------------------------------------------------
print("HACKED THIS IN")

# NRM shape dir
shape_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/NRM_Clusters/"

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
linkage_method = "average"
min_cut_height = 0.1

### ---------------------------------------------------------------------------

# Classify step
#  # grid specifics
grid_space = 0.05
min_dist = 0.3
restrict_aus = TRUE
  # number of neighbours
knn_value = 15

### ---------------------------------------------------------------------------

# ### NOT WORKING CURRENTLY
# ### Subset step
# high_class_prob = 0.75
# min_class_prob = 0.4

### ---------------------------------------------------------------------------

# Fitting step

  #restriction
min_stns_for_fitting = 10

 # sample parameters
seed_value = 1
sample_type = "random"
num_samples = 20
samp_size = 25
# percentage = 2/3*100

  #fitting parameters
convert = TRUE
frech_bool = TRUE
min_common_obs = 10
min_pairs = choose(min_stns_for_fitting, 2)
cov_mod = "gauss"
sample_bool = TRUE

# num_partitions = 3

### ---------------------------------------------------------------------------

### Check fits
ratio_threshold = 0.1
ellipse_alpha = 0.1

### ---------------------------------------------------------------------------
