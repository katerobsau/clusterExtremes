# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")

# -----------------------------------------------------------------------------

# Get Region
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_nrm_clusters.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/get_region_from_nrm.R")

map_info <- get_nrm_clusters(cluster_type = nrm_cluster_type, shape_dir = shape_dir)
region_names = c("TAS", "EA", "SEA", "SWWA", "NA", "R")
region_name = "EA"

for(i in 1:length(region_names)){

  region_name = region_names[i]
  region_info <- get_region_from_nrm(region_id = region_name, map_info)

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

  #------------------------------------------------------------------------------

  # # Plot points
  # coord_plot <- ggplot() +
  #   geom_path(data = region_info, aes(x=long, y=lat)) +
  #   geom_point(data = coords, aes(x=x, y=y))
  #
  # coord_plot

  # -----------------------------------------------------------------------------

  # Get cluster summary
  print(paste("Using linkage method", linkage_method))
  source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/1-cluster.R")

  # -----------------------------------------------------------------------------

  # # Plot dendrogram
  #
  # source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/plot_dendrogram.R")
  # # plot(full_tree)
  # # abline(h = min_cut_height, col ="red", lty = "dashed")
  # dendro_plot <-
  #   plot_upper_tree(full_tree, region_name, c(min_cut_height, 1/6), cut_h)
  # dendro_plot

  # -----------------------------------------------------------------------------

  # Classify
  print("HARD CODE: MANUALLY CHANGED GRID SPACE")
  if(region_name %in% c("EA", "SEA", "NA", "R")){
    grid_space = 0.1
  }else{
    grid_space = 0.05
  }

  source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/2-classify.R")

  saveRDS(grid_classify, file = paste("Data/classify_", region_name, ".rds", sep = ""))
  saveRDS(hclusters, file = paste("Data/cluster_", region_name, ".rds", sep = ""))

}

