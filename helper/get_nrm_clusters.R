library(maptools)
library(ggmap)
library(broom)
library(dplyr)
library(rgdal)
library(rgeos)

shape_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/NRM_Clusters/"
get_nrm_clusters <- function(cluster_type, shape_dir, poly_tol = 0.005){

  # shape file names
  shape_filenames = shape_folders =
    c("NRM_clusters", "NRM_super_clusters", "NRM_sub_clusters")

  if(cluster_type == "standard") i = 1
  if(cluster_type == "super") i = 2
  if(cluster_type == "sub") i = 3

  shape_filename = shape_filenames[i]
  shape_folder = shape_folders[i]

  # read in the clusters
  map <- rgdal::readOGR(paste(shape_dir, shape_folder, sep = ""),
                      shape_filename)

  # thin the polys
  # map = map %>% maptools::thinnedSpatialPoly(tolerance = poly_tol)

  # get the classes from the map data
  map@data$id = rownames(map@data)
  map.points = ggplot2::fortify(map)
  map.df = dplyr::full_join(map.points, map@data, by="id")

  return(map.df)

}

