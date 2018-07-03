library(extRemes)
library(RaingleExtremes)
library(SpatialExtremes)
library(dplyr)
library(SDMTools)

# read in cluster data
load("Data/test_Data.RData")
load("Data/medoid_indexes.RData")

# read in the maximum data
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"
data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = "")) %>%
  select(knn_info$id)

# covert data from GEV to Frechet for fitting
frech_data = apply(data, 2, gev2frech_with_tryCatch)

# check the region is connected
fit_info <- knn_info %>%
  mutate(class_id = (knn_id == 6))


check_clusters_connected(coord = fit_info %>% select(x,y,class_id),
                         medoid = medoid_indexes,
                         grid = )

#### -------------------------------------------------------------

filter_id_list =list(1,2,3,4,5,6,7)
library(SpatialExtremes)

iter <- utils_wrapper_fit(filter_id = 1,
                          all_cluster_info = knn_info,
                          fmado_data = fmado_data,
                          min_common_years = 20)

fit_list <- mclapply(filter_id_list,
                     utils_wrapper_fit,
                     all_cluster_info = knn_info,
                     fmado_data = fmado_data,
                     min_common_years = 20,
                     mc.cores = detectCores())

