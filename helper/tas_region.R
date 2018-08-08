# Tasmania data
coords = region_coords %>%
  filter(latitude < -40.5)
max_data = select(fmado_data, coords$id)
dim(max_data); dim(coords)

#Source
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/1-cluster.R")
