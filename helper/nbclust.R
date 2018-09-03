test <- NbClust::NbClust(data = coords %>% select(x,y),
                         distance = NULL,
                         diss = clust_dist,
                         min.nc = 2,
                         max.nc = 50,
                         method = "average",
                         index = "all")
