### ---------------------------------------------------------------------------

point_info <- data.frame(test_coords, cluster_id = hcluster_list[[4]])
names(point_info)[2:3] = c('x', 'y')
var_name = "cluster_id"
use_id = 7
one_fit = FALSE
min_common_obs = 10
min_pairs = 10
frech_bool = TRUE
cov_mod = "gauss"
# sample_type = "partition"; num_partitions = 5
sample_type = "random"; num_samples = 10; samp_size = 20;
# sample_type = "percentage"; percentage = 0.75

# ## GENERATE AN EXAMPLE
#
# n.site <- 60
# locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
# cluster_id <- rep( c(1,2), each = n.site/2)
# id <- paste("ID", as.character(1:n.site), sep = "")
#
# data <- SpatialExtremes::rmaxstab(100, locations, cov.mod = "gauss",
#                   cov11 = 1, cov12 = 0, cov22 = 1)
# data <- as.data.frame(data)
# names(data) = id
#
# point_info <- data.frame(id,
#                          x = locations[,1], y = locations[,2],
#                          cluster_id,
#                          stringsAsFactors = FALSE)
#
# use_id = "1"

cluster_plot <-  ggplot(data = point_info) +
  geom_point(aes(x=x , y = y, col = as.factor(cluster_id)))

cluster_plot

### ---------------------------------------------------------------------------

fit_subsample = TRUE
min_common_obs = 10
min_pairs = 10
frech_bool = TRUE
cov_mod = "gauss"
# sample_type = "partition"; num_partitions = 5
sample_type = "random"; num_samples = 10; samp_size = 20;
# sample_type = "percentage"; percentage = 0.75

### IDENTIFY CLUSTER/KNN ID FOR FITTING
if(missing(var_name) | missing(use_id)){
  point_info <- point_info %>%
    dplyr::mutate(binary_clase = TRUE)
}else{
  point_info <- create_binary_class(point_info = point_info,
                                    var_name = var_name, use_id = use_id)
}

if(!any(point_info$binary_class == TRUE))
  stop("No binary class matched the use_id provided")

fit_info <- point_info %>%
  dplyr::filter(binary_class == TRUE)

### ---------------------------------------------------------------------------

### CHECK THE FITS

# caluclate ratio of elliptical curves
ratio_values = lapply(model_list, utils_check_cov_ratio) %>%
  unlist()



### ---------------------------------------------------------------------------

# Need function to get subset of distance matrix from a dist object!
DD = dist(coord_fit)
medoid = get_medoid(coords = coord_fit, DD)

ell = utils_get_ellipse(fitM) %>%
  data.frame()
ell$x = ell$x + medoid$x
ell$y = ell$y + medoid$y

plot(coord_fit)
lines(ell)
points(medoid, pch = 3)

### ---------------------------------------------------------------------------


sample_bool = TRUE
samp_size = 30
set.seed(1)
if(sample_bool){
  sample_cols = sample(1:nrow(fit_info), samp_size, replace = FALSE)
  data_fit = data_fit[ ,sample_cols]
  coord_fit = coord_fit[sample_cols, ]
}

### ---------------------------------------------------------------------------

grid_domain <- get_grid_for_classification(
                    coords = point_info %>% select(x,y),
                    grid_space = 0.1,
                    restrict_aus = TRUE)

### ---------------------------------------------------------------------------

# KNN Classifications: (1) Station Check and (2) Boundaries
k_nbrs = 20

stn_train = point_info %>% select(x, y)
stn_test = stn_train
stn_label = point_info$cluster_id
knn_stations = knn(train = stn_train, test = stn_test,
                   cl = stn_label, k = k_nbrs,
                   prob = TRUE)
knn_info <- knn_info %>%
  mutate(knn = knn_stations, prob = attr(knn_stations, "prob"))

knn_grid_labels = knn(train = stn_train, test = region_grid,
                      cl = stn_label, k = k_nbrs,
                      prob = TRUE)

knn_grid <- region_grid %>%
  mutate(knn = knn_grid_labels, prob = attr(knn_grid_labels, "prob"))

### ---------------------------------------------------------------------------

grid_class_info <- create_binary_class(point_info = grid_domain,
                                       var_name = "cluster_id",
                                       use_id = 4)

# check the region is connected
connection_check = check_clusters_connected(
    coord = fit_info %>% select(x, y, class_id),
    grid = grid,
    medoid_ref = FALSE) %>%
    as.numeric()

  # handle more than one region
  temp_info <- fit_info %>%
    dplyr::mutate(new_class_id = paste(class_id, connection_check, sep = "-"))
  count_info <- temp_info %>%
    dplyr::filter(class_id == TRUE) %>%
    dplyr::count(new_class_id)

  if(nrow(count_info) != 1){
    max_i = which.max(count_info$n)
    max_id = count_info$new_class_id[max_i]
    fit_info <- temp_info %>%
      mutate(class_id = if_else(new_class_id == max_id,
                                class_id, FALSE))
    connection_check = fit_info$class_id
  }

#   id_check = all(knn_info$cluster_id %in% knn_info$knn_id) &
#     all(knn_info$knn_id %in% knn_info$cluster_id)
#   if(id_check == FALSE) warning("Error: cluster_id and knn_id does not match")
#
#   id1_check = all(knn_info$knn_id %in% grid$knn_id) &
#     all(grid$knn_id %in% knn_info$knn_id)
#   if(id1_check == FALSE) stop("Error: grid_id and knn_id does not match")
#
#   range_check = all(max(grid$x) >= knn_info$x) &
#     all(min(grid$x) <= knn_info$x) &
#     all(max(grid$y) >= knn_info$y) &
#     all(min(grid$y) <= knn_info$y)
#
#   if(range_check == FALSE) warning("Warning: grid does not cover range of station data")
#
#   if(is.integer(num_samps)) stop("Error: num_samps must be an integer")
#   if(is.integer(samp_size)) stop("Error:samp_size must be an integer")
#   if(is.integer(max_iter)) stop("Error: max_iter must be an integer")
#
#   if(min_class_prob > 1 | min_class_prob < 0)
#     stop("Error: incorrect value for min_class_prob specified")
#
#   if(ellipse_alpha > 0.5 | ellipse_alpha < 0)
#     stop("Error: incorrect value for ellipse_alpha specified")
#
#   if(ncol(data) != nrow(knn_info))
#     stop("Error: There data and knn_info have incompatible dimensions")
#
#   # create a new class_id that is binary
#   fit_info <- knn_info %>%
#     mutate(class_id = (knn_id == use_id))
#   grid <- grid %>%
#     mutate(class_id = (knn_id == use_id))
#
#   # check the region is connected
#   connection_check = check_clusters_connected(
#     coord = fit_info %>% select(x, y, class_id),
#     grid = grid,
#     medoid_ref = FALSE) %>%
#     as.numeric()
#
#   # handle more than one region
#   temp_info <- fit_info %>%
#     dplyr::mutate(new_class_id = paste(class_id, connection_check, sep = "-"))
#   count_info <- temp_info %>%
#     dplyr::filter(class_id == TRUE) %>%
#     dplyr::count(new_class_id)
#
#   if(nrow(count_info) != 1){
#     max_i = which.max(count_info$n)
#     max_id = count_info$new_class_id[max_i]
#     fit_info <- temp_info %>%
#       mutate(class_id = if_else(new_class_id == max_id,
#                                 class_id, FALSE))
#     connection_check = fit_info$class_id
#   }

# get representative objects
medoid_coords = fit_info %>%
  filter(class_id == TRUE) %>%
  select(x, y)
DD = dist(medoid_coords)
medoid = get_medoid(coords = medoid_coords, DD)

# # check plot
# ggplot(fit_info, aes(x=x, y =y)) + geom_point(aes(col = as.factor(connection_check)))

# -----------------------------------------------------------------------------

# get possible stations for fitting
possible_stns = which(connection_check == TRUE &
                        fit_info$prob > min_class_prob &
                        !((fit_info$cluster_id != fit_info$knn_id) & fit_info$prob > high_class_prob)) %>%
  as.numeric()

if(length(possible_stns) < samp_size + 1){
  warning("Warning: too few stations for sampling, reduce samp_size")
  samp_size = length(possible_stns) - 1
}

# -----------------------------------------------------------------------------

}
