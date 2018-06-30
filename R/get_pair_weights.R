#' Set pair weights
#'
#' For fitting a statistical model using composite likelihood pairs of
#' stations must have overlapping observations.
#'
#' Here we set a minimum number of overlapping observations before we
#' will consider pairs for fitting, otherwise the weight is set to 0.
#'
#' @param data data frame with observations
#' @param min_common_obs minimum number of shared observations, default is 10
#'
#' @return Returns a vector of pair weights for input into composite likelihood
#' @export
#' @examples
# library(SpatialExtremes)
# library(devtools)
# install_github("saundersk1/RaingleExtremes")

##Define the coordinate of each location
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "whitmat", nugget = 0, range = 3,
                 smooth = 0.5)

data[1:40, 1] = NA
data[1:20, 2] = NA

pairs = expand.grid(j = 1:30, i = 1:30) %>%
  as.data.frame() %>%
  filter(i < j) %>%
  mutate(pair_weights = ifelse(i == 1 | j == 1, 0, 1))

pair_weights <- utils_get_pair_weights(data, 10)

##Fit a max-stable process using the Schlather's model
loc.form <-~ 1
scale.form <-~ 1
shape.form <-~ 1

# fails with the na column of data
m = fitmaxstab(data, locations, "whitmat", loc.form, scale.form,
               shape.form) #, weights = pair_weights)

# passes without the na column of data
m = fitmaxstab(data[,-1], locations[-1,], "whitmat", loc.form, scale.form,
               shape.form)
start_list = as.list(m$fitted.values)

# try weights (j > i)
m_wghts = fitmaxstab(data, locations, "whitmat", loc.form, scale.form,
                     shape.form, start = start_list, weights = pairs$pair_weights)

get_pair_weights <- function(data, min_common_obs = 10){

  num_stns = ncol(data)

  common_count <- get_num_common_obs(data)

  pairs = expand.grid(j = 1:num_stns, i = 1:num_stns) %>%
    as.data.frame() %>%
    filter(i < j) %>%
    mutate(pair_weights = ifelse(i == 1 | j == 1, 0, 1)) %>%
    mutate(common = common_count) %>%
    mutate(pair_weights = ifelse(common < min_common_obs, 0, pair_weights))

  return(pairs$pair_weights)

}
