library(SpatialExtremes)
library(dplyr)
library(ggplot2)
library(plotly)

set.seed(4)
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "gauss", cov11 = 1, cov12 = 0.5, cov22 = 0.75)

model_list <- vector("list", 100)
for(i in 1:100){
  set.seed(i)
  l = sample(1:n.site, 10)
  model_list[[i]] <- fitmaxstab(data[,l], locations[l,], "gauss", loc.form ~ 1,
           scale.form ~ 1,
           shape.form ~ 1)
}

# get elliptical curves
fit_list = model_list
ellipse_list = lapply(model_list, utils_get_ellipse)
ellipse_df <- do.call(rbind, ellipse_list)

# caluclate ratio of elliptical curves
ratio_values = lapply(model_list, utils_check_cov_ratio) %>%
  unlist()

# combine all simulation informaiton for plotting
num_rows = lapply(ellipse_list, nrow) %>% unlist()
sim_index = rep(1:length(ellipse_list), times = num_rows)
temp <- data.frame(sim_index = 1:100, ratio = ratio_values)
ellipse_df <- ellipse_df %>%
  as.data.frame() %>%
  mutate(sim_index) %>%
  left_join(temp, by = "sim_index")

# plot elliptical curves
ell_plot <- ggplot(data = ellipse_df) +
  geom_path(aes(x=x, y =y, group = sim_index, col = ratio < 0.1), alpha = 0.25) +
  theme_bw()
ell_plot

# ggplotly(ell_plot)
check_ellipses <- utils_flag_ellipses(model_list, alpha = 0.1)
check_ellipses <- left_join(check_ellipses, ellipse_df, by = "sim_index")

# plot elliptical curves
ell_plot <- ggplot() +
  geom_path(data = ellipse_df, aes(x=x, y =y, group = sim_index), alpha = 0.25) +
  geom_path(data = check_ellipses, aes(x=x, y =y,
                                       group = sim_index, col = as.factor(sim_index))) +
  theme_bw()
ell_plot
