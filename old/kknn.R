library(kknn)
# data(iris)
# m <- dim(iris)[1]
# val <- sample(1:m, size = round(m/3), replace = FALSE,
#               prob = rep(1/m, m))
# iris.learn <- iris[-val,]
# iris.valid <- iris[val,]
# iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
#                   kernel = "triangular")
# summary(iris.kknn)
# fit <- fitted(iris.kknn)
# table(iris.valid$Species, fit)

grid = get_grid_for_classification(point_info %>% select(x, y),
                                   grid_space = 0.1,
                                   min_dist = 0.3,
                                   restrict_aus = TRUE)

set.learn <- point_info %>% select(x,y,cluster_id)
set.learn$cluster_id <- as.factor(set.learn$cluster_id)

my.kknn <- kknn(cluster_id~., train = set.learn,
                  test = grid, distance = 2,
                  kernel = "inv", k = 10)

summary(my.kknn)
fit <- fitted(my.kknn)

test <- ggplot() +
  geom_point(data = grid, aes(x=x, y=y, col = fit),
                              shape = as.numeric(fit)%%6) +
  geom_point(data = point_info, aes(x=x, y=y,
                                    col = as.factor(cluster_id)), shape = 20)

ggplotly(test)

