
# is disjoint ?? (if yes, create new id in class and cluster ids)

stn_classify = apply(hclusters %>% select(-k, -h), 1,
                      classify_with_kknn,
                      coords = coords,
                      points_classify = coords %>% select(x, y),
                      knn_value = knn_value)

# check low prob ?? (if yes, keep old ids, but create new ids for fitting)

# check high prob ?? (if yes, keep old ids, but create new ids for fitting)
