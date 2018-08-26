region_names = c("TAS", "EA", "SEA", "SWWA", "NA", "R")
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)
data_dir = "Data/"
thesis_dir = "/Users/saundersk1/Dropbox/Hard Drive/Thesis/"
chap_dir = "chapters/06_cluster/sections/img/"

for(r in 1:length(region_names)){

  region_name = region_names[r]
  grid_classify = readRDS(file = paste(data_dir, "classify_", region_name, ".rds", sep = ""))
  hclusters = readRDS(file = paste(data_dir, "cluster_", region_name, ".rds", sep = ""))

  num_plots = 12
  if(region_name %in% c("TAS", "SWWA")) num_plots = 16

  set.seed(1)
  range_cut_heights = seq(0.11, 0.15, length.out = num_plots)
  h_values = sapply(range_cut_heights, function(cut_near_h, all_h_values){
    cut_i = which.min(abs(all_h_values - cut_near_h))
    cut_h = all_h_values[cut_i]
    return(cut_h)
    }, all_h_values = hclusters$h) %>% unique()
  len_h_values = length(h_values)
  if(len_h_values != num_plots){
    num_h_samps = num_plots - len_h_values
    possible_h_values = hclusters %>% select(h) %>% filter(h > 0.10 & h < 0.16) %>%
      unlist() %>% as.numeric()
    possible_h_values = setdiff(possible_h_values, h_values)
    if(length(possible_h_values) > num_h_samps){
      add_h_values = sample(possible_h_values, num_h_samps)
      h_values = c(h_values, add_h_values) %>% sort()
    }else{
      h_values = c(h_values, possible_h_values)
    }
  }

  poly_df <- NULL
  for(i in 1:length(h_values)){

    h_val = h_values[i]
    grid_input = grid_classify %>% filter(h == h_val)
    k_val = grid_input$k[1]

    grid_info = grid_input %>%
      mutate(z = class_id %>% as.numeric(), xc = x, yc=y) %>%
      select(-class_id)

    z = grid_info$z %>% as.numeric()
    x = grid_info$xc %>% as.numeric()
    y = grid_info$yc %>% as.numeric()

    grd <- data.frame(z = z, xc = x, yc = y)
    sp::coordinates(grd) <-~ xc + yc
    sp::gridded(grd) <- TRUE
    grd <- as(grd, "SpatialGridDataFrame")
    at <- 1:ceiling(max(z, na.rm = TRUE))
    plys <- inlmisc::Grid2Polygons(grd, level = FALSE, at = at)

    # get the classes from the map data
    map <- plys
    map@data$id = rownames(map@data)
    map.points = fortify(map) #, region="id")
    map.df = full_join(map.points, map@data, by="id")
    map.df = map.df %>%
      mutate(h = h_val, k = k_val)

    poly_df = rbind(poly_df, map.df)

  }

  # get plot poly
  poly_plot_df <- poly_df %>%
    mutate(plot_group = paste(group, h)) %>%
    mutate(facet_title = paste("h = ", round(h, 4), " k = ", k))

  num_col = 4
  if(region_name %in% c("NA", "R", "SEA")){
    num_col = 3
  }

  poly_plot <- ggplot() +
  geom_raster(data = grid_classify %>%
                filter(h %in% h_values) %>%
                mutate(facet_title = paste("h = ", round(h, 4), " k = ", k)),
                aes(x=x, y=y, fill = as.factor(class_id), alpha = prob_summary)) +
  geom_polygon(data = poly_plot_df,
                 aes(long, lat, group = group),
                  color = "black", fill = NA) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  facet_wrap(~facet_title, ncol = num_col)  +
  theme_bw() +
  theme(legend.position = "none",
              strip.text.x = text.type.large,
              axis.text = text.type.small,
              plot.title = text.type.large,
              axis.title = text.type.large)

  mainland_df <- utils_mainland()
  tas_df <- utils_tasmania()
  if(region_name == "TAS"){
    poly_plot <- poly_plot +
      geom_path(data = tas_df, aes(x = Long, y =Lat), col = "gray")
  }else{
    poly_plot <- poly_plot +
      geom_path(data = mainland_df, aes(x = Long, y =Lat), col = "gray") +
      scale_x_continuous(limits = range(x) + c(-0.25, 0.25)) +
      scale_y_continuous(limits = range(y) + c(-0.25, 0.25))
  }

  poly_plot

  plot_dir = paste(thesis_dir, chap_dir, sep = "")
  file_name = paste(plot_dir, "Summary_", region_name, ".pdf",sep="")
  width = 8.27; height = 11.64
  if(region_name %in% c("SEA", "NA", "R")){
    width = 11.64; height = 8.27
  }
  pdf(file =  file_name, width = width, height = height)
    print(poly_plot)
  dev.off()

}

# # num clusters
# if(region_name != "NA"){
#   num_clusters = apply(hclusters  %>% select(-k,-h), 1, function(row, min_cluster_size){
#     num_clusters = sum(table(row) > min_cluster_size)
#   }, min_cluster_size = 10)
#   temp_df <- data.frame(k = hclusters$k, h = hclusters$h, num_clusters = num_clusters) %>%
#     mutate(lag = num_clusters - lag(num_clusters)) %>%
#     filter(lag > 0 | is.na(lag))
#   h_values = intersect(h_values, temp_df$h)
# }

# ------------------------------------------
# k_seq = unique(poly_plot_df$k)

# print("fix length(k_seq) - keep_first for regions with i starts")
#
# if(region_name == "TAS"){
#   num_plots = 16
#   if(length(k_seq) > num_plots){
#     keep_first = 8
#     k_seq = c(k_seq[1:keep_first], k_seq[seq(keep_first + 1, length(k_seq), length.out = num_plots - keep_first)%>% floor()])
#     num_col = 4
#   }else{
#     num_col = length(k_seq)/4 %>% floor()
#   }
# }
#
# if(region_name == "SWWA"){
#   num_plots = 16
#   keep_first = 4
#   if(length(k_seq) - keep_first > num_plots){
#     i = which(k_seq >= 6)[1]
#     k_seq = c(k_seq[i:(i+keep_first - 1)], k_seq[seq(i+keep_first, length(k_seq), length.out = num_plots - keep_first) %>% floor()] )
#     num_col = 4
#   }else{
#     k_seq = kseq[keep_first: length(k_seq)]
#     num_col = length(k_seq)/4 %>% floor()
#   }
# }
#
# if(region_name == "SEA"){
#   num_plots = 12
#   keep_first = 4
#   if(length(k_seq) - keep_first > num_plots){
#     i30 = which(k_seq >= 30)[1]
#     i40 = which(k_seq >= 40)[1]
#     i50 = which(k_seq >= 50)[1]
#     i60 = which(k_seq >= 60)[1]
#     k_seq = c(k_seq[c(i30, i40, i50)], k_seq[seq(i60, length(k_seq), length.out = num_plots - keep_first + 1) %>% floor()] )
#     num_col = 3
#   }else{
#     k_seq = kseq[keep_first: length(k_seq)]
#     num_col = length(k_seq)/3 %>% floor()
#   }
# }
#
# if(region_name == "EA"){
#   num_plots = 12
#   keep_first = 4
#   if(length(k_seq) - keep_first > num_plots){
#     i40 = which(k_seq >= 40)[1]
#     i50 = which(k_seq >= 50)[1]
#     i60 = which(k_seq >= 60)[1]
#     i70 = which(k_seq >= 70)[1]
#     k_seq = c(k_seq[c(i40, i50, i60)],
#               k_seq[seq(i70, length(k_seq), length.out = num_plots - keep_first + 1) %>% floor()] )
#     num_col = 4
#   }else{
#     k_seq = kseq[keep_first: length(k_seq)]
#     num_col = length(k_seq)/4 %>% floor()
#   }
# }
#
# if(region_name == "NA"){
#   num_plots = 12
#   keep_first = 3
#   if(length(k_seq) - keep_first > num_plots){
#     i50 = which(k_seq >= 50)[1]
#     i75 = which(k_seq >= 75)[1]
#     i100 = which(k_seq >= 100)[1]
#     k_seq = c(k_seq[c(i50, i75, i100)], k_seq[seq(i100, length(k_seq), length.out = num_plots - keep_first + 1) %>% floor()][-1])
#     num_col = 3
#   }else{
#     k_seq = k_seq[keep_first: length(k_seq)]
#     num_col = length(k_seq)/3 %>% floor()
#   }
# }
#
#
# if(region_name == "R"){
#   num_plots = 12
#   keep_first = 3
#   if(length(k_seq) - keep_first > num_plots){
#     i50 = which(k_seq >= 50)[1]
#     i75 = which(k_seq >= 75)[1]
#     i100 = which(k_seq >= 100)[1]
#     k_seq = c(k_seq[c(i50, i75, i100)], k_seq[seq(i100, length(k_seq), length.out = num_plots - keep_first + 1) %>% floor()][-1])
#     num_col = 3
#   }else{
#     k_seq = k_seq[keep_first: length(k_seq)]
#     num_col = length(k_seq)/3 %>% floor()
#   }
# }
#
