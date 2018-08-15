region_names = c("TAS", "SWWA", "SEA", "EA", "NA", "R")
classify_list = vector("list", length(region_names))
ell_list = vector("list", length(region_names))
cut_h_values = c(0.12,0.13,0.12, 0.12, 0.13, 0.125)
# for(cut_h in h_values){
for(i in 1:length(region_names)){
  cut_h = cut_h_values[i]
  region_name = region_names[i]
  classify_file_name = paste("plots/Summary/classify_", region_name, "_cut_h_", cut_h, ".rds", sep="")
  classify_plot <- readRDS(file = classify_file_name)
  classify_list[[i]] = classify_plot
  ell_file_name = paste("plots/Summary/ellipses_", region_name, "_cut_h_", cut_h, ".rds", sep="")
  ell_plot <- readRDS(file =  ell_file_name)
  ell_list[[i]] = ell_plot
}
