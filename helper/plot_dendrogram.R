plot_upper_tree <- function(full_tree, plot_title, y_range, cut_h){

  print("Hard coded library and text sizes")
  library(dendextend)
  library(ggdendro)
  text.type.large <- element_text(size = 16)
  text.type.small <- element_text(size = 14)

  dend <- as.dendrogram(full_tree)

  dend <- color_branches(dend, h = cut_h)
  plot(cut(dend, h = y_range[1])$upper,
     main=plot_title,
     leaflab = "none",
     ylim= y_range) +
    abline(h=cut_h, lty = 2)
  plot_obj = recordPlot()
#
#   dendro <- as.dendrogram(cut(dend, h = y_range[1])$upper)
#   ddata <- dendro_data(dendro, type="rectangle")
#   plot_obj <- ggplot(segment(ddata)) +
#     geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
#     scale_y_continuous(limits = y_range) +
#     ggtitle(plot_title) +
#     theme_bw() +
#     theme(axis.text = text.type.small,
#           plot.title = text.type.large,
#           axis.title = text.type.large)

  return(plot_obj)

}

#
# # Get dendextend
# library(dendextend)
# library(colorspace)
#
# # I'll do this to just 4 clusters for illustrative purposes
# k <- 20
# cols <- rainbow_hcl(k)
# dend <- as.dendrogram(m_hclust)
# dend <- color_branches(dend, k = k)
# plot(dend)
#
# plot(dends[[6]])
#
# labels_dend <- labels(dend)
# groups <- cutree(dend, k=k, order_clusters_as_data = FALSE)
# dends <- list()
# for(i in 1:k) {
#   labels_to_keep <- labels_dend[i != groups]
#   dends[[i]] <- prune(dend, labels_to_keep)
# }
#
# par(mfrow = c(2,2))
# for(i in 1:k) {
#   plot(dends[[i]],
#        main = paste0("Tree number ", i))
# }

