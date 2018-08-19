function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    # iris[, c(input$xcol, input$ycol)]
    # cut_tree = cutree(full_tree, k = clusters()$cluster)$upper
  })

  tree_info <- reactive({
    list(cluster_ids = cutree(full_tree, k = input$clusters),
        cut_h = full_tree$height[(nrow(coords) - input$clusters + 1)]
    )
  })

  output$plot1 <- renderPlot({
    # palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    #
    # par(mar = c(5.1, 4.1, 0, 1))
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    ggplot() +
      geom_point(data =coords,
                 aes(x=x, y=y,
                     col = as.factor(tree_info()$cluster_ids),
                     shape = as.factor(tree_info()$cluster_ids%%6)),
                 size = 4,alpha = 0.75) +
      ggtitle(round(tree_info()$cut_h, 4)) +
      theme(legend.position = "none")
  })

  output$plot2 <- renderPlot({
    plot(full_tree) +
      abline(h = tree_info()$cut_h, col = "red", lty = "dotted")
  })

}
