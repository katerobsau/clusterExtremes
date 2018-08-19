pageWithSidebar(
  headerPanel('Dendrgram'),
  sidebarPanel(
    # selectInput(NULL, 'Cut Height', ),
    # selectInput('ycol', 'Y Variable', names(iris),
    #             selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', floor(nrow(coords)/50),
                 min = 1, max = nrow(coords))
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)
