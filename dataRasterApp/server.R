# server.R
library(shiny)
source("../R/plot.R")
source("webdata.R")

shinyServer(function(input, output) {
  # Reactive Functions

  # Height helper
  plotHeight <- reactive({
    if (input$plot == 0)
      return(400)
    isolate((diff(input$days)+1)*100)
  })
  
  # Dynamic UI

  # Plot
  output$raster <- renderPlot({
    if (TRUE)
      return(NULL)
    # isolate(raster_plot(subjectData(), subjectPositions(), colors, input$days[1]:input$days[2]))
  })
  #, height=plotHeight)

  
  output$file_preview <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$data_file
    
    if (is.null(inFile))
      return(NULL)
    
    as.data.table(read.csv(inFile$datapath))
  })

})


