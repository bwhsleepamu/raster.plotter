# server.R
library(shiny)
#source("../R/plot.R")

shinyServer(function(input, output) {
  # Reactive Functions
  rv <- reactiveValues()
  rv$dt_list <- list()
  
  # Height helper
#   plotHeight <- reactive({
#     if (input$plot == 0)
#       return(400)
#     isolate((diff(input$days)+1)*100)
#   })
#   
  readFile <- reactive({
    inFile <- input$data_file
    print(length(rv$dt_list))
    
    if (!is.null(inFile)) {
      isolate(rv$dt_list <- c(rv$dt_list, as.data.table(read.csv(inFile$datapath))))
      print(length(rv$dt_list))
    }
  })
  
  topFile <- reactive({
    readFile()
    
    print(length(rv$dt_list))

    if(length(rv$dt_list) > 0)
      rv$dt_list[[1]]
    else
      NULL
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
    
    topFile()

  })

})


