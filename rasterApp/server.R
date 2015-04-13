# server.R
source("../R/plot.R")
source("webdata.R")

# Setup Data
main_data <- loadMainData()
subjects <- as.character(unique(main_data$sleep$subject_code))
colors <- data.table(data_label=c('SLEEP', 'WAKE', 'NREM','REM','UNDEF'), color=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))
positions <- data.table(name=c('activity','light', 'sleep','episodes','stages'), length=c(5,5,1,1,3))

shinyServer(function(input, output) {
  # Reactive Functions
  subjectData <- reactive({
    loadForSubject(input$subject_code, main_data)
  })
  
  subjectPositions <- reactive({
    #input$datatypes
    if(!is.null(input$datatypes))
      positions[name %in% input$datatypes]
    else
      positions[numeric(0)]
  })
  
  dayRange <- reactive({
    getDayRange(subjectData())
  })
  
  availableVariables <- reactive({
    names(subjectData()[sapply(subjectData(), function(x){nrow(x) > 0})])
  })
  
  # Height helper
  plotHeight <- reactive({
    if (input$plot == 0)
      return(400)
    isolate((diff(input$days)+1)*100)
  })
  
  # Dynamic UI
  output$subject_select <- renderUI({
    selectInput("subject_code", "Subject:", choices=subjects, selected=subjects[1])
  })
  
  output$data_checkboxes <- renderUI({
    checkboxGroupInput("datatypes", "Data Types:", choices=availableVariables(), selected=availableVariables())
  })
  
  output$day_slider <- renderUI({
    sliderInput("days", "Days to show:", min=dayRange()$min_day, max=dayRange()$max_day, value=dayRange())  
  })
  
  # Plot
  output$raster <- renderPlot({
    if (input$plot == 0)
      return()
    print("RASTER!")
    isolate(raster_plot(subjectData(), subjectPositions(), colors, input$days[1]:input$days[2]))
  }, height=plotHeight)


})