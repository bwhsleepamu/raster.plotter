  library(shiny)

shinyUI(fluidPage(
  titlePanel("Protocol Visualizer"),
  
  fluidRow(
    column(width=3,
      uiOutput("subject_select")
    ),
    column(width=4,
      uiOutput('day_slider')
    ),
    column(width=3,
      uiOutput('data_checkboxes')
    ),
    column(width=2,
      actionButton("plot", "Plot Raster")
    )
  ),
  fluidRow(
    column(width=12,
      plotOutput("raster")       
    )
  )
  
  
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Select a stock to examine. 
#                Information will be collected from yahoo finance."),
#       
#       textInput("symb", "Symbol", "SPY"),
#       
#       dateRangeInput("dates", 
#                      "Date range",
#                      start = "2013-01-01", 
#                      end = as.character(Sys.Date())),
#       
#       actionButton("get", "Get Stock"),
#       
#       br(),
#       br(),
#       
#       checkboxInput("log", "Plot y axis on log scale", 
#                     value = FALSE),
#       
#       checkboxInput("adjust", 
#                     "Adjust prices for inflation", value = FALSE)
#       ),
#     
#     mainPanel(plotOutput("plot"))
#  )
))