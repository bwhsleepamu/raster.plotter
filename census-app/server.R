library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

shinyServer(function(input, output) {
  
  output$map <- renderPlot({
    plot_data <- switch(input$var,
      "Percent White" = list(dataset=counties$white, color='darkgreen', title="White Demographics"),
      "Percent Black" = list(dataset=counties$black, color='black', title="Black Demographics"),
      "Percent Asian" = list(dataset=counties$asian, color='darkorange', title="Asian Demographics"),
      "Percent Hispanic" = list(dataset=counties$hispanic, color='brown', title="Hispanic Demographics")
    )
    
    
    percent_map(var=plot_data$dataset, color=plot_data$color, legend.title=plot_data$title, max = input$range[2], min = input$range[1])
  })
}
)