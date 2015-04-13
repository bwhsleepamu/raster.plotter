library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Piotr's Shiny App!")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      h2("Installation"),
      p("Some boooring instructions..."),
      code("c^2 = b^2 + a^2"),
      br(),
      br(),
      fluidRow(
        column(4, img(src='bigorb.png', class='img-responsive')),
        column(8, 
          span(
            "Shiny is a product of ",
            a("you", href='www.google.com')
          )
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Intro"),
      p("Haa you think I'll write actual content here..."),
      p("Maybe check this out: ", a("shadylink", href='www.espn.com')),
      h2("Features"),
      tags$ul(
        tags$li("Booooring"),
        tags$li("Lots of uses"),
        tags$li("But so booooring.", strong("VERY BORING!!"))
      ),
      fluidRow(
        
        column(3,
               h3("Buttons"),
               actionButton("action", label = "Action"),
               br(),
               br(), 
               submitButton("Submit")),
        
        column(3,
               h3("Single checkbox"),
               checkboxInput("checkbox", label = "Choice A", value = TRUE)),
        
        column(3, 
               checkboxGroupInput("checkGroup", 
                                  label = h3("Checkbox group"), 
                                  choices = list("Choice 1" = 1, 
                                                 "Choice 2" = 2, "Choice 3" = 3),
                                  selected = 1)),
        
        column(3, 
               dateInput("date", 
                         label = h3("Date input"), 
                         value = "2014-01-01"))   
      ),
      
      fluidRow(
        
        column(3,
               dateRangeInput("dates", label = h3("Date range"))),
        
        column(3,
               fileInput("file", label = h3("File input"))),
        
        column(3, 
               h3("Help text"),
               helpText("Note: help text isn't a true widget,", 
                        "but it provides an easy way to add text to",
                        "accompany other widgets.")),
        
        column(3, 
               numericInput("num", 
                            label = h3("Numeric input"), 
                            value = 1))   
      ),
      
      fluidRow(
        
        column(3,
               radioButtons("radio", label = h3("Radio buttons"),
                            choices = list("Choice 1" = 1, "Choice 2" = 2,
                                           "Choice 3" = 3),selected = 1)),
        
        column(3,
               selectInput("select", label = h3("Select box"), 
                           choices = list("Choice 1" = 1, "Choice 2" = 2,
                                          "Choice 3" = 3), selected = 1)),
        
        column(3, 
               sliderInput("slider1", label = h3("Sliders"),
                           min = 0, max = 100, value = 50),
               sliderInput("slider2", "",
                           min = 0, max = 100, value = c(25, 75))
        ),
        
        column(3, 
               textInput("text", label = h3("Text input"), 
                         value = "Enter text..."))   
      ),
      fluidRow(
        column(4,
          p("Create dem maps yo!"),
          selectInput("race", 
            label = h3("Choose a race:"), 
            choices = list("Black" = 1, "White" = 2, "Asian" = 3, "Hispanic" = 4), selected = 2
          )
          
        ),
        column(8,
          sliderInput("race_range", label="Range:",  min = 0, max = 100, value = c(0, 100))
        
        )
      )
    )
  )
))