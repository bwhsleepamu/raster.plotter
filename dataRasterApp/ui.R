library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(width=7,
      uiOutput("subject_select"),
      div(class='panel panel-default',
        div(class='panel-heading',
          "Upload Data Files"
        ),
        div(class='panel-body',
          fileInput('data_file', '', accept=c('text/csv','text/comma-separated-values','.csv'))
        )
      ),
      div(class='panel panel-default',
        div(class='panel-heading',
          "Data File List"
        ),
        tags$table(class='table table-striped',
          tags$thead(tags$th("file"), tags$th("labtime column"), tags$th("value column"), tags$th("Y Range"), tags$th("Day Range"), tags$th("Color"), tags$th("Plot?")),
          tags$tbody(
            tags$th(tags$a("filename.csv")),
            tags$td(
              tags$select(class='form-control', id='labtime_column',
                tags$option("labtime column")
              )
            ),
            tags$td(
              tags$select(class='form-control', id='value_column',
                tags$option("value column")
              )
            ),
            tags$td(div(class='form-inline', style='max-width: 180px',
              tags$input(type='text', class='form-control', placeholder='min', style='width: 40%', id='y_min'),
              tags$input(type='text', class='form-control', placeholder='max', style='width: 40%', id='y_max')
            )),
            tags$td(div(class='form-inline', style='max-width: 180px',
              tags$input(type='text', class='form-control', placeholder='min', style='width: 40%', id='day_min'),
              tags$input(type='text', class='form-control', placeholder='max', style='width: 40%', id='day_max')
            )),
            tags$td(
              tags$input(type='text', class='form-control', placeholder='color')
            ),
            tags$td(tags$label(class="checkbox-inline",
              tags$input(type='checkbox', id='include', value='include')
            ))
          )
        )
      )
    ),
    column(width=5,
      div(class='panel panel-default',
        div(class='panel-heading', "File Preview"),
      
        div(class='panel-body', style="overflow: scroll; height: 400px",
          dataTableOutput("file_preview")
        )
      )
    )
  ),
  fluidRow(
    column(width=12,
      div(class='panel panel-default',
        div(class='panel-heading', "Raster Plot"),
        div(class='panel-body',
          plotOutput("raster")       
        )
      )
      
    )
  )
))