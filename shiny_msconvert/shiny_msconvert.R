library(tidyverse)
library(shiny)
library(shinyFiles)

  

server <- funtion(input, output, session) {
  
  roots = c(msconvert = 'C:/pwiz',
            wd = 'C:/')
  
  shinyFileChoose(input = input,
                  id = 'msconvert',
                  roots = roots,
                  filetypes = 'exe')

}

ui <- pageWithSidebar(
  
  headerPanel(
    "Select files to be converted"
  ),
  
  sidebarPanel(
    shinyFilesButton("msconvert", "MSConvert", "Please point to the MSConvert.exe a file", multiple = F),
    tags$p(strong("Input files")),
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = T),
    selectInput(inputId = 'pol', label = 'Select polarity', choices = c('Negative', 'Positive', 'Both', 'Default'))
  ),
  
  mainPanel(
    tags$h4("Converting file:"),
    verbatimTextOutput("file")
    # maybe create some progress report
  )
  
)

shinyApp(ui = ui, server = server)

## To start this app from the R command line type:
## shell("R -e \"shiny::runApp(\'shiny_msconvert.R')\"")