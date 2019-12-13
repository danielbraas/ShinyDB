library(tidyverse)
library(shiny)
library(shinyFiles)


# calling modules ---------------------------------------------------------

source('modules/module_app1.R')

# UI logic ----------------------------------------------------------------

ui <- navbarPage(
  title = 'Shiny modules',
  theme = 'braasian.min.css',
  
  tabPanel(
    title = 'First app',
    
    fluidRow(
      column(
        width = 2,
        csvFileInput(id = "datafile", 
                     label = "User data (.csv format)")
      ),
      column(
        width = 10,
        dataTableOutput("table")
      )
    )
  ),
  
  # Second app
  tabPanel(
    title = 'Second app',
    
    fluidRow(
      column(
        width = 2,
      ),
      column(
        width = 10,
      )
    )
  )
)

# server logic ------------------------------------------------------------

server <- function(input, output, session){
  
  # Define folder structure
  roots <- read_lines('~/config.json') %>% 
    jsonlite::fromJSON() %>% 
    .$Shiny
  
  # Define server logic for app 1
  
  datafile <- callModule(csvFile, "datafile",   # "datafile" is the namespace for the function call
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
    
}

shinyApp(ui = ui, server = server)
