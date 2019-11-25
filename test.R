library(tidyverse)
library(shiny)
library(shinyFiles)

ui <- pageWithSidebar(
  headerPanel(
    "Select Data Table"
  ),
  
  sidebarPanel(
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = F)
  ),
  mainPanel(
    DT::dataTableOutput('summary')
  )
)

server <- function(input, output, session){
  volumes <- jsonlite::fromJSON(read_lines('C:/R_lib/config.json'))$Shiny
  
  shinyFileChoose(input, "files", roots = volumes, filetypes=c('csv'))
  
  dat <- reactive({
    if (!is.integer(input$files[1])){
      
      read_csv(parseFilePaths(volumes, input$files)$datapath)
    } else {
      tibble('A' = NA, 'B' = NA)
    }
    
  })
  
  output$summary <- DT::renderDataTable({
    DT::datatable(dat(), editable = 'cell')
  })
}

shinyApp(ui = ui, server = server)