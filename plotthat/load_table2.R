library(tidyverse)

library(shiny)
library(shinyFiles)
#library(fs)

server <- function(input, output) {
  
  volumes <- c(wd = 'C:/',
               dropbox = 'C:/Users/Daniel/Dropbox/',
               Shiny = 'C:/Users/Daniel/Dropbox/R_projects/ShinyDB/')
  wd <- getwd()

# select and load a table -------------------------------------------------

  shinyFileChoose(input, "files", roots = volumes, filetypes=c('csv'))
  
  output$filepath <- renderPrint({
    parseFilePaths(volumes, input$files)$datapath
  })
  
  #Load file only if file was selected
  dat <- eventReactive(input$files,{
    
    if (!is.integer(input$files[1])){
      read.csv(parseFilePaths(volumes, input$files)$datapath)
    }
  }) 
  
  output$tab <- renderDataTable(dat())
  
# do something with the data ----------------------------------------------

#  output$plot <- renderPlot({
  
#  })
  
}

ui <- pageWithSidebar(
  headerPanel(
    "Select Data Table"
  ),
  sidebarPanel(
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = F),
    tags$p("Input File")
    
  ),

  mainPanel(
    tags$h4("Input File:"),
    tags$hr(),
    verbatimTextOutput("filepath"),
    dataTableOutput('tab')
#    plotOutput('My plot')
  )
)

shinyApp(ui = ui, server = server)