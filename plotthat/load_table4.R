library(tidyverse)

library(shiny)
library(shinyFiles)
#library(fs)

server <- function(input, output, session) {
  
  volumes <- c(wd = 'C:/',
               dropbox = 'C:/Users/Daniel/Dropbox/',
               R_projects = 'C:/Users/Daniel/Dropbox/R_projects/',
               Shiny = 'C:/Users/Daniel/Dropbox/R_projects/ShinyDB/')
  wd <- getwd()

# select and load a table -------------------------------------------------

  shinyFileChoose(input, "files", roots = volumes, filetypes=c('csv'))
  
  # Prints the filepath in the main 
  output$filepath <- renderPrint({
    req(input$files)
    parseFilePaths(volumes, input$files)$datapath
  })
  
  #Load file only if file was selected
  dat <- eventReactive(input$files,{
    
    if (!is.integer(input$files[1])){
      read.csv(parseFilePaths(volumes, input$files)$datapath)
    }
  }) 
  
 # output$tab <- renderDataTable(dat())
  
  
  df <- reactive({
    req(input$dataset)
    switch(input$dataset,
           pressure = pressure,
           cars = cars,
           mtcars = mtcars)
  })
  
  observe({
    updateSelectInput(session, 'column_x', choices = c('Choose'='', names(dat())))
    updateSelectInput(session, 'column_y', choices = c('Choose'='', names(dat())))
  })
  
  coldata <- reactive({
    req(input$column_x)
    req(input$column_y)
    #dat()[[c(input$column_x, input$column_y)]]
    select(dat(), input$column_x, input$column_y)
  })
  
  
  output$summary <- renderDataTable({
    coldata()
  })
  
# do something with the data ----------------------------------------------

#  output$plot <- renderPlot({
  
#  })
  
}

ui <- pageWithSidebar(
  headerPanel(
    "Select Data Table"
  ),
  
  sidebarPanel(
    tags$p(strong("Input File")),
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = F),
    tags$hr(),
    #selectInput('dataset', 'Dataset', c("Choose"="", 'pressure','cars','mtcars')),
    selectInput('column_x','x-Column ',character(0)),
    selectInput('column_y','y-Column ',character(0))
  ),

  mainPanel(
    tags$h4("Input File:"),
    tags$hr(),
    dataTableOutput('summary'),
    verbatimTextOutput("filepath")
  #  dataTableOutput('tab')
#    plotOutput('My plot')
  )
)

shinyApp(ui = ui, server = server)