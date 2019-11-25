library(tidyverse)
library(shiny)
library(shinyFiles)

dropbox <- 'C:/Users/Daniel/Dropbox/'

server <- function(input, output, session) {
  
  volumes <- c(wd = 'C:/',
               Dropbox = dropbox,
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
  dat <- reactive({
    if (!is.integer(input$files[1])){
      read.csv(parseFilePaths(volumes, input$files)$datapath,
               header = input$heading,
               row.names = input$rowNames,
               stringsAsFactors = input$strings,
               na.string = input$na.string)
    }
  }) 
  
  observe({
    print(rownames(dat()))
  })

#  Do something with the data ---------------------------------------------

  observe({
    updateSelectizeInput(session, 'rowNames', choices = c('Choose', names(dat())))
    updateSelectInput(session, 'column_x', choices = c('Choose'='', names(dat())))
    updateSelectInput(session, 'column_y', choices = c('Choose'='', names(dat())))
    updateSelectInput(session, 'color', 
                      choices = c('Choose'='', c('None', names(dat()))),
                      selected = 'None')
  })
  
  coldata <- reactive({
    req(input$column_x)
    req(input$column_y)
    
    select(dat(), input$column_x, input$column_y)

  })
  
  output$plot <- renderPlot({
    req(input$column_x, input$column_y)
    
    p <- ggplot(dat(), aes_string(input$column_x, input$column_y)) +
      geom_point()+
      theme_bw()+
      theme(text = element_text(size = 16, face = 'bold'))
    
    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }
    
    print(p)
  })
  
  
  output$summary <- renderDataTable({
    dat()
  })
  
}

ui <- pageWithSidebar(
  headerPanel(
    "Select Data Table"
  ),
  
  sidebarPanel(
    tags$p(strong("Input File")),
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = F),
    checkboxInput("heading", "Has header?"),
    #selectizeInput('rowNames', ),
    selectizeInput('rowNames', label = 'Has rownames?', character(0) ),
    checkboxInput("strings", "Coerce strings to factors"),
    textInput("na.string", "NA symbol", value = "NA"),
    tags$hr(),
    #selectInput('dataset', 'Dataset', c("Choose"="", 'pressure','cars','mtcars')),
    selectInput('column_x','x-Column ', character(0)),
    selectInput('column_y','y-Column ', character(0)),
    selectInput('color', 'Color', character(0))
  ),

  mainPanel(
    tags$h4("Input File:"),
    verbatimTextOutput("filepath"),
    tags$hr(),
    plotOutput('plot'),
    dataTableOutput('summary')
  #  dataTableOutput('tab')
  )
)

shinyApp(ui = ui, server = server)

## To start this app from the R command line type:
## shell("R -e \"shiny::runApp(\'load_table5.R')\"")