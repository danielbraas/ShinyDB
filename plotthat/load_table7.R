library(tidyverse)
library(shiny)
library(shinyFiles)
library(DT)


# I modified the Rprofile file of the base package to set the variable for dropbox

server <- function(input, output, session) {
  
  volumes <- jsonlite::fromJSON(read_lines('C:/R_lib/config.json'))$Shiny

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
      
      read_csv(parseFilePaths(volumes, input$files)$datapath,
               col_names = input$heading,
               na = input$na.string)
    }
  })

# update values for selecters ---------------------------------------------

  observe({
    updateSelectizeInput(session = session, 
                         inputId = 'rows', 
                         choices = c('Choose'='', c('None', names(dat()))),
                         selected = 'None')
    updateSelectInput(session, 'column_x', 
                      choices = c('Choose'='', names(dat())))
    
    updateSelectInput(session, 'column_y', 
                      choices = c('Choose'='', names(dat())))
    
    updateSelectInput(session, 'color', 
                      choices = c('Choose'='', c('None', names(dat()))),
                      selected = 'None')
    
    print(input$rows)
  })

# Work on data to produce plot --------------------------------------------

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

  output$summary <- renderDT({
    
    datatable(dat(), editable = 'cell', rownames = input$rows)
  })
  
}

ui <- pageWithSidebar(
  headerPanel(
    "Select Data Table"
  ),
  
  sidebarPanel(
    tags$p(strong("Input File")),
    shinyFilesButton("files", "Choose File", "Please select a file", multiple = F),
    checkboxInput(inputId = "heading", 
                  label = "Has header?"),
    selectizeInput(inputId = 'rows', 
                  label = 'Use rownames?',
                  choices = character(0),
                  selected = 'None'),
    textInput("na.string", "NA symbol", value = "NA"),
    tags$hr(),
    selectInput('column_x','x-Column ', character(0)),
    selectInput('column_y','y-Column ', character(0)),
    selectInput('color', 'Color', character(0))
  ),

  mainPanel(
    tags$h4("Input File:"),
    verbatimTextOutput("filepath"),
    tags$hr(),
    plotOutput('plot'),
    DTOutput('summary')
  )
)

shinyApp(ui = ui, server = server)

## To start this app from the R command line type:
## shell("R -e \"shiny::runApp(\'load_table5.R')\"")