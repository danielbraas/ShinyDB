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
    updateSelectInput(session = session, 
                         inputId = 'rows', 
                         choices = c('Choose'='', c('None', names(dat()))))
                         
    updateSelectInput(session, 'column_x', 
                      choices = c('Choose'='', names(dat())))
    
    updateSelectInput(session, 'column_y', 
                      choices = c('Choose'='', names(dat())))
    
    updateSelectInput(session, 'color', 
                      choices = c('Choose'='', c('None', names(dat()))),
                      selected = 'None')
    
  })

# Work on data to produce plot --------------------------------------------

  output$plot <- renderPlot({
    req(dat(), input$column_x, input$column_y)
    
    p <- ggplot(dat(), aes_string(input$column_x, input$column_y)) +
      geom_point()+
      theme_bw()+
      theme(text = element_text(size = 16, face = 'bold'))
    
    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }
    
    p
  })
  

# use brush and click to define the data shown in the data table ----------
  
  # global variable, what type of plot interaction
  interaction_type <- "click"
  
  # observe for user interaction and change the global interaction_type
  # variable
  observeEvent(input$user_click, {
    interaction_type <<- "click"
    })
  
  observeEvent(input$user_brush, {
    interaction_type <<- "brush"
    })

# select rows to show in table on brush or click --------------------------

    dat2 <- reactive({
    req(dat(), input$column_x, input$column_y)
    
    user_brush <- input$user_brush
    user_click <- input$user_click
    
    if(interaction_type == "brush") res <- brushedPoints(dat(), user_brush)
    if(interaction_type == "click") res <- nearPoints(dat(), user_click, threshold = 10, maxpoints = 1)
  
    return(res)
    
  })

# render output as editable table using DT package ------------------------
  
  output$summary <- renderDT({
    req(dat())
    if(nrow(dat2()) > 0){
      datatable(dat2(), 
                editable = 'cell', 
                rownames = pull(dat2(), input$rows))
    } else {
      datatable(dat(),
                editable = 'cell',
                rownames = pull(dat(), input$rows))
    }
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
    selectInput(inputId = 'rows', 
                  label = 'Use rownames?',
                  choices = character(0)),
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
    plotOutput("plot", click = "user_click", brush = "user_brush"),
    DTOutput('summary')
  )
)

shinyApp(ui = ui, server = server)

## To start this app from the R command line type:
## shell("R -e \"shiny::runApp(\'load_table5.R')\"")