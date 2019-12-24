library(tidyverse)
library(shiny)
library(shinyFiles)
library(DT)

source('module_table11.R')


# UI logic ----------------------------------------------------------------

ui <- fluidPage(
  title = 'Plotthat',
  theme = shinythemes::shinytheme('braasian'),
  titlePanel('Plotthat'),
  
  fluidRow(
    column(
      width = 2,
      loadFileUI(id = "Plotthatfile", 
                 label = 'Select a data file'),
      tags$hr(),
      h3("Plot information"),
      selectInput(inputId = 'column_x',
                  label = 'x-Column ', 
                  choices =  character(0)),
      selectInput(inputId = 'column_y',
                  label = 'y-Column ', 
                  choices = character(0)),
      selectInput(inputId = 'color', 
                  label = 'Color', 
                  choices = character(0)),
      h3('Data table info'),
      selectInput(inputId = 'rows', 
                  label = 'Use rownames?',
                  choices = character(0))
      
    ),
    column(
      width = 10,
      verbatimTextOutput("filepath"),
      hr(),
      plotOutput(outputId = "plot", 
                 click = "user_click", 
                 brush = "user_brush"),
      DTOutput('summary')
      
    )
  )
)

# server logic ------------------------------------------------------------

server <- function(input, output, session){
  
  volumes <- jsonlite::fromJSON(read_lines('~/config.json'))$Shiny
  
  
  dat <- callModule(loadFile, 
                    id = "Plotthatfile",   # "Plotthatfile" is the namespace for the function call
                    volumes = volumes,
                    file.type = c('csv','rds'))
  
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
    
    # Explicitly naming columns in case that they do not conform to variable names
    x_col <- paste0('`', input$column_x, '`')
    y_col <- paste0('`', input$column_y, '`')
    
    p <- ggplot(dat(), aes_string(x = x_col, y = y_col)) +
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
      
      ret <- dat2()
      
    } else {
      
      ret <- dat()
      
    }
    
    if(input$rows %in% c('None','')){
      
      ret <- datatable(ret,
                       editable = 'cell')
      
    } else {
      
      ret <- datatable(ret, 
                       editable = 'cell',
                       rownames = pull(ret, input$rows))
    }
    ret
  })
}

shinyApp(ui, server)
