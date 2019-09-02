library(shiny)
library(datasets)

ui <- fluidPage(
  selectInput('dataset', 'Dataset', c("Choose"="", 'pressure','cars','mtcars')),
  selectInput('column','Column',character(0)),
  verbatimTextOutput('summary')
)

server <- function(input, output, session){
  df <- reactive({
    req(input$dataset)
    switch(input$dataset,
           pressure = pressure,
           cars = cars,
           mtcars = mtcars)
  })
  
  observe({
    updateSelectInput(session, 'column', choices = c('Choose'='', names(df())))
  })
  
  coldata <- reactive({
    df()[[input$column]]
  })
  
  output$summary <- renderText({
    summary(coldata())
  })
}

shinyApp(ui, server)