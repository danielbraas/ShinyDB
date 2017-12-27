library(shiny)

shinyServer(function(input, output){
  output$myname <- renderText(input$Name)
  output$myAge <- renderText(input$Age)
  }
)