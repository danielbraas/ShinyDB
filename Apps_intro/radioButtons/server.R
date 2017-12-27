library(shiny)

shinyServer(
  function(input, output){
    output$myName <- renderText(input$Name)
    output$myAge <- renderText(input$Age)
    output$mySex <- renderText(input$Sex)
  }
)