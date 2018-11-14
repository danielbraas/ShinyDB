library(shiny)

server <- function(input, output, session) {
  
  observe({
    txt <- c(paste("Value above is:", c(input$mytext1, input$mytext2)))
    
    # here I'm essentially writing a result to the text box
    # called myresults
    updateSelectInput(session, "myresults", choices = txt)
  })
  
}

ui <-   basicPage(
  h3("An example of an update* function"),
  textInput("mytext1", "Input goes here"),
  textInput("mytext2", "Input2 goes here"),
  selectInput("myresults", choices = 'something',label=h3( "Results will be printed here"))
)

shinyApp(ui = ui, server = server)
