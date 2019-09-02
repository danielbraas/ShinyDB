library(shiny)

server <- function(input, output, session){
  
  y <- read.csv('iris.csv')
  observeEvent(c(input$slide,input$xcol), {
    
    z <- y[1:input$slide, input$xcol]
    output$tab <- renderTable(z)
    
  })
  
}

ui <- basicPage(
  selectInput('xcol','Choose you x input:', choices = names(iris)[1:4]),
  sliderInput('slide','How many rows', value=5, min = 1, max = nrow(iris)),
  tableOutput('tab')
)

shinyApp(ui = ui, server = server)