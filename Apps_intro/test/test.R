library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel(title = h1('This is my first shiny app, hello shiny!')),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "num",
                    label = "Choose a number",
                    value = 25, min = 1, max = 100)
      ),
      mainPanel = NULL
    ),
    textOutput('number'),
    sidebarLayout(
      position = 'right',
      sidebarPanel(
        selectInput(
          inputId = 'group',
          label = 'Group',
          choices = c('Good','OK','Bad'),
          selected = 'OK'
        )
      ),
      mainPanel(h1('more text'))
    ),
    textOutput('choice')
  )
)
server <- shinyServer(
  function(input, output) {
    output$number <- input$num
    output$choice <- input$group
  }
)

shinyApp(ui = ui, server = server)