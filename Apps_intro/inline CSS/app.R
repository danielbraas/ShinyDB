library(shiny)

server <- function(input, output, session) {
  
}

ui <- basicPage(
  # this is your web page header information
  tags$head(title = 'Whatever app...',
    # here you include your inline styles
    tags$style(HTML("

      body {
        background-color: firebrick;
        color: #FAFAFA;
      }

    "))
  ),
  
  h3("CSS using the HTML tag"),
  p("Some important text")
  
)

shinyApp(ui = ui, server = server)