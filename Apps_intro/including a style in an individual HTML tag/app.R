library(shiny)

server <- function(input, output, session) {
  
}

ui <- basicPage(
  
  # here we style inline
  h2(style = "color:green; font-family:'Comic Sans MS'", 
     "Styles within an HTML tag (not recommended for big apps)"),
  p(style = "color:cornflowerblue; 
             size:24; 
             background:green;
             face:bold; 
             font-family:'Arial Black'",
    "Some important text")
  
)


shinyApp(ui = ui, server = server)