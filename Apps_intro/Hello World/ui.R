library(shiny)

shinyUI(fluidPage(
  titlePanel(title = 'This is my first shiny app, hello shiny!'),
  sidebarLayout(position = 'right',
    sidebarPanel(h3('This is a sidebar'), h4('widget4'), h5('widget5')),
    mainPanel(h4('this is the main panel text'))
  )
  )  
)