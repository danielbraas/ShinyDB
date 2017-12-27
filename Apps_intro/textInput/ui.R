library(shiny)

shinyUI(fluidPage(
  titlePanel(h1('Demonstration of textInput widget in Shiny')),
  sidebarLayout(
    sidebarPanel(h3('Information'),
                 textInput('Name','Enter Name',value=''),
                 textInput('Age','Enter Age', value='')),
    mainPanel(('You entered:'),
              textOutput("myname"),
              textOutput('myAge')
              )
  )
)
  
)