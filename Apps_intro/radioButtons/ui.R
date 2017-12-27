library(shiny)

shinyUI(
  fluidPage(
    headerPanel(title = 'Demonstration of shiny widgets'),
    sidebarLayout(
      sidebarPanel(
        h3('Enter Information'),
        textInput('Name','Enter Name', value=''),
        textInput('Age','Enter Age', value=''),
        radioButtons('Sex','Select gender',choices = list('Male','Female'), selected = 'Female')
      ),
      mainPanel(
        h3('You entered:'),
        textOutput('myName'),
        textOutput('myAge'),
        textOutput('mySex')
      )
    )
  )
)
