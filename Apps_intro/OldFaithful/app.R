library(tidyverse)
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    
    bins <- seq(min(faithful$waiting), 
                max(faithful$waiting), 
                length.out = input$bins + 1)
    
    ggplot2::ggplot(faithful, aes(waiting))+
      geom_histogram(breaks=bins, color='firebrick1', fill='navy')+
      theme_bw()+
      labs(x = "Waiting time to next eruption (in mins)",
           title = "Histogram of waiting times")
    
    
    #hist(x, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui=ui, server=server)