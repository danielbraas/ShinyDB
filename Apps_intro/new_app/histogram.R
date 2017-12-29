setwd("C:/Users/Daniel/Dropbox/projects/ShinyDB/Apps_intro/new_app")

library(shiny)



# Define UI ----
ui <- fluidPage(
  titlePanel(h1("This will be my histogram app", align = 'center')),

  sidebarLayout(
    sidebarPanel("This is the selecter",
                 sliderInput("bins",
                             "Number of bins:",
                             min = 5,
                             max = 500,
                             value = 30)
                 ),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)
