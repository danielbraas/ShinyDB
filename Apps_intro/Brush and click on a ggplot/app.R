library(shiny)

library(ggplot2)


server <- function(input, output, session) {
  
  # global variable, what type of plot interaction
  interaction_type <- "click"
  
  # observe for user interaction and change the global interaction_type
  # variable
  observeEvent(input$user_click, {
    interaction_type <<- "click"
    print(input$user_click)
    })
  observeEvent(input$user_brush, {
    interaction_type <<- "brush"
    print(input$user_brush)
    })

    
  output$plot <- renderPlot({
    p <- ggplot(mtcars, aes_string(input$x_col, input$y_col)) + geom_point()
    p
  })
  
  # generate the data to put in the table
  dat <- reactive({
    
    user_brush <- input$user_brush
    user_click <- input$user_click
    
    if(interaction_type == "brush") res <- brushedPoints(mtcars, user_brush)
    if(interaction_type == "click") res <- nearPoints(mtcars, user_click, threshold = 10, maxpoints = 1)
    
    return(res)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(dat()[,c("mpg", "cyl", "disp")]))
  
}


ui <- fluidPage(
  
  h3("Click or brush the plot and it will filter the table"),
  selectInput(inputId = 'x_col', 'X', choices = c('Choose'='', names(mtcars))),
  selectInput(inputId = 'y_col', 'Y', choices = c('Choose'='', names(mtcars))),
  plotOutput("plot", click = "user_click", brush = "user_brush"),
  DT::dataTableOutput("table")
  
)

shinyApp(ui = ui, server = server)