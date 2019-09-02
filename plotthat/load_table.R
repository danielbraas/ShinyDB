library(shiny)
library(shinyFiles)

server <- (function(input, output) {
  
  volumes <- c(wd = "C:/")
  wd <- getwd()
 
  
  shinyDirChoose(input, "directory", roots = volumes )
  
  # print folder information to console not browser
  observeEvent(input$directory, {

#    print(parseDirPath(volumes, input$directory))
    
    if (length(parseDirPath(volumes, input$directory)) != 0) {
      volumes <<- c(dbraas = as.character(
        parseDirPath(volumes, input$directory)))
      
      print(volumes)
    }
    
  })
  
  # print to folder to browser
 
  output$directorypath <- renderPrint({
    parseDirPath(volumes, input$directory)
  })
  

  shinyFileChoose(input, "files", roots = volumes)

  # print folder information to console not browser
  observeEvent(input$files, {
    
    print(parseFilePaths(volumes, input$files)$datapath)
    print(volumes)
    if (length(parseFilePaths(volumes, input$files)$datapath) != 0) {
      dat <- read.csv(parseFilePaths(volumes, input$files)$datapath)
  
    }
    
  })
  
  output$filepath <- renderPrint({
    parseFilePaths(volumes, input$files)$datapath
  })
  
  dat <- eventReactive(input$files,{
    read.csv(parseFilePaths(volumes, input$files)$datapath)
  })


  output$tab <- renderDataTable(dat())
  
})

ui <- (pageWithSidebar(
  headerPanel(
    "Select Data Table",
    "shinyFiles example"
  ),
  sidebarPanel(
    shinyDirButton("directory", "Choose Input", "Please select a folder"),
    #tags$p(),
    tags$p("Input Directory"),
    tags$hr(),
    shinyFilesButton("files", "Choose File", "Please select a folder", multiple = F)
    
  ),

#  sidebarPanel(
#    shinyFilesButton("file", "Choose File", "Please select a file")
#  ),
  
  mainPanel(
    tags$h4("Input Directory:"),
    verbatimTextOutput("directorypath"),
    tags$hr(),
    verbatimTextOutput("filepath"),
    dataTableOutput('tab')
  )
))

shinyApp(ui = ui, server = server)