library(shinyFiles)
library(shiny)

ui <- fluidPage(
  shinyFilesButton("GetFile", "Choose a file" ,multiple=F,
                 title = "Please select a file:",
                 buttonType = "default", class = NULL),
  
  textOutput("txt_file")
)


server <- function(input,output,session){
  
  volumes = getVolumes()
  observe({
    
    shinyFileChoose(input, "GetFile", roots = volumes, filetypes = c('','csv'),
                   session = session)
    
 #   if(!is.null(input$GetFile)){
      # browser()
    print(str(input$GetFile))
      myInputFile <- parseFilePaths(volumes, input$GetFile)
      #listBands <- list.files(myInputDir1, full.names = T)
      output$txt_file <- renderPrint(myInputFile)
      file <- myInputFile$datapath
      
      if(!is.null(input$GetFile)){
        print(file)
        a <- read.csv(myInputFile$datapath)
      }
      #Call your function here..... 
 #   }
  })
}

shinyApp(ui = ui, server = server)