library(shinyFiles)
library(shiny)

ui <- fluidPage(
  shinyDirButton("Btn_GetFolder", "Choose a folder" ,
                 title = "Please select a folder:",
                 buttonType = "default", class = NULL),
  
  textOutput("txt_file")
)


server <- function(input,output,session){
  
  volumes = getVolumes()
  observe({
    
    shinyDirChoose(input, "Btn_GetFolder", roots = volumes, 
                   session = session)
    
    if(!is.null(input$Btn_GetFolder)){
      # browser()
      myInputDir1 <- parseDirPath(volumes, input$Btn_GetFolder)
      listBands <- list.files(myInputDir1, full.names = T)
      output$txt_file <- renderText(listBands)
      
      #Call your function here..... 
    }
  })
}

shinyApp(ui = ui, server = server)