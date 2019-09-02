library(shiny)
library(shinyFiles)
library(tidyverse)

server <- function(input, output, session) {
  
  volumes = getVolumes()
  
  #data <- shinyFileChoose(input, 'PWIZ', roots = volumes, session=session)
  
  observeEvent(input$PWIZ, {
    shinyFileChoose(input, 'PWIZ', roots = volumes, session=session)
    if(!is.null(input$PWIZ)){
      x <<- parseFilePaths(volumes, input$PWIZ)
     output$pwiz <- renderText(x$datapath)
     invisible(pwiz)
    }
  })
  
  observe({
    
    shinyDirChoose(input, "Btn_GetFolder", roots = volumes, session = 
                     session)
    if(!is.null(input$Btn_GetFolder)){
      myInputDir1 <- parseDirPath(volumes, input$Btn_GetFolder)
      listBands <- list.files(myInputDir1, full.names = T)
      output$txt_file <- renderText(listBands)
      output$volume <- renderText(myInputDir1)
      print(parseFilePaths(volumes, data))
      #Call your function here..... 
    }
    
    #shell(paste(x,'msconvert.exe', sep='/'))
    
  })
}

ui <- fluidPage(
  p(shinyFilesButton(id = 'PWIZ', 
                     label = "choose path to msconvert", 
                     multiple = F,
                     title='MSconvert.exe:', 
                     buttonType = 'btn btn-primary')),
  
  h3(textOutput('PWIZ')),
  
  #p(actionButton('load_button','Load')),
  
  p(shinyDirButton(id = "Btn_GetFolder", 
                   label = "Choose a folder" ,
                   title = "Please select a folder:",
                   buttonType = "btn btn-success", 
                   class = NULL)),
  
  
  textOutput("txt_file"),
  h2(textOutput("volume")),
  
  h4(textOutput('struc')),   
  tableOutput('tab')
  
)

shinyApp(ui = ui, server = server)