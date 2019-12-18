
loadFileUI <- function(id, label = 'choose csv file'){
  
  ns <- NS(id)
  
  tagList(
    tags$p(strong("Input File")),
    shinyFilesButton(id = ns("files"), 
                     label = "Choose File", 
                     title = "Please select a file", 
                     multiple = F,
                     icon = icon('folder-open')),
    checkboxInput(inputId = ns("heading"), 
                  label = "Has header?"),
    textInput(inputId = ns("na.string"), 
              label = "NA symbol", 
              value = "NA")

  )
}

loadFile <- function(input, output, session, volumes = volumes){
  
  
  # select and load a table -------------------------------------------------
  
  shinyFileChoose(input, "files", roots = volumes, filetypes=c('csv'))
  
  # Prints the filepath in the main 
  output$filepath <- renderPrint({
    req(input$files)
    parseFilePaths(volumes, input$files)$datapath
  })
  
  #Load file only if file was selected
  dat <- reactive({
    if (!is.integer(input$files[1])){
      
      read_csv(parseFilePaths(volumes, input$files)$datapath,
               col_names = input$heading,
               na = input$na.string)
    }
  })

}

