
loadFileUI <- function(id, label = 'choose csv file'){
  
  ns <- NS(id)
  
  tagList(
    tags$p(strong("Input File")),
    shinyFilesButton(id = ns("files"), 
                     label = "Choose File", 
                     title = "Please select a file", 
                     multiple = F,
                     icon = icon('folder-open')),
    textOutput(ns('filepath')),
    checkboxInput(inputId = ns("heading"), 
                  label = "Has header?"),
    textInput(inputId = ns("na.string"), 
              label = "NA symbol", 
              value = "NA")
  )
}

loadFile <- function(input, output, session, volumes = volumes, file.type = 'rds'){
  
  
  # select and load a table -------------------------------------------------
  
  shinyFileChoose(input, "files", 
                  roots = volumes, 
                  filetypes = file.type)
  
  # Prints the filepath in the main 
  output$filepath <- renderPrint({
    req(input$files)
    file <- parseFilePaths(volumes, input$files)$datapath
    if (length(file) > 0) names(file) <- 'Chosen file:'   #only name file after a file is chosen
    file
  })
  
  #Load file only if file was selected
  dat <- reactive({
    if (!is.integer(input$files[1])){
      name <- parseFilePaths(volumes, input$files)$name
      
      if(grepl('csv|CSV', name)){
        read_csv(parseFilePaths(volumes, input$files)$datapath,
                 col_names = input$heading,
                 na = input$na.string)
      }
      
      if(grepl('rds|RDS', name)){
        read_rds(parseFilePaths(volumes, input$files)$datapath)
      }
    }
  })
 
}

