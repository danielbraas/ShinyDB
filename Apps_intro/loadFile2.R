ui <- shinyUI(bootstrapPage(
  shinyFilesButton('files', 'File select', 'Please select a file', FALSE),
  verbatimTextOutput('rawInputValue'),
  TextOutput('filepaths')
))
server <- shinyServer(function(input, output) {
  roots = c(wd='.')
  shinyFileChoose(input, 'files', roots=roots, filetypes=c('', 'txt', '[Rr]'))
  output$rawInputValue <- renderPrint({str(input$files)})
  output$filepaths <- renderPrint({parseFilePaths(roots, input$files)})
})

runApp(list(
  ui=ui,
  server=server
))