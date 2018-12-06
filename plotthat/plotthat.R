library(shiny)

# This app will faciltate the use of msconvert (http://proteowizard.sourceforge.net/downloads.shtml).
# While the GUI has a lot of filters, it doesn't allow one to, for instance, select a polarity in case of MS data that was recorded with polarity switching.
#

#setwd("C:/Users/Daniel/Dropbox/projects/ShinyDB/shiny_msconvert")


ui <- fluidPage(
  titlePanel(h1("ShinyMSConvert\nMaking it easy to convert MS files to mzML, mzXML etc using the ProteoWizard's msconvert.exe"),
             h3("Go to http://proteowizard.sourceforge.net/downloads.shtml to download and install msconvert.exe before running the app")),
  
  sidebarLayout(
    sidebarPanel(

      fileInput("input_files", "Choose file to be loaded (currently only .csv)",
                accept = c( '.csv'),
                multiple = T),
      actionButton("updateButton", "Load")
    ),
    
    mainPanel(textOutput('msconvert'))
  )
)


# Server logic ----
server <- function(input, output, session) {
  
  
  observe({
    input$updateButton
    updateSelectInput(session, "text_output2", value = isolate(heads(input$input_files)))
  })
  
}

shinyApp(ui = ui, server = server)