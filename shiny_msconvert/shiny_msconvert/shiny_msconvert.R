library(tidyverse)
library(shiny)
library(shinyFiles)


make_mzXML <- function(files, pol, msconvert, out){
    
    files <- files()
    msconvert <- msconvert()
    out <- paste('-o', out())
    print(out)

    #convert the files
    for (i in 1:length(files)){
        shell(paste(msconvert,
                    files[i],
                    '--mzXML --filter "peakPicking true 1-" --filter', 
                    pol,
                    out))
        # '-o C:/Users/Daniel/Dropbox/R_projects/test'
        # pol <- '\"polarity positive\"'
    }
    #move the files to the respective folder
    #file.rename(list.files(pattern='\\.mz'), paste(pol, list.files(pattern='\\.mz'), sep = '/'))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Convert .raw files to mzXML"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            shinyFilesButton(id = 'msconvert',
                             label = 'MSConvert.exe',
                             title = 'Please point to the MSConvert.exe file',
                             multiple = F),
            hr(),
            shinyFilesButton(id = 'files',
                             label = 'Choose files',
                             title = 'Please files to be converted',
                             multiple = T),
            hr(),
            selectInput(inputId = 'pol',
                        label = 'Polarity',
                        choices = c('Negative', 'Positive', 'Both', 'Default')),
            
            shinyDirButton(id = 'out',
                           label = 'Target directory',
                           title = 'Please chose directory for files to be converted to'),
            
            actionButton(inputId = 'go',
                         label = 'Start')
            
        ),

        mainPanel(
            p("MSConvert.exe:"),
            verbatimTextOutput('MSConvert'),
            p('Selected files:'),
            verbatimTextOutput('Files'),
            p('Target directory:'),
            verbatimTextOutput('OUT')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #define what hard drive(s) can be accessed
    
    roots = c(wd = 'C:/Users/Daniel/Dropbox/R_projects/test',
              msconvert = 'C:/pwiz')
    
    shinyFileChoose(input = input,
                    id = 'msconvert',
                    roots = roots, 
                    filetypes = 'exe')
    
    msconvert <- reactive({
        req(input$msconvert)
        parseFilePaths(roots = roots, input$msconvert)$datapath
    })
    output$MSConvert <- renderPrint({
        req(input$msconvert)
        parseFilePaths(roots = roots, input$msconvert)$datapath
    })
    
    shinyFileChoose(input = input,
                    id = 'files',
                    roots = roots, 
                    filetypes = 'raw')
    
    files <- reactive({
        req(input$files)
        parseFilePaths(roots = roots, input$files)$datapath
    })
    
    output$Files <- renderPrint({
        req(input$files)
        parseFilePaths(roots = roots, input$files)$datapath
    })

    
    shinyDirChoose(input = input,
                   id = 'out',
                   roots = roots)
    out <- reactive({
        req(input$out)
        parseDirPath(roots = roots, input$out)
    })
    
    output$OUT <- renderPrint({
        req(input$out)
        parseDirPath(roots = roots, input$out)
    })
    
    observeEvent(input$go, {
        req(input$files, input$msconvert, input$pol)
        if (input$pol == 'Negative') {
            make_mzXML(files, pol='\"polarity negative\"', msconvert, out)
        } else if (input$pol == 'positive') {
            make_mzXML(files(), pol='\"polarity positive\"', msconvert(), out)
        }
        
#
#               Both = make_mzXML(input$files, pol='\"polarity negative\"', input$msconvert, input$out), 
#               Default = make_mzXML(input$files, pol='\"polarity negative\"', input$msconvert, input$out))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)


## To start this app from the R command line type:
## shell("R -e \"shiny::runApp(\'load_table5.R')\"")