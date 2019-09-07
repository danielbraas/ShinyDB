library(tidyverse)
library(shiny)
library(shinyFiles)


make_mzXML <- function(files, pol, msconvert){
    
    wd <- str_extract_all(files, '^(.)*/')
    dir.create(paste0(wd, pol))
    setwd(wd)
    
    #convert the files
    for (i in 1:length(files)){
        shell(paste(msconvert, 
                    "--mzXML --filter \"peakPicking true 1-\" --filter \"polarity positive\" ", 
                    files[i]))
    }
    #move the files to the respective folder
    file.rename(list.files(pattern='\\.mz'), paste(pol, list.files(pattern='\\.mz'), sep = '/'))
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
                        choices = c('Negative', 'Positive', 'Both', 'Default'))
#            shinyDirButton(id = 'out',
#                           label = 'Target directory',
#                           title = 'Please chose directory for files to be converted to')
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            p("MSConvert.exe:"),
            verbatimTextOutput('MSConvert')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
