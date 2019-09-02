library(shiny); library(shinyFiles); library(tidyverse)

#Configuration variables. Eventually load these from a file.---
NASdir <- "T:/"
wd <- paste0(NASdir,"MS_Data/Data/Maintenance/PQ_PPM/")
script.dir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
datlocation <<- paste0(NASdir,"MS_Data/Data/Maintenancee/PQ_PPM/files/")
datFile<<- paste0(datlocation,"PQPPM_data.csv")
featfile <<- paste0(datlocation,"PQPPM_feat.csv")
datbackup <<- paste0(datlocation,"PQPPM_backup_",lubridate::today(),".csv")

plotColors <<-rev(rainbow(8))
ncolumns <<-2

autoqual <- 'C:/Program Files/Agilent/MassHunter/Workstation/Qual/B.08.00/Bin'
method <- paste0(script.dir,"files/")
source(paste0(script.dir,'files/copy_convert.R'))

#Load file
setwd(wd)
alldat <<- suppressWarnings(suppressMessages(read_csv(file = datFile)))

#UI-----
ui <- fluidPage(
      titlePanel(paste0("Delta_PPM Instrument Check")),
      mainPanel(
        tabsetPanel(type="tabs",
        
        tabPanel("Create Data",
              shinyFiles::shinyDirButton("dir","Choose .d file","Upload"), 
              verbatimTextOutput("dir", placeholder = TRUE),
              plotOutput("plot",width="1000px",height="800px"),
              dataTableOutput("table")),
          
        tabPanel("Browse Previous Data",
           sidebarLayout(
             sidebarPanel(
               width=4,
               uiOutput("dateRangeSelect"),
               selectizeInput("assay","Assay:",
                              unique(alldat$Assay),
                              selected=unique(alldat$Assay),
                              multiple=T),
               selectizeInput("instrument","Instrument:",
                              unique(alldat$Instrument),
                              selected=unique(alldat$Instrument),
                              multiple=T),
               #selectInput("column","Column:",unique(alldat$Column)),
               selectizeInput("esimode","ESI Mode:",
                              unique(alldat$ESImode),
                              selected=unique(alldat$ESImode)[1],
                              multiple=F),
               actionButton("go","Generate Plot")),
             mainPanel("Results",  
               plotOutput("plot2",width="1200px",height="1000px"),
               dataTableOutput("table2"))
             )
          )
        ))
    )

#server----
server <- function(input, output) {
#TAB 1: Reactive events culminating in output$plot and output$table -----
  shinyFiles::shinyDirChoose(input,'dir',roots=c(wd='.'))
  
  global <- reactiveValues(datapath = getwd())
  dir <- reactive(input$dir)
  output$dir <- renderText({global$datapath})
#Once file is selected...  
  observeEvent(ignoreNULL = TRUE,{input$dir},
    {req(is.list(input$dir)) #once folder is selected, record it as global$datapath
     home <- normalizePath("~")
     global$datapath <-file.path(wd, paste(unlist(dir()$path[-1]),collapse = .Platform$file.sep))
     
#Convert & copy file if not already present. ------
     IN <- dirname(global$datapath)
     files <- basename(global$datapath)
     OUT <- paste0(wd,"datafiles/")
     
     filesplit <- strsplit(files,split="_")
     
     esiFile <- case_when(grepl("neg",tolower(files)) ~ "ESIneg",
                          grepl("pos",tolower(files)) ~ "ESIpos")
     
     feat <- read.csv(featfile) %>% filter(
       tolower(Project)==tolower(filesplit[[1]][2]), #filter feat by project
       tolower(ESImode)==tolower(filesplit[[1]][5])) #and by ESI mode
     
if(!file.exists(paste0(OUT,files))){
  conv(IN, OUT, autoqual, method, files)}

mzfile <- gsub(files,pattern="\\.d$",replacement=".mzdata.xml")
mzfile <- paste0(OUT, mzfile)

#Parse MZ datafile to get 'results' for output$plot-------
source(paste0(script.dir,'files/extract_MS_Custom.R'))
results <- suppressWarnings(
  1:length(mzfile) %>% 
  map(~ extract_MS(feat, mzfile[.],50)) %>% 
  map_df(~., bind_rows) %>% 
  left_join(feat, ., by=c('UNIQUE.ID'='ID')) %>% 
  mutate(Sample = str_remove(File, '\\.mz(.)*$'),
         Sample = str_remove(Sample, OUT))) %>%
  filter(!is.na(MS1))

results <- select(results,ESImode,UNIQUE.ID,File,Rt,Intensity,mz,MS1,delta_ppm,MobilePhase.Additive,Formula) %>% 
  mutate(Name=paste0(UNIQUE.ID, " (", mz,")"),
        Directory=dirname(File),Filename=basename(File),
        Intensity=round(Intensity,1),mz=round(mz,4),MS1=round(MS1,4),
        Rt=round(Rt,1),delta_ppm=round(delta_ppm,2))%>% 
  separate(Filename,
           into=c("MaintType","Assay","Instrument","Column","ESImode","Rate","Date","Extension"),
           sep ="_|.mzdata.") %>%
  mutate(Rate=as.numeric(gsub(x=Rate,pattern="p",replacement="\\.")),
        Date=gsub(x=Date,pattern="-",replacement="")) %>%
  mutate(Date=as.numeric(Date)) %>%
  select(Assay,MobilePhase.Additive,Instrument:Date,Name,UNIQUE.ID,Formula,Rt:delta_ppm) %>%
  filter(Intensity>=1000,Rt>=30) %>%
  arrange(UNIQUE.ID,as.numeric(mz),Rt)

minIntensity <- min(log(results$Intensity,10))
maxIntensity <- max(log(results$Intensity,10))

#Saves to its own file in the file dir, as well as the main PQPPM_data file.
saveRDS(results,file=paste0(OUT,files,".csv"))

old.dat <- suppressMessages(suppressWarnings(read_csv(datFile)))

write_csv(old.dat,datbackup)
merged <- suppressMessages(suppressWarnings(bind_rows(old.dat,results)))%>% 
  arrange(Date,Assay,Instrument,UNIQUE.ID,as.numeric(mz),Rt)
write_csv(merged, datFile)
message("PQ_PPM data was added to the database.")
print(paste0("Merged file length is",nrow(merged)))
print(paste0("Merged file has ",sum(is.na(merged$Date))," NA values for Date."))

#Produce summary output$table---------
output$table <- renderDataTable(

  suppressWarnings(results %>% 
    group_by(Name,.drop=F) %>% 
    mutate(LineGraph.Intensity = skimr::inline_linegraph(Intensity)) %>%
    #mutate(Hist.Intensity = skimr::inline_hist(Intensity)) %>%
    mutate(LineGraph.DeltaPPM = skimr::inline_linegraph(delta_ppm)) %>%
    #mutate(Hist.DeltaPPM = skimr::inline_hist(delta_ppm)) %>%
    mutate(Mean.Intensity=mean(Intensity),
           p10.Intensity = quantile(Intensity, probs=0.1),
           p90.Intensity = quantile(Intensity, probs=0.9),
           Mean.DeltaPPM=mean(delta_ppm)) %>%
    select(Name,Mean.Intensity,p10.Intensity,p90.Intensity,LineGraph.Intensity,Mean.DeltaPPM,LineGraph.DeltaPPM) %>%
    mutate_at(c(2,3,4,6),as.numeric) %>%  
    mutate_at(2:4,round,0) %>% 
    mutate_at(6,round,1) %>%
    distinct(Name,.keep_all = T)) 
  )
  
#Generate output$plot (PPM error plots x intensity)--------
plotColors <-rev(rainbow(8))# rev(c("#EBBFE6","#C7AAE8","#949FDB","#6198B8","#3C8E83","#2C794A","#2D5A1E","#2E3609"))
plotBreaks <- seq(minIntensity,maxIntensity,length.out = 5)
RtBreaks <- round(c(  seq(min(round(results$Rt,-1)),
                    max(round(results$Rt,-1)),length.out = 5)),-1)
  
plotLimits <- c(min(plotBreaks),max(plotBreaks))
antilog <- as.integer(10^plotBreaks)
antilog <- if (antilog[1]<10000) {round(as.integer(10^plotBreaks),-3)} else {round(as.integer(10^plotBreaks),-4)}

output$plot <- renderPlot({
  suppressWarnings(ggplot(data=results,aes(x=Rt,y=delta_ppm,color=log(Intensity,10)))+
  geom_point(pch=21,size=3,color="#726B68")+  
  geom_point(shape=16,size=2.5,stroke=0.1)+
  scale_colour_gradientn(colours = plotColors,
                         breaks = plotBreaks, 
                         labels=round(plotBreaks,1),#antilog,
                         limits= plotLimits,
                          )+
  scale_x_continuous("RT (seconds)",breaks=RtBreaks,labels=RtBreaks)+
  theme(plot.subtitle = element_text(vjust = 1,size=12), 
        plot.caption = element_text(vjust = 1,size=12), 
        axis.text = element_text(size = 12),
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill = NA),
        strip.text.x = element_text(size = 15,face="bold"),
        legend.text=element_text(size=14)) +
  geom_hline(yintercept=0,color="#483D41",linetype="solid")+
  geom_hline(yintercept=-20,color="red",linetype="dashed")+
  geom_hline(yintercept=20,color="red",linetype="dashed")+
  labs(title = paste0("Delta_ppm vs RT, ",unique(results$ESImode)[1]," Mode, colored by intensity"), 
       colour = "log10(Intensity) ",y="Delta_ppm")+
  ylim(-22,22)+ facet_wrap(~Name,ncol=ncolumns,scales="free_x"))
    })
})

#TAB 2: Generate output$table2 and output$plot2 (PPM error plots x date within range)--------
 
  output$dateRangeSelect <- renderUI({tagList(dateRangeInput(inputId = "dateRangeSelect",label = "Date Range:"))})
  getDateRange <- reactive({
    input$go  #upon selection of the Go! button load the variables...
    isolate({dateRange <- list(input$dateRangeSelect[1], input$dateRangeSelect[2])})
    return(dateRange)
  }) #setup date range to correct format
  
  observe({
    req(input$dateRangeSelect)
    req(input$go)

    alldat <<- suppressWarnings(suppressMessages(read_csv(file = datFile))) #reload
    
    #Get the date range from selectizors
    dateRange <- getDateRange()
    daterange <<- c(as.Date(as.numeric(dateRange[1]),origin="1970-01-01"),
                   as.Date(as.numeric(dateRange[2]),origin="1970-01-01"))
    
    #Converts from numeric to date, then filters from input$'s
    stringi::stri_sub(alldat$Date,5,4) <- "-"
    stringi::stri_sub(alldat$Date,8,7) <- "-"
    alldat$Date <- as.Date(alldat$Date,"%Y-%m-%d")
    
    alldat <- alldat %>%
        filter(Date>=dateRange[1] & Date <=dateRange[2],
               ESImode %in% input$esimode,
               Assay %in% input$assay,
               Instrument %in% input$instrument,
               Intensity>=5000) #filtered to remove noise/outliers
               #abs(delta_ppm)<28)
    
#DATA TABLE & SETUP -----
    alldattable <- suppressWarnings(group_by(alldat,Assay,Instrument,ESImode,Name,Date) %>%
      mutate(LineGraph.DeltaPPM = skimr::inline_linegraph(delta_ppm),
             Hist.DeltaPPM = skimr::inline_hist(delta_ppm),
             Median.Intensity=median(Intensity),
             min.dPPM = min(delta_ppm,na.rm=F),
             p25.dPPM = quantile(delta_ppm, probs=0.25),
             Median.dPPM=median(delta_ppm),
             wMedian.dPPM=spatstat::weighted.median(delta_ppm,Intensity,na.rm=T),
             p75.dPPM = quantile(delta_ppm, probs=0.75),
             max.dPPM=max(delta_ppm,na.rm=F),
      ) %>% 
      ungroup() %>% distinct(Assay,Instrument,ESImode,Name,Date,.keep_all = T))
    alldattable <- suppressWarnings(group_by(alldat,Assay,Instrument,ESImode,Name,Date) %>%
      summarise(sd=sd(delta_ppm),median=median(delta_ppm),
                mean=mean(delta_ppm),CV=sd/mean) %>% ungroup() %>%
      left_join(.,alldattable,by=c("Assay","Instrument","ESImode","Name","Date")) %>%
      select(Assay,Instrument,ESImode,Name,Date,CV,min.dPPM,p25.dPPM,wMedian.dPPM,p75.dPPM,max.dPPM,Hist.DeltaPPM,Median.Intensity) %>%
      mutate_at(c(6:11,13),as.numeric) %>%  
      mutate_at(13,round,0) %>% mutate_at(c(6:11),round,1))
    
    alldat <- suppressWarnings(left_join(alldat,alldattable,by=c("Assay","Instrument","Name","ESImode","Date")))
    alldattable <- suppressWarnings(select(alldattable,-min.dPPM,-max.dPPM))
    
    output$table2 <- renderDataTable(alldattable)
    
    RtBreaks <- round(c(  seq(min(round(alldat$Rt,-1)),  #Load up the plots,
                              max(round(alldat$Rt,-1)),length.out = 5)),-1)
    
    #PLOT2--------
    output$plot2 <- renderPlot({suppressWarnings(
        ggplot(data=alldat,
               aes(x=Date,y=delta_ppm,label=wMedian.dPPM,
                   group=interaction(Date,Instrument),color=log(Intensity,10)))+
          geom_point(aes(group=interaction(Date,Instrument)),
                     position=position_jitterdodge())+
          geom_boxplot(aes(fill=Instrument),
                       alpha=0.9,outlier.shape=NA,varwidth=F)+
          theme(plot.subtitle = element_text(vjust = 1,size=12), 
                plot.caption = element_text(vjust = 1,size=12), 
                axis.text = element_text(size = 12),
                axis.title=element_text(size=14,face="bold"),
                panel.background = element_rect(fill = NA),
                strip.text.x = element_text(size = 12,face="bold"),
                legend.text=element_text(size=12))+#,legend.position="bottom")+
          geom_hline(yintercept=0,color="#483D41",linetype="solid")+
          geom_hline(yintercept=-20,color="red",linetype="dashed")+
          geom_hline(yintercept=20,color="red",linetype="dashed")+
          labs(title = paste0("Delta_ppm vs RT, ",unique(alldat$ESImode)[1]," Mode, colored by intensity"), 
               colour = "Intensity (log10)",y="delta ppm error")+
          scale_color_gradientn(colours = plotColors,
                                breaks = plotBreaks, 
                                labels=round(plotBreaks,1),#antilog,
                                limits= plotLimits)+
          scale_fill_manual(values=RColorBrewer::brewer.pal(length(unique(alldat$Instrument)),"Set3"))+
          facet_wrap(~Name,ncol=ncolumns,scales="free_x")+ylim(-27,27))
    })
    
  })



}

# Run the application
shinyApp(ui = ui, server = server)