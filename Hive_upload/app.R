#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize=500*1024^2)
##连Hive
library("RJDBC")

drv_impala <- JDBC(
  'org.apache.hive.jdbc.HiveDriver',
  list.files("/usr/hive/lib", pattern = "jar$", full.names = TRUE, recursive = TRUE)
)




library(DBI)
library(readxl)
library(dplyr)
library(DT)


library(shiny)
library(progress)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Data upload"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #radioButtons('db_name','select database',c('Hive','Oracle'),selected='Hive'),
      fileInput('files','Upload data file'),
      uiOutput('filesheet'),
      textInput('filename','Input the table name',value='gr_user_cf_media_td_cost'),
      actionButton('run','click to write'),
      width=3
    ),
    
    mainPanel(
      # textOutput('filesheet')
      dataTableOutput('filedata')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$filesheet<-renderUI({
    req(input$files)
    A=input$files$datapath
    sheet_name=readxl::excel_sheets(A)
    selectInput('sheet','select sheet name',
                sheet_name,sheet_name[1]
    )
    
  })
  
  filedata<-reactive({
    data=NULL
    A=input$files$datapath
    #print(A)
    if( !is.null(A)){
      data=read_excel(A,sheet=input$sheet,col_types =c(rep('guess',10),'numeric','numeric') )
      
      colnames(data)=gsub(' ','_',colnames(data))
      if('total_download' %in% colnames(data) & input$filename=='gr_user_cf_media_td_cost'){
        data$total_download=as.numeric(data$total_download)
        data$paid_download=as.numeric(data$paid_download)
        data=data%>%arrange(desc(paid_download))
        
      }
    }
    
    data
  })
  
  
  observeEvent(input$run,{
    source('/home/dwhang/R/DataConnect.R')
    data=filedata()
    req(input$filename)
    p=Progress$new()
    data$etl_time=Sys.time()
    p$set(value = 1, message = "writing data to hive database...")
    RHive_WriteTable(con_impala,input$filename,data,overwrite = T)
    p$close()
    
  })
  
  output$filedata<-renderDataTable({
    data=filedata()
    if(!is.null(data))
      head(data)%>%datatable()
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
