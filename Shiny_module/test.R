# library(shiny)
# library(DT)
# library(lubridate)
# 
# DATE_RANGE=reactiveValues(start=floor_date(Sys.Date()-1,unit = 'month'),end=Sys.Date())
# plat_ui<-function(id,div_width= "col-xs-12 col-sm-6 col-md-4"){
#   ns<-NS(id)
#   fluidRow(
#     div(class=div_width ,
#         checkboxGroupInput(ns('platform'),'Platform select',
#                            c("Android","iOS","MiniProgram","Ali MiniAPP" ),
#                            selected = c("Android","iOS","MiniProgram","Ali MiniAPP" ),
#                            inline = T
#         )
#     ),
#     div(class=div_width,
#         radioButtons(ns('date_default'),'Period type',
#                      c('FTD','Past 7 days','MTD','Last Month'),
#                      selected = 'MTD',inline = T)
#     ),
#     div(class=div_width,    
#         dateRangeInput(ns('date_range'),'Date period range select',
#                        start=floor_date(Sys.Date()-1,unit = 'month'),end=Sys.Date(),
#                        min='2019-06-01',
#                        weekstart = 1
#         )
#     )
#   )
# }
# 
# 
# 
# test <- function(input,output,session)
# {  ns=session$ns
# observeEvent(input$date_default,{
#   day_type=input$date_default
#   FTD=Sys.Date()-1
#   
#   start=switch (day_type,
#                 'FTD'=Sys.Date()-1,
#                 'Past 7 days'=Sys.Date()-8,
#                 'MTD'=floor_date(Sys.Date()-1,'month'),
#                 'Last Month'=floor_date(floor_date(Sys.Date()-1,'month')-1,'month')
#   )
#   end=switch (day_type,
#               'FTD'=Sys.Date()-1,
#               'Past 7 days'=Sys.Date()-1,
#               'MTD'=Sys.Date()-1,
#               'Last Month'=floor_date(Sys.Date()-1,'month')-1
#   )
#   
#   DATE_RANGE$start=start
#   DATE_RANGE$end=end
#   
# })
# }
# 
# 
# test_update<-function(input,output,session){
#   ns <- session$ns
#   observe({
#     updateDateRangeInput(session,'date_range',
#                          'Date period range select',
#                          start=DATE_RANGE$start,end=DATE_RANGE$end,
#                          min='2019-06-01'
#     )
#   })
# }
# ui<- dashboardPage(
#   dashboardHeader(title = "DAU Channel Performance"),
#   dashboardSidebar(collapsed = TRUE
#   ),
#   dashboardBody(
#     tags$head(tags$style(HTML('
#             body {
#              -moz-transform: 80%; /* Moz-browsers */
#              zoom: 80%; /* Other non-webkit browsers */
#              zoom: 80%; /* Webkit browsers */
#              }
#             .content-wrapper {
#              background-color: white ;
#              }
#            .main-sidebar {
#              background-color: white ;}
#            .skin-blue .main-sidebar {
#             background-color:  white;}
#         '))),
#     #######tabset####
#     tabBox(width=12,
#            tabPanel(icon('table'),
#                     h2('DAU split by DAU Source',style='weight:bold'),
#                     plat_ui('DAU')
#            ),
#            tabPanel(icon('table'),
#                     h2('DAU split by Member Source',style='weight:bold'),
#                     plat_ui('DAU_T')
#            )
#            
#     )
#     
#   )
# )
# 
# shinyApp(
#   ui,
#   server = function(input,output,session) 
#   { 
#     callModule(test,'DAU')
#     callModule(test_update,'DAU')
#     callModule(test,'DAU_T')
#     callModule(test_update,'DAU_T')
#     #print(input)
#   }
#   
# )












############################

# library(shiny)
# library(DT)
# 
# 
# 
# downladTableUI<-function(id){
#   ns <- NS(id)
#   DT::dataTableOutput(ns("table"))
#  
# }
# 
# 
# 
# downloadDT<- function(input, output, session,data){
#   ns<-session$ns
#   print(class(input))
#   #print(ns)
#   moduledata<-reactive({
#     data
#   })
#   output$table <- renderDT(
#     datatable(moduledata(),
#               extensions = 'Buttons',
#               options = list(
#                 dom = 'Bfrtip',
#                 buttons = list(
#                   "copy",
#                   list(
#                     extend = "collection",
#                     text = 'download dataset',
#                     action = DT::JS(sprintf("function ( e, dt, node, config ) {
#                                     Shiny.setInputValue('%s', true, {priority: 'event'});
#                             }",ns('test')))
#                   )
#                 )
#               )
#     )
#   )
#   myModal <- function() {
#     ns<-session$ns
#     div(id ='test',
#         modalDialog(downloadButton(ns("download1"),"Download data as csv"),
#                     downloadButton(ns("download2"),"Download data as csv2"),
#                     easyClose = TRUE, title = "Download Table")
#     )
#   }
#   observeEvent(input$test, {
#     showModal(myModal())
#   })
# 
#   
#   output$download1 <- downloadHandler(
#     filename = function() {
#       paste("data-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(moduledata(), file)
#     }
#   )
#   
#   output$download2 <- downloadHandler(
#     filename = function() {
#       paste("data-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv2(moduledata(), file)
#     }
#   )
# }
# 
# 
# ui <- basicPage(
#   selectInput('rar','aa',c('a','b'),'a'),
#   tabBox(width=12,
#     tabPanel(icon("bar-chart"),downladTableUI("iris")
#              ),
#     tabPanel(icon("bar-chart"),downladTableUI("cars"))
#   )
#   
# )
# 
# server<-function(input,output,session){
#   callModule(downloadDT,'iris',iris)
#   callModule(downloadDT,'cars',cars)
# }
# 
# shinyApp(ui, server)


library(shiny)
library(DT)

## module UI
test_data_table_ui  <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(outputId = ns("my_data_table"))
  )
}

## module server
test_data_table_server <- function(input, output, session ){
  ns = session$ns
  
  myValue <- reactiveValues(check = '')
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  
  my_data_table <- reactive({
    tibble::tibble(
      Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
      Motivation = c(62, 73, 3, 99, 52),
      Actions = shinyInput(downloadButton, 
                           5,
                           'button_',
                           ns = ns,
                           label = "Download",
                           onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))
      )
    )
  })
  
  
  lapply(1:5, function(i){
    output[[paste0("button_",i)]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        readr::write_delim(x = iris, path = file, delim = "\t")
      }
    )
  })
  
  observeEvent(input$select_button, {
    print(input$select_button)
  })
  
  
  output$my_data_table <- DT::renderDataTable({
    datatable(my_data_table(), escape = FALSE, 
              options = 
                list(
                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                )
    )
  })
}

ui <- fluidPage(
  test_data_table_ui(id = "test_dt_inside_module")
)

server <- function(input, output, session) {
  callModule(module = test_data_table_server , id = "test_dt_inside_module")
}

shinyApp(ui, server)