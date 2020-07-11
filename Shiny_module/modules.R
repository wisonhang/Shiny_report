chartTableBoxUI <- function(id, div_width = "col-xs-12 col-sm-6 col-md-4") {
  
  ns <- NS(id)
  
  div(class = div_width,
      tabBox(width = 12, title = id,
             tabPanel(icon("bar-chart"),
                      highchartOutput(ns("chart") )
             ),
             tabPanel(icon("table"),
                      DT::dataTableOutput(ns("table"))
             )
      )
  )
  
}


chartTableBox <- function(input, output, session, data, dem_group) {
  
  module_data <- reactive({
    data %>% filter(category == dem_group)
  })
  
  output$chart <- renderHighchart({
    
    hchart(module_data(), "column", hcaes(x = demographic, y = percent)) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%  
      hc_tooltip(valueDecimals = 1, valueSuffix = " %")
    
  })
  
  output$table <- renderDataTable({
    
    dt_data <- module_data() %>% 
      select(demographic, percent) %>% 
      mutate(percent = (percent / 100))
    
    DT::datatable(dt_data, 
                  style = "bootstrap",
                  class = "display",
                  options=list(scrollX=TRUE, dom = 't')) %>% 
      formatPercentage('percent', 0)
    
  })
  
}




library(lubridate)

DATE_RANGE=reactiveValues(start=floor_date(Sys.Date()-1,unit = 'month'),end=Sys.Date())
filter_ui<-function(id,div_width= "col-xs-12 col-sm-6 col-md-4"){
  ns<-NS(id)
  fluidRow(
    div(class=div_width ,
        checkboxGroupInput(ns('platform'),'Platform select',
                           c("Android","iOS","MiniProgram","Ali MiniAPP" ),
                           selected = c("Android","iOS","MiniProgram","Ali MiniAPP" ),
                           inline = T
        )
    ),
    div(class=div_width,
        radioButtons(ns('date_default'),'Period type',
                     c('FTD','Past 7 days','MTD','Last Month'),
                     selected = 'MTD',inline = T)
    ),
    div(class=div_width,    
        dateRangeInput(ns('date_range'),'Date period range select',
                       start=floor_date(Sys.Date()-1,unit = 'month'),end=Sys.Date(),
                       min='2019-06-01',
                       weekstart = 1
        )
    )
  )
}


filter_ui_select <- function(input,output,session)
{ns=session$ns
observeEvent(input$date_default,{
  day_type=input$date_default
  FTD=Sys.Date()-1
  
  start=switch (day_type,
                'FTD'=Sys.Date()-1,
                'Past 7 days'=Sys.Date()-8,
                'MTD'=floor_date(Sys.Date()-1,'month'),
                'Last Month'=floor_date(floor_date(Sys.Date()-1,'month')-1,'month')
  )
  end=switch (day_type,
              'FTD'=Sys.Date()-1,
              'Past 7 days'=Sys.Date()-1,
              'MTD'=Sys.Date()-1,
              'Last Month'=floor_date(Sys.Date()-1,'month')-1
  )
  
  DATE_RANGE$start=start
  DATE_RANGE$end=end
  
})
}


filter_ui_update<-function(input,output,session){
  ns <- session$ns
  observe({
    updateDateRangeInput(session,'date_range',
                         'Date period range select',
                         start=DATE_RANGE$start,end=DATE_RANGE$end,
                         min='2019-06-01'
    )
  })
}


downloadModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download iris as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download iris as csv2"),
                  easyClose = TRUE, title = "Download Table")
  )
}



downladTableUI<-function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
  
}



downloadDT<- function(input, output, session,data){
  ns<-session$ns
  print(class(input))
  #print(ns)
  moduledata<-reactive({
    data
  })
  
  
  output$table <- renderDT({
    temp=moduledata()
    sketch <- htmltools::tags$table(
      tableHeader(names(temp)),
      tableFooter(rep("", ncol(temp)))
    )
    ops=list(
      dom = 'Bfrtip',
      columnDefs = list(list(className = 'dt-center', targets='_all')),
      buttons = list(
        "copy",
        list(
          extend = "collection",
          text = 'download dataset',
          action = DT::JS(sprintf("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('%s', true, {priority: 'event'});
                            }",ns('test')))
        )
      ),
      scrollX=T
      # footerCallback = JS(
      #   "function(tfoot, data, start, end, display) {",
      #   "  var api = this.api();",
      #   "  for(var i=0; i<data[0].length; i++) {",
      #   "    $(api.column(i).footer()).html(",
      #   "      api.column(i, {page:'all'}).data().reduce(function(a,b){",
      #   "        if(isNaN(a)) return ''; return Math.round(a+b);",
      #   "      })",
      #   "    );",
      #   "  }",
      #   "}")
      
    )
    datatable(temp,
              style = "bootstrap",
              class = "display",
              rownames = F,
              container = sketch, 
              extensions = c('Buttons','Scroller'),
              option=ops

    )
  }
  )
  myModal <- function() {
    ns<-session$ns
    div(id ='test',
        modalDialog(downloadButton(ns("download1"),"Download data as csv"),
                    downloadButton(ns("download2"),"Download data as csv2"),
                    easyClose = TRUE, title = "Download Table")
    )
  }
  observeEvent(input$test, {
    showModal(myModal())
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(moduledata(), file)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(moduledata(), file)
    }
  )
}