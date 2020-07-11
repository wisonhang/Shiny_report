library(plotly)
library(DT)
shinyUI(pageWithSidebar(
  
  headerPanel("Industry index board"),

  #Sidebar with a slider input for number of observations
  sidebarPanel(
    h5('Current index data update date:'),
    htmlOutput("status"),
    br(),
    h5('Click the Button below to update data (请在收盘后更新)  '),
    helpText('It may take a long time, wait until finished signal appear'),
    actionButton('update',strong('Update index data'),icon=icon('play-circle')),
    width=2
  ),

  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
    #navbarPage(
      #title = 'Industry index board',
      tabPanel('Industry index',
               h3('指数筛选标准选择'),
               fluidRow(
                 column(width=4,
                        selectInput('index.compare',strong("指数筛选4433法:"),c(TRUE,FALSE),selected=FALSE)),
                 checkboxGroupInput('index.rank',strong('指数排名'),c('近1周','近1月','近3月','近6月','近1年'),
                                    inline =TRUE,c('近1周','近1月','近3月','近6月','近1年'))
               ),
               fluidRow(
                 column(width=4,
                        sliderInput('近1周',strong('近1周排名前%'),0,100,value=c(0,40)),
                        sliderInput('近6月',strong('近6月排名前%'),0,100,value=c(0,25))
                 ),
                 column(width=4,
                        sliderInput('近1月',strong('近1月排名前%'),0,100,value=c(0,40)),
                        sliderInput('近1年',strong('近1年排名前%'),0,100,value=c(0,25))
                 ),
                 column(width=4,
                        sliderInput('近3月',strong('近3月排名前%'),0,100,value=c(0,25))
                 )
               ),
               
               fluidRow(width=12,
                        h3('指数收益率 %'),
                        dataTableOutput('openfund'))       
      ),
  tabPanel('Single Index Detail',
           fluidRow(
             column(width=4,selectInput('code', '指数选择(单选)', indexname, multiple=FALSE, selectize=TRUE))
             #column(width=4,sliderInput('months','回溯月份选择',0,36,6))
           ),
           h3('历史K线图',align='center'),
           plotlyOutput('candleplot',height=600),
           br(),
           h3('月度收益表格',align='center'),
           plotOutput('monthr',height=200),
           fluidRow(width=12,
           h3('成分股信息'),
           dataTableOutput('stock'))
  ),
  
  tabPanel('Return vs Volatility',
           #fluidRow(
           #   column(width=4,selectInput('type', '时间维度', c('YTD','Last','Hist'),selected = 'YTD'))
           #   #column(width=4,sliderInput('months','回溯月份选择',0,36,6))
           # ),
           plotlyOutput('rev_vol_bubble',height = 800),
           dataTableOutput('index_summary')

  ),
  tabPanel('Indexes comparsion',
           fluidRow(
           column(width=5,checkboxGroupInput('basic', '默认指数选择(多选)', choices=c('000001-上证指数','000016-上证50','000300-沪深300','000905-中证500',
                                                                              '399001-深证成指','399005-中小板指','399006-创业板指'),
                                               selected=c('000001-上证指数','000016-上证50','000300-沪深300','000905-中证500',
                                                          '399001-深证成指','399005-中小板指','399006-创业板指'),inline=TRUE)),
           column(width=4,selectInput('codes', '指数选择(多选)', indexname,
                                      multiple=TRUE, selectize=TRUE))
           ),
           fluidRow(
            column(width=10,sliderInput("date_range", 
                         "Choose Date Range:", 
                         min = as.Date("2015-01-01"), max = as.Date('2020-12-31'), 
                         value = c(as.Date("2016-01-01"), Sys.Date())
             ))
           ),
           plotOutput('indexplot'),
           h3('指数日均收益&波动率'),
           plotOutput('iaplot'),
           h3('指数收益概要%'),
           plotOutput('indexsummary')
  ),
  tabPanel('Index selected',
           fluidRow(
             column(width=5,sliderInput('corperiod','Select the period range',
                                        min = as.Date("2006-01-01"), max = as.Date('2020-12-31'), 
                                        value = c(as.Date("2006-01-01"), Sys.Date())
                                        )),
             column(width=5,numericInput('index_num','Select the num of index',10,
                    min=5,max=20)
           )),
           plotOutput('matplot'),
           plotOutput('corrplot'),
           plotOutput('index')
           
          )

#main panel
))
#shiny ui
))
