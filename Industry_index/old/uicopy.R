shinyUI(
  # pageWithSidebar(
  # 
  # headerPanel("Industry index board"),
  # 
  # #Sidebar with a slider input for number of observations
  # sidebarPanel(
  #   # make a non reactive text input with Update button
  #   br(),
  #   #selectInput('fund.compy',strong("基金公司:"),c("所有",as.character(selectdata$compy_id)),selected='华夏基金'),
  #   #verbatimTextOutput('select'),
  #   #selectInput('codes', '比较基金选择(可多选)', fundscodename, multiple=TRUE, selectize=TRUE),
  #   htmlOutput("status"),
  #   width=2
  # ),
  
  # Show a plot of the generated distribution
  # mainPanel(
  #   tabsetPanel(
  navbarPage(
    title = 'Industry index Dashboard',
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
               column(width=5,selectInput('code', '指数选择(单选)', indexname, multiple=FALSE, selectize=TRUE)),
               column(width=5,sliderInput('months','回溯月份选择',0,36,6))
             ),
             h3('历史K线图',align='center'),
             plotOutput('candleplot',height=600),
             br(),
             h3('月度收益表格',align='center'),
             plotOutput('monthr',height=200),
             fluidRow(width=12,
                      h3('成分股信息'),
                      dataTableOutput('stock'))
    ),
    tabPanel('Indexes comparsion')
  )
)


