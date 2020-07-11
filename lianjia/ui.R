library(DT)

shinyUI(pageWithSidebar(
  #library('leaflet'),
  headerPanel("上海链家二手房成交分析"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(   
    # make a non reactive text input with Update button
    h3('成交信息查询'),
    selectInput('year',strong("年份from:"),c('2015','2016','2017','2018','2019'),selected='2018'),
    selectInput('district',strong("数据区域:"),c('浦东','闵行','宝山','徐汇','普陀','杨浦',
                                           '长宁','松江','嘉定','黄浦','静安',
                                           '闸北','虹口','青浦','奉贤','金山','崇明','上海周边','全部'),selected='全部'),
    actionButton('set','生成(重置)数据集 ',icon=icon('play-circle')),
    
    br(),
    
    br(),
    selectInput('size','街道or小区:',c('街道','小区'),selected = '街道'),

    width=2
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel('二手房成交地图',
        leafletOutput('map',height = '800')),
      tabPanel('二手房-Pivot table',
               checkboxGroupInput('index',strong("纵向:"),c('区域','街道','户型','装修','朝向','月份'),
                                  inline=TRUE,
                                  selected='区域'),
               checkboxGroupInput('colum',strong("横向:"),c('区域','街道','户型','装修','朝向','月份'),
                                  inline=TRUE,
                                  selected='月份'),
               rpivotTableOutput('table1')),
      tabPanel('箱线图',
               column(width=3,
                      selectInput('x','x axis:',c('区域','街道','建筑年代','地铁'),selected='建筑年代')),
               column(width=3,
                      selectInput('y','y axis:',c('单价','总价'),selected = '单价')),
               column(width=3,
                      selectInput('col','col class is:',c('装修','电梯','年','月','null'),selected='电梯')),
               column(width=3,
                      selectInput('wrap','facet is:',c('装修','电梯','年','月','null'),selected='null')),
              plotOutput('boxplot',height = '800')
      ),
      tabPanel('回归分析',
               h3('样本数据统计'),
               verbatimTextOutput('regression'),
               h3('线性回归summary'),
               verbatimTextOutput('lm'),
               h3('Lasso 系数'),
               plotOutput('lasso')
               ),
      tabPanel('小区聚类',
               selectInput('kmean','聚类个数:',2:10,selected=4),
               helpText('程序运行中,请等待'),
               plotOutput('clust',width = '100%'),
               plotOutput('tree',width = '100%'),
               dataTableOutput('clustsummary'),
               br(),
               #tableOutput('clustcount'),
               dataTableOutput('xiaoquinfo')
               
               ),
      tabPanel('二手房成交信息',
               dataTableOutput("sold"))
      
      
    ))
))


