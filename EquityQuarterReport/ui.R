library(DT)
library(shinymanager)

report_time<-rep(2005:2019,each=4)
report_time<-paste0(report_time,c('-03-31','-06-30','-09-30','-12-31'))
ui<-pageWithSidebar(

  headerPanel("A股年报数据概览"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(   
    # make a non reactive text input with Update button
    br(),
    h3('年报信息查询'),
    selectInput('time',strong("报告:"),report_time,selected='2019-06-30'),
    #selectInput('industry',strong('行业'),names(industry_code),selected='全行业'),
    selectInput('group',strong('分类'),names(group),'行业'),
    br(),
    # uiOutput('select'),
    selectInput('industry',strong('分类选择'),c(group$行业,'全部'),'全部'),
    br(),
    checkboxGroupInput('columns',strong('报告内容'),c('股票代码','股票简称','每股收益','扣非每股收益','营业收入','同比增长','环比增长',
                                                  '净利润','同比','环比','每股净资产','净资产收益率','每股经营现金流量',
                                                  '销售毛利率','利润分配','股息率','市盈率','公告日期','报告期','类别'),
                       c('股票代码','股票简称','营业收入','同比增长',
                         '净利润','同比','每股净资产','市盈率','公告日期')),
    htmlOutput("status"),
    width=2
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    downloadButton("downloadData", "Download Data")	,
    tabPanel('年报信息',
             dataTableOutput("report"))

  )
)
  


ui <- secure_app(ui,id = "auth",
                 tag_img = tags$img(
                   src = "https://i.postimg.cc/h4dk6HWH/OYO.png", width = 200
                 ),
                 tag_div = tags$div(
                   tags$p(
                     "Default account is your email address before @, and the password is 12345.
                      If you have no authentication, please contact ",
                     tags$a(
                       href = "david.hang@oyohotels.cn",
                       target="_top", "david.hang"
                     )
                   )
                 ),
                 enable_admin = T)
