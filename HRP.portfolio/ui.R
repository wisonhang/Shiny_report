# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  headerPanel("HRP Risk Parity demo"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(   
    tags$label('Initializing Assets pool (data from Tushare)'),
    textAreaInput('symbols','Asset symbols input',"Equity:510050,510300,159915,000006,159923,159938,159939,159940,512880;
Commodity:000819,399979,518880,;
Bond:000832,511010,120601",rows=5, cols=50),
    # h5('Note:Only Equity,Commodity,Bond and Cash Big Assets are allowed,
    #    please split each Assets using the semicolon(;)'),
     helpText("'中证消费':'159928',
                '中证军工':'512660',
                  '中证100':'159923',
                  '中小板指':'159902',
                  '证券公司':'512880',
                  '有色金属-指数':'000819',
                  '大宗商品':'399979',
                  '小康指数':'510160',
                  '细分医药':'512120',
                  '深证红利':'159905',
                  '深证成指':'159943',
                  '上证医药':'510660',
                  '上证消费':'510630',
                  '上证50':'510050',
                  '上海国企':'510810',
                  '全指医药':'159938',
                  '全指信息':'159939',
                  '全指金融':'159940',
                  '沪质城投':'511220',
                  '沪深300':'510300',
                  '地产指数-指数':'000006',
                  '创业板指':'159915',
                  '创业板50':'159949',
                  '5年国债':'511010',
                  '500医药':'512300',
                  '300非银':'512070',
                  '180金融':'510230',
                  '10年国债-指数':'000012',
                  '黄金':'518880',
                  '中证转债':'000832',
                  '06冀建投':'120602',
                  '06大唐债':'120601'
                  "),
    fileInput('files','Upload the data files(csv)',multiple = T),
    helpText('You can upload your own data to the assets pool,it allows multiple files,just note that the file name should be the 
             same as the symbol you type in the text above.'),
    #selectInput('Update',strong('Using the updated data ?(If True,it may take a few seconds)'),choices=c(TRUE,FALSE),selected=FALSE),
    #selectInput('Update',strong('Using the updated data ?(If True,it may take a few seconds)'),choices=c(TRUE,FALSE),selected=FALSE),
    h4('Click the Button below  '),
    actionButton('btu',strong('Update Assets pool'),icon=icon('play-circle')),
    
    uiOutput("Assets pool"),
    selectInput('rlen',strong('Return and risk lenth used for correlation :'),choices=c('years','months','weeks','days'),selected='weeks'),
    sliderInput("yr", strong("Years of data used for backtest:"), 2005, 2020, value = c(2013, 2018)),
    actionButton('btn', 'Finish selecting assets,reset the assets', icon=icon('play-circle')),
    h5('After selecting the assets, click the Run ActioButton, the assets pool will reset'),
    br(),
    h3('Backtest Parameter'),
    selectInput('Periodicity',strong('Trade Periodicity is :'),choices=c('months','weeks','days'),selected='weeks'),
    helpText('It is the trade perioicity, months means trade monthly,
             especially at the end of the month'),
    numericInput('lookback',strong('Lookback days length is:'),60,step=1),
    helpText('252 means for each transaction, 252 days data are used to lookback'),
    selectInput('estimate.len',strong('Estiamte return and volatility periodicity for lookback data'),choices=c('months','weeks','days'),selected='weeks'),
     helpText('It is the lookback estimate len for return and volatiliy,
              months means expeacted return and cov is monthly'),
    actionButton('modelrun', 'Finish set parameter,Run the backtest', icon=icon('play-circle')),
    br(),
    h3('Model performance'),
    selectInput('allocation',strong('Portfolio construction method:'),choices=c('MVTF','MV','RV','RVTF','HRP','HRPTF'),selected='HRF'),
    sliderInput("year", strong("Years of transition map:"), 2005, 2020, value = c(2013, 2017)),
    numericInput('num',strong('Number of last trades'),40,min=0,max=100,step=5),
    htmlOutput("status"),
    width=3
    ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel('Efficient Fronter',
               fluidRow(
                 column(width=12,
                        h3('Return and Covariance Table',align='center'),
                        plotOutput('retcor'))
               ),
               fluidRow(
                 column(width=6,
                        h3('Normal-Corrplot',align='center'),
                        plotOutput('cor1',height="400px")),
                        #div(style = "height:200px")),
                 column(width=6,
                        h3('Clusted-Corrplot',align='center'),
                        plotOutput('cor2',height="400px"))
                        #div(style = "height:200px"))
               ),
               fluidRow(
                 column(width=12,
                        plotOutput('ggplot1',height=200)),
                 column(width=12,
                        h3('Total Average'),
                        plotOutput('ggplot2',height=200))
               ),
               fluidRow(
                 column(width=12,
                        h3('Equity'),
                        plotOutput('ggplot3',height=200)),
                 column(width=12,
                        h3('Commodity'),
                        plotOutput('ggplot4',height=200))
               ),
               fluidRow(
                 column(width=12,
                        h3('Bond'),
                        plotOutput('ggplot5',height=200))
               )
               
      
               ),
      tabPanel('Portfolio Allocation Page',
               fluidRow(
                 column(width=12,
                        h3('Assets return',align="center"),
                        plotOutput('scalereturn'),
                        div(style = "height:300px"))
               ),
               
               fluidRow(
                 column(width=12,
                        h3('Models compare',align='center'),
                        plotOutput('models1'),
                        div(style = "height:600px"))
               ),
               fluidRow(
                 column(width=12,
                        h3('Strategy.performance.snapshoot',align='center'),
                        plotOutput('snapshoot'),
                        div(style = "height:200px"))
               ),
               fluidRow(
                 column(width=12,
                        h3('Custom.report',align='center'),
                        plotOutput('custom.report'),
                        div(style = "height:700px"))
               ),
               fluidRow(
                 column(width=12,
                        h3('Trade Summary',align='center'),
                        plotOutput('trades'),
                        div(style = "height:700px"))
               )
               
      ),
      tabPanel('Backtest code',
               p('This application demonstrates a demo of Hierarchical risk parity portfolio allocation',
                 a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework and',
                 a("Systematic Investor Toolbox", href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/", target="_blank")),
                 a('Hierarchical Risk Parity algorithm',href='https://quantstrattrader.wordpress.com/2017/05/26/testing-the-hierarchical-risk-parity-algorithm/'),
               
               br(),
               
               strong('Author'), p('Wison hang', a('KRM group', href="120.92.148.52/krm/risk.parity.demo", target="_blank")),
               
               br(),
               
               strong('Code'), p('Original source code for portfolio.alocation.helper for this application at',
                                 a('GitHub', href='https://github.com/systematicinvestor/SIT/blob/master/R/strategy.r')),
               
               br(),
               
               strong('Core code'),
               pre('
getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}
                   
getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}
                   
getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}
                   
recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <- w[cItems0] * alpha
  w[cItems1] <- w[cItems1] * (1-alpha)
  
  if(length(cItems0) > 1) {
  w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
  w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}'),
               pre("
min.var.portfolio.TF<-function(ia,constrants){
 moms <- Return.cumulative(ia$hist.returns)
 highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
 posReturnAssets <- moms > 0 # positive momentum assets
 selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
 selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
 
 if(sum(selectedAssets)==0){
 weights<-rep(0, ia$n)
 names(weights)<-names(ia$index)
 weights
 }else if (sum(selectedAssets)==1){
 weights<-rep(0,ia$n)
 weights[which(selectedAssets==TRUE)]=1
 names(weights)<-names(ia$index)
 weights
 }else{
 selectia<-create.ia(selectedSubset)
 selconstrants =create.basic.constraints(sum(selectedAssets),constrants$lb[selectedAssets], 
 constrants$ub[selectedAssets], 1)
 weights<-min.var.portfolio(selectia,selconstrants)
 set.risky.asset(weights,selectedAssets)
 }
}"),
               pre("
HRP.portfolio.TF<-function(ia,constrants){
  moms <- Return.cumulative(ia$hist.returns)
  highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0){
  weights<-rep(0, ia$n)
  names(weights)<-names(ia$index)
  weights
  }else if (sum(selectedAssets)==1){
  weights<-rep(0,ia$n)
  weights[which(selectedAssets==TRUE)]=1
  names(weights)<-names(ia$index)
  weights
  }else{
  clustOrder <- hclust(dist(ia$corr[selectedAssets,selectedAssets]), method = 'single')$order
  weights<-getRecBipart(ia$cov[selectedAssets,selectedAssets], clustOrder)
  set.risky.asset(weights,selectedAssets)
  }
}"),
               pre("
risk.parity.portfolio.TF<-function(ia,constrants){
  moms <- Return.cumulative(ia$hist.returns)
  highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0){
  weights<-rep(0, ia$n)
  names(weights)<-names(ia$index)
  weights
  }else if (sum(selectedAssets)==1){
  weights<-rep(0,ia$n)
  weights[which(selectedAssets==TRUE)]=1
  names(weights)<-names(ia$index)
  weights
  }else{
  risk=ia$risk[selectedAssets]
  weights=1/risk/(sum(1/risk))
  set.risky.asset(weights,selectedAssets)
  }
}")
               
               
               # strong('References'),
               # p(HTML('<ul>'),
               #   HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
               #   HTML('<li>'),a('SIR application by Samuel M. Jenness', href="http://glimmer.rstudio.com/smjenness/SIR/", target="_blank"),HTML('</li>'),
               #   HTML('<li>'),a('SIR application code by Samuel M. Jenness', href="https://github.com/smjenness/Shiny/tree/master/SIR", target="_blank"),HTML('</li>'),
               #   HTML('</ul>'))
               )
      ))
  )
)