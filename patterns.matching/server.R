library(SIT)
library(quantmod)
# Define server
# library(quantstrat)
shinyServer(function(input, output) {

  dir<-getwd()
  dir_index<-sub('patterns.matching','Industry_index',dir)
  con_index<-dbConnect(dbDriver("SQLite"),dbname=paste0(dir_index,'/','index_database.db'))
  index_pool<-dbListTables(con_index)
  index_pool=paste(index_pool,'index',sep='-')
  
  # dir<-getwd()
  # dir_stock<-sub('patterns.matching','Stocks_historical',dir)
  # con_stock<-dbConnect(dbDriver("SQLite"),dbname=paste0(dir_stock,'/','stocks_database.db'))
  # stock_pool<-dbListTables(con_stock)
  #pools=c(index_pool,stock_pool)
  
  # Create an environment for storing data
  symbol_env <- new.env()
  
  #*****************************************************************
  # Shared Reactive functions
  # http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
  #******************************************************************    	
  # Get stock data
  
  output$input<-renderUI({
    if (input$index=='指数')
      selectInput("symbol", "code:", choices = index_pool, selected=index_pool[1])
    else
      selectInput("symbol", "code:", choices = stock_pool, selected=stock_pool[1])
  })

  getData<-eventReactive(input$reset,{
    if(input$index=='指数'){
      sym=spl(input$symbol,'-')[1]
      query=sub('code',sym,'select * from "code"')
      data=read.xts(dbGetQuery(con_index,query))
    }else if(input$index=='股票'){ 
      sym=input$symbol
      query=sub('code',sym,'select * from "code"')
      data=read.xts(dbGetQuery(con_stock,query))
    }
    data=data[paste0(as.character(input$date),'::')]
    data	
  })
  
  # getData<-eventReactive(input$reset,{
  #   initialData()
  # })
  
  getobj<-reactive({
    data = getData()
    obj = bt.matching.find(Cl(data), main = input$symbol, n.query=input$dnum, n.match=input$nnum,
                               n.reference=252*input$nref,plot=FALSE)
  })
  getmatches<-reactive({
    obj=getobj()
    macthes=bt.matching.overlay(obj, plot=TRUE)
  })
  # Find Januaries with return greater than threshhold
  
  #*****************************************************************
  # Not Reactive helper functions
  #*****************************************************************
  # Make plot
  makePlot1 <- function() {  	
    data = getData()
    month.year.seasonality(data, input$symbol)
    
  }
  
  
  makePlot2<-function(){
    data = getData()
    matches = bt.matching.find(Cl(data), main = input$symbol, n.query=input$dnum, n.match=input$nnum,
                               n.reference=252*input$nref,plot=TRUE)  
  }
  
  
  makePlot3<-function(){
    matches = getobj()
    bt.matching.overlay(matches, plot=TRUE)  
  }
  
  makePlot4<-function(){
    data = getData()
    plot.patterns(data,252*input$ypat,input$symbol)
  }
  
  makeSeasonalityTable<-function(){
    data = getData()
    obj = bt.matching.find(Cl(data), 
                           main = input$symbol, 
                           n.query=input$dnum, 
                           n.match=input$nnum,
                           plot=FALSE)  
    matches = bt.matching.overlay(obj, plot=FALSE) 
    bt.matching.overlay.table(obj, matches, plot=T)
  }

  #*****************************************************************
  # Update plot(s) and table(s)
  #******************************************************************    	
  # Generate a plot
  
  output$seasonalityPlot <- renderPlot({
    makePlot1()
  }, height = 400, width = 1200)
  
  output$patternsMatch <- renderPlot({
    makePlot2()
  }, height = 400, width = 1200)
  
  output$patternsPred <- renderPlot({
    makePlot3()
  }, height = 400, width = 1200)
  
  output$patterns <- renderPlot({
    makePlot4()
  }, height = 400, width = 1200)
  
  #Generate a table
  # output$patternsTable <-reactive({
  #   tableColor(makeSeasonalityTable(), include.rownames=TRUE,digits=1)
  # })
  output$patternsTable <-renderPlot({
    plot.table(makeSeasonalityTable())
  },height = 400, width = 1200)
  
  #*****************************************************************
  # Download
  #******************************************************************    
  # Download pdf report
  output$downloadReport <- downloadHandler(
    filename = 'report.pdf',
    content = function(file) {
      pdf(file = file, width=7, height=4,onefile=TRUE)
      #layout(matrix(c(1,1,2,3),2,2,byrow=T))
      makePlot1()
      plota.add.copyright()
      
      makePlot2()
  
      #plota.add.copyright()
      makePlot3()

      #plota.add.copyright()
      makePlot4()
      #plota.add.copyright()
      makeSeasonalityTable()
      dev.off()
    }
  )	
  
  
  
  
  
  
  
  
  
  # Download csv data
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      data=getData()
      write.xts(data,filename=file)     		
    }
  )	
  
  
  
  #*****************************************************************
  # Update status message 
  #******************************************************************    
  # output$status <- renderUI({
  #   out = tryCatch( getData(), error=function( err ) paste(err))	    				
  #   if( is.character( out ) ) 
  #     HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
  #   else
  #     HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
  # })
  
  backtest<-function(){
    dt = getData()
    data=new.env()
    testdt=dt['2017-01-01::']
    data[[input$symbol]]=testdt
    bt.prep(data)
    
    hist=Cl(dt['2010-01-01::'])
    
    month.ends = endpoints(hist, 'days')
    month.ends = month.ends[month.ends > 0]

    start.index = which(date.year(index(hist[month.ends])) == 2017)[1]
    weight = hist * NA

    for( i in start.index : len(month.ends) ) {
      #obj = bt.matching.find(hist[1:month.ends[i],], n.match=10, normalize.fn = normalize.mean, plot=T)
      #matches = bt.matching.overlay(obj, future=hist[(month.ends[i]+1):(month.ends[i]+22),], plot=T)
      #bt.matching.overlay.table(obj, matches, weights=NA, plot=T)

      #obj = bt.matching.find(hist[1:month.ends[i],], normalize.fn = normalize.first,dist.fn = 'dist.DTW')
      obj = bt.matching.find(hist[1:month.ends[i],],
                             n.query = 12,
                             n.match = 10,
                             #n.reference = 5*252,
                             normalize.fn = normalize.first,dist.fn = 'dist.DTW'
                             )
      matches = bt.matching.overlay(obj)

      # compute prediction for next month
      n.match = len(obj$min.index)
      n.query = len(obj$query)
      month.ahead.forecast = matches[,(n.query+1)]/ matches[,1*n.query] - 1

      # Average, mean(month.ahead.forecast[1:n.match])
      # weights = rep(1/n.match, n.match)
      # avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)

      # Distance weighted average
      temp = round(100*(obj$dist / obj$dist[1] - 1))
      n.weight = max(temp) + 1
      weights = (n.weight - temp) / ( n.weight * (n.weight+1) / 2)
      weights = weights / sum(weights)
      # barplot(weights)
      avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)

      # Logic
      weight[month.ends[i]] = 0
      if( avg.direction > 0 ) weight[month.ends[i]] = 1

      # print progress
      if( i %% 10 == 0) cat(i, '\n')
  }
    prices = data$prices  
    
    # Buy & Hold	
    data$weight['2017-01-01::'] = 1
    buy.hold = bt.run(data)	
    
    # Strategy
    data$weight[] = NA
    data$weight[] = weight['2017-01-01::']
    capital = 100000
    data$weight[] = (capital / prices) * bt.exrem(data$weight)
    test = bt.run(data, type='share', do.lag = 0,capital=capital, trade.summary=T)
    plotbt.custom.report.part1(test, buy.hold, trade.summary=T)
    
  }

  
  
}  
)


#bt.matching.backtest.test()
#bt.matching.dtw.test()