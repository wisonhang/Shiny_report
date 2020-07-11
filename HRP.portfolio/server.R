# Define server
shinyServer(function(input, output) {
  
  #load('symboldata.Rdata')
  load('Adata.Rdata')
  
  InitialAssets<-eventReactive(input$btu,{
    txt<-unlist(strsplit(gsub(' ','',gsub('\n','',input$symbols)),';'))
    symbols_asset<-list()
    for (subname in c('Equity','Bond','Commodity','Cash')){
      symbols_asset[[subname]]=spl(gsub(paste0(subname,':'),'',txt[grep(subname,txt)]))
    }
    symbols_asset
  })
  
  #symbols_asset<-InitialAssets()
  output$`Assets pool`<-renderUI({
    symbols_asset<-InitialAssets()
    print(symbols_asset)
    #InitialAssets()
    tag<-tagList(
      checkboxGroupInput('Equity',strong('Equity assets select:'),
                         symbols_asset$Equity,selected =symbols_asset$Equity ,inline=TRUE),
      checkboxGroupInput('Commodity',strong('Commodity assets select:'),
                         symbols_asset$Commodity,selected =symbols_asset$Commodity ,inline=TRUE),
      checkboxGroupInput('Bond',strong('Bond assets select:'),
                         symbols_asset$Bond,selected =symbols_asset$Bond ,inline=TRUE))
      # checkboxGroupInput('Cash',strong('Cash assets select:'),
      #                    symbols_asset$Cash,selected =NULL ,inline=TRUE))
    #selectInput('BechMark',strong("Bench-Mark as:"), choices =symbols_asset$Equity ,selected=symbols_asset$Equity[1]))
    
  })
  
  InitialData <- reactive({  	
    cat('getData was called\n')
    data <- new.env()
    now=as.Date(Sys.Date())
    upload_symbol=gsub('.csv','',input$files$name)
    data_symbol=gsub('.csv','',list.files('data'))
    #for(symbol in unlist(asset_pool)) {
    for(symbol in getAssets() ) {
      #print(symbol)
      if(!is.null(symbol_env[[symbol]])){
        if(!symbol %in% c(upload_symbol,data_symbol)){
          start=index(last(symbol_env[[symbol]]))+1
          if(start<now){
            #tempdata<-getSymbols.mine(symbol,from=as.character(start),auto.assign=FALSE)
            tryCatch({
              #tempdata<-getSymbols.mine(symbol,from=as.character(start),auto.assign=FALSE)
              tempdata<-getSymbols(symbol,from=as.character(start),auto.assign=FALSE)
            }, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
            if(!is.null(tempdata))
              symbol_env[[symbol]]=rbind(symbol_env[[symbol]],tempdata)
          }
        }
      }else if (symbol %in% data_symbol){
        #print(paste0(symbol,'data_symbol'))
        temp=read.xts(paste0('data/',symbol,'.csv'))
        colnames(temp)<-paste(symbol, 
                              c("Open", "High", "Low", "Close","Adjusted",'Volume'), 
                              sep = ".")
        symbol_env[[symbol]]=temp
      }else if (symbol %in% upload_symbol){
        temp=read.xts(input$files$datapath[which(grepl(symbol,upload_symbol))])
        colnames(temp)<-paste(symbol, 
                              c("Open", "High", "Low", "Close","Adjusted",'Volume'), 
                              sep = ".")
        symbol_env[[symbol]]=temp
      }else{
        tryCatch(
          #symbol_env[[symbol]]<-getSymbols.mine(symbol,auto.assign=FALSE)
          symbol_env[[symbol]]<-getSymbols(symbol,auto.assign=FALSE)
          , error = function(e) { stop(paste('Problem getting prices for',symbol)) }
        )
      }
      
      data[[symbol]] = symbol_env[[symbol]][paste0(input$yr[1],'::',input$yr[2])]			
    }
    
    #if(input$Update==TRUE)
      #save(symbol_env,file='symboldata.Rdata')
    cat('update the assetdata')
    data
  })
  
  # Helper fns
  getAssets <- reactive({ 
    asset_pool=list(input$Equity,input$Commodity,input$Bond,input$Cash)
    unlist(asset_pool)
  })
  
  getdata<-eventReactive(input$btn,{
    InitialData()
  })
  
  ret.initial<-reactive({
    data<-getdata()
    bt.prep(data, align='keep.all', fill.gaps = T)
    p<-data$prices[paste0(input$year[1],'::',input$year[2])]
    p<-p[endpoints(p,on=input$rlen)]
    ret<-p/mlag(p)-1
    ret
  })
  
  scale.plot<-function(){
    data<-getdata()
    #data = bt.change.periodicity(data, periodicity = input$Periodicity)
    #bt.prep(data, align='remove.na', fill.gaps = T)
    bt.prep(data, align='keep.all', fill.gaps = T)
    par(mfrow=c(2,2))
    plota.matplot(scale.one(data$prices[,input$Equity]),main='Equity')
    plota.matplot(scale.one(data$prices[,input$Commodity]),main='Commodity')
    plota.matplot(scale.one(data$prices[,input$Bond]),main='Bond')
    # if(! is.null(input$Cash)){
    #   plota.matplot(scale.one(data$prices[,input$Cash]),main='Cash')
    # }
  }
  
  output$scalereturn<-renderPlot({
    #scale.plot()
    data<-getdata()
    #data = bt.change.periodicity(data, periodicity = input$Periodicity)
    #bt.prep(data, align='remove.na', fill.gaps = T)
    bt.prep(data, align='keep.all', fill.gaps = T)
    par(mfrow=c(2,2))
    plota.matplot(scale.one(data$prices[,input$Equity]),main='Equity')
    if(! is.null(input$Commodity)){
    plota.matplot(scale.one(data$prices[,input$Commodity]),main='Commodity')
    }
    if(! is.null(input$Bond)){
    plota.matplot(scale.one(data$prices[,input$Bond]),main='Bond')
    }
    if(! is.null(input$Cash)){
      plota.matplot(scale.one(data$prices[,input$Cash]),main='Cash')
    }
  },width=1100,height=600)
  
  ia.plot<-function(){
    ret<-ret.initial()
    ia<-create.ia(ret)
    plot.ia(ia,layout=layout(matrix(c(1,2,2,2,2),nr=1)))
    
  }
  
  
  cor.plot<-function(){
    ret<-ret.initial()
    ia<-create.ia(ret)
    corrplot(ia$correlation,method='square')
    
  }
  
  cor.hrp.plot<-function(){
    ret<-ret.initial()
    ia<-create.ia(ret)
    clustOrder <- hclust(dist(ia$correlation), method = 'single')$order
    seleia<-create.ia(ret[,clustOrder])
    corrplot(seleia$correlation,method='square')
  }
  
  output$retcor<-renderPlot({
    ia.plot()
  })
  
  output$cor1<-renderPlot({
    cor.plot()
  })
  
  output$cor2<-renderPlot({
    cor.hrp.plot()
  })
  
  assets.price<-reactive({
    data<-getdata()
    bt.prep(data,align='keep.all',fill.gaps = T)
    prices=data$prices[paste0(input$yr[1],'::',input$yr[2])]
    prices
  })
  
  
  assetcoef<-reactive({
    data<-getdata()
    bt.prep(data,align='keep.all',fill.gaps=T)
    gpnum<-cbind(apply(data$prices[,input$Equity],1,count),
                 apply(data$prices[,input$Commodity],1,count),
                 apply(data$prices[,input$Bond],1,count))
                 #apply(data$prices[,input$Cash],1,count))
    colnames(gpnum)=c('Equity','Commodity','Bond')
    gpnum<-transform(stack(data.frame(gpnum)),Index=rep(index(data$prices),3))
    colnames(gpnum)=c('nums','AssetsGroup','Index')
    g1<-ggplot(data=gpnum,aes(x=Index,y=nums,fill=AssetsGroup))+geom_area()+
      labs(x='Time of Year',y='Num of assets',title='Number of assets per Group asset')+
      theme(plot.title=element_text(hjust=0.5),legend.position = 'top')
    
    runintraCor<-function(x,n=252){
      result<-NULL
      for(i in 1:(ncol(x)-1)){
        for(j in (i+1):ncol(x)){
          result<-cbind(result,runCor(x[,i],x[,j],n=n,use='all.obs'))
        }
      }
      return(apply(result,1,mean,na.rm=T))
    }
    
    runinterCor<-function(x,y,n=252){
      result<-NULL
      for(i in 1:ncol(x)){
        for(j in 1:ncol(y)){
          result<-cbind(result,runCor(x[,i],y[,j],n=n,use='all.obs'))
        }
      }
      return(apply(result,1,mean,na.rm=T))
    }
    
    total.cof<-runintraCor(data$prices,n=252)
    total.cof<-as.data.frame(list('value'=total.cof))
    total.cof<-transform(total.cof,Index=index(data$prices))
    total.cof['groups']='across.class.coefficient'
    #rownames(total.cof)=seq(1,nrow(total.cof))
    g2<-ggplot(total.cof,aes(x=Index,y=value,fill=groups))+geom_area(alpha=0.5)+
      labs(x='Time of Year',y='Average correlation',title='Rolling Average Pairwise Correlations across all assets')+
      theme(plot.title=element_text(hjust=0.5),legend.position = 'top')
    
    plot.cof<-function(prices=data$prices,gp=input$Equity,n=252){
      cof.intra<-runintraCor(prices[,gp],n=n)
      cof.inter<-runinterCor(prices[,gp],prices[,setdiff(colnames(prices),gp)],n=n)
      temp<-as.data.frame(list('intra-class-coefficient'=cof.intra,
                               'inter-class-coefficient'=cof.inter
      ))
      temp<-transform(stack(temp),Index=rep(index(prices),2))
      colnames(temp)<-c('values','groups','Index')
      temp=na.omit(temp)
      g<-ggplot(data=temp,aes(x=Index,y=values,fill=groups))+
        geom_area(position='identity',alpha=0.5)+
        #geom_line(size=1)+
        labs(x='Time of Year',y='Average correlation',title='Intra and Inter Asset-Class Average Pairwise Correlations')+
        theme(plot.title=element_text(hjust=0.5),legend.position = 'top')
      #theme(legend.position = 'left')
      g
      
    }
    
    g<-list('num'=g1,'total'=g2,
            'Equity'=plot.cof(gp=input$Equity),
            'Commodity'=plot.cof(gp=input$Commodity),
            'Bond'=plot.cof(gp=input$Bond)
            #'Cash'=plot.cof(gp=input$Cash)
    )
    
  })

  
  output$ggplot1<-renderPlot({
    g<-assetcoef()
    g[['num']]
  },height=200)
  
  output$ggplot2<-renderPlot({
    g<-assetcoef()
    g[['total']]
  },height=200)
  
  output$ggplot3<-renderPlot({
    g<-assetcoef()
    g[['Equity']]
  },height=200)
  
  output$ggplot4<-renderPlot({
    g<-assetcoef()
    g[['Commodity']]
  },height=200)
  
  output$ggplot5<-renderPlot({
    g<-assetcoef()
    g[['Bond']]
  },height=200)
  
  
  Backtest<-eventReactive(input$modelrun,{
    data<-getdata()
    bt.prep(data, align='keep all', fill.gaps = T)
    prices = data$prices
    models<-list()
    obj = portfolio.allocation.helper(prices, 
                                      periodicity = input$Periodicity, lookback.len =input$lookback,silent=T, 
                                      create.ia.fn = function(hist.returns,index,nperiod
                                      ){
                                        hist.returns<-apply(hist.returns+1,2,cumprod)
                                        lenout<-switch(input$estimate.len,'months'=21,'weeks'=5,'days'=1)
                                        hist.returns<-hist.returns[rev(seq(input$lookback,1,by = -lenout)),]
                                        hist.returns<-hist.returns/mlag(hist.returns)-1
                                        ia = create.ia(hist.returns)
                                        return(ia)
                                      },
                                      min.risk.fns =list(
                                        MV=min.var.portfolio,
                                        MVTF=min.var.portfolio.TF,
                                        RV=risk.parity.portfolio.basic,
                                        RVTF=risk.parity.portfolio.TF,
                                        HRP=HRP.portfolio,
                                        HRPTF=HRP.portfolio.TF
                                      )
                                      
    )  
    models = create.strategies(obj, data,trade.summary=T, silent=T)$models
    
    models = bt.trim(models)
    models
  })
  
  models.plot1<-function(){
    models<-Backtest()
    data<-getdata()
    layout(1:3)
    plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
    mtext('Cumulative Performance', side = 2, line = 1)
    plotbt.strategy.sidebyside(models, return.table=T,make.plot = T)
    barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  }
  
  
  output$models1<-renderPlot(
    models.plot1(),
    width=1100,height=1000
  )
  
  
  models.plot2<-function(){
    models<-Backtest()
    plotbt.custom.report.part2(models[input$allocation])
  }
  output$custom.report<-renderPlot({
    models.plot2()
  },width=1100,height=1000
  )
  
  models.plot3<-function(){
    models<-Backtest()
    out = sapply(models, function(x) list(
      Ntrades = x$trade.summary$stats['ntrades', 'All'],
      ProfitFactor = x$trade.summary$stats['profitfactor', 'All'],
      Win.Prob= x$trade.summary$stats['win.prob', 'All']*100,
      Win.avg.PnL=x$trade.summary$stats['win.avg.pnl', 'All']*100,
      Loss.avg.PnL=x$trade.summary$stats['loss.avg.pnl', 'All']*100,
      Expectancy=x$trade.summary$stats['expectancy', 'All']*100
    ))  
    performance.barchart.helper(out,sort.performance = F)
  }
  
  output$snapshoot<-renderPlot({
    models.plot3()
  },width=1100,height=500)
  
  trades.summary<-function(){
    models<-Backtest()
    layout(matrix(c(1,rep(2,input$num/15))))
    plotbt.transition.map(models[[input$allocation]]$weight[paste0(input$year[1],'::',input$year[2])],
                          name=paste('asset weight',input$allocation,sep=' '))
    plot.table(last(models[[input$allocation]]$trade.summary$trades,input$num))
  }
  output$trades<-renderPlot(
    {trades.summary()},width=1100,height=1200
  )
  
  
})

