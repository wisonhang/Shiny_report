require(quantmod)
require(rPython)
require(rjson)
require(RSQLite)
library(showtext)
library(corrplot)
library(stringr)
library(plotly)
library(DT)
library('infotheo')
showtext_auto()
font_add("SimSun", "/home/dwhang/project/simsun.ttc")

showTable<-function(x,pct_col=c(),comma_col=c()){
 nlen=ncol(x)
 dt=datatable(x,
            rownames = F,
            filter = 'bottom',
            extensions = 'Buttons',
            # class='cell-border stripe',
            style = "bootstrap",
            class = "display",
            options=list(
              buttons = c('copy', 'excel','colvis'),
              initComplete = JS("function(settings, json)
                                  {$(this.api().table().header()).css({'font-size' : '15px'});}"),
              pageLength = 20, autoWidth = F,
              lengthMenu = c(20,50,100,nrow(x)),
              dom='Blfrip',
              columnDefs = list(list(className = 'dt-center',
                                     targets = 0:(nlen-1))
              )
              
            )
  )
 if(!is.null(pct_col))
   dt=dt%>%formatPercentage(pct_col,digits = 2)
 if(!is.null(comma_col))
   dt=dt%>%formatCurrency(comma_col,'',digits = 0)
 
 dt
}


get_mktcap<-function(){
  #url='http://xuanguapi.eastmoney.com/Stock/JS.aspx?type=xgq&sty=xgq&token=eastmoney&c=[cz20(4|0w)][cz19(4|0w)]&p=1&jn=yJmWnAha&ps=4000&s=cz19(4|0w)&st=1&r=1523285116734'
  
  url='http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?cb=jQuery112402093827286204657_1544789086599&type=CT&token=4f1862fc3b5e77c150a2b985b12db0fd&sty=FCOIATC&js=(%7Bdata%3A%5B(x)%5D%2CrecordsFiltered%3A(tot)%7D)&cmd=C._A&st=(ChangePercent)&sr=-1&p=1&ps=4000&_=1544789086639'
  
  tryCatch(
    #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
    web<-read_html(url,encoding ='utf-8'),
    error=function(e){cat("ERROR :","","\n")
      Sys.sleep(1)
    }
  )
  txt<-web%>% html_text(trim=TRUE)
  txt<-sub('jQuery.*data:[["]','',txt)
  #txt<-sub(",record.*curpage:1}",'',txt)
  txt<-sub('["]].*',"",txt)
  data<-sapply(strsplit(txt,'",')[[1]],strsplit,',')
  data=do.call(rbind,data)
  data=as.data.frame(data[,c(2,3,18:22)],row.names = 1:nrow(data))
  
  col=c('股票代码','股票名称','市净率','总市值','流通市值','60日涨跌幅','自年初涨跌幅')
  colnames(data)=col
  
  url='http://dcfm.eastmoney.com/em_mutisvcexpandinterface/api/js/get?type=QGQP_LB&token=70f12f2f4f091e459a279469fe49eca5&cmd=&st=Code&sr=1&p=1&ps=4000&js=var%20AqhvICne={pages:(tp),data:(x)}&filter=&rt=50776132'
  tryCatch(
    #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
    web<-read_html(url),
    error=function(e){cat("ERROR :","","\n")
      Sys.sleep(1)
    }
  )
  txt<-web%>% html_text(trim=TRUE)
  txt<-sub("var.*data:",'',txt)
  #txt<-sub(",record.*curpage:1}",'',txt)
  data0<-rjson::fromJSON(txt)
  data0<-matrix(unlist(data0),ncol=22,byrow=TRUE)
  data0<-as.data.frame(data0[,c(2:10)])
  rownames(data0)<-1:nrow(data0)
  colnames(data0)<-c('股票代码','股票名称','最新价','涨跌幅','市盈率','换手率','主力成本','机构参与度','控盘')
  stock_info<-merge(data0,data,by.x=c('股票代码','股票名称'),by.y=c('股票代码','股票名称'),all.x=T)
  stock_info[,c('主力成本','机构参与度')]<-sapply(stock_info[,c('主力成本','机构参与度')],function(x){round(as.numeric(as.character(x)),2)})
  stock_info[,c('总市值/亿','流通市值/亿')]<-sapply(stock_info[,c('总市值','流通市值')],function(x){round(as.numeric(as.character(x))/1e8,2)})
  stock_info[,c('最新价','涨跌幅','市盈率','换手率')]<-sapply(stock_info[,c('最新价','涨跌幅','市盈率','换手率')],function(x){as.numeric(as.character(x))})
  stock_info[,c('60日涨跌幅','自年初涨跌幅')]=sapply(stock_info[,c('60日涨跌幅','自年初涨跌幅')],function(x){as.numeric(sub('%','',x))})
  
  stock_info<-stock_info[,c(-11,-12)]
  save(stock_info,file='stock.Rdata')
  stock_info
}




#get_mktcap()

update_data<-function(){
  con=dbConnect(dbDriver("SQLite"),dbname='index_database.db')
  code_list=dbListTables(con)
  symbol_env=new.env()
  #timeDate=Sys.Date()-365
  timeDate='2015-01-01'
  for (code in code_list){
    query=paste0(sub('code',code,'select * from "code"'),sub('Date',timeDate,' where date>="Date 00:00:00"'))
    temp=read.xts(dbGetQuery(con,query))
    if (len(temp)==0)
      print(code)
    else
      symbol_env[[code]]=temp
  }
  bt.prep(symbol_env, align='keep.all', fill.gaps = T)
  save(symbol_env,file='index.Rdata')
}




indexsummary<-function(prices){
  ret<-na.omit(prices/mlag(prices,1)-1)
  equity<-cumprod(1+ret)
  out<-list()
  out$Period = SIT::join(format(range(index.xts(equity)), "%b%Y"), 
                         " - ")
  out$Cagr = compute.cagr(equity)
  out$Sharpe = compute.sharpe(ret)/100
  out$DVR = compute.sharpe(ret) * compute.R2(equity)/100
  out$Volatility = compute.risk(ret)
  out$MaxDD = compute.max.drawdown(equity)
  out$AvgDD = compute.avg.drawdown(equity)
  out$VaR = compute.var(ret)
  out$CVaR = compute.cvar(ret)
  temp = list2matrix(out, keep.names = F)
  temp=data.frame(temp,stringsAsFactors = FALSE)
  colnames(temp)=names(prices)
  temp[2:9,]=round(as.numeric(temp[2:9,])*100,2)
  temp
}



#update_data()
# index_info=get_index(save=TRUE)
# stock_info=get_mktcap()



shinyServer(function(input, output) {
  load('index.Rdata')
  load('indexcontent.Rdata')
  load('stock.Rdata')
  index_info=read.csv('index.csv',row.names = 1,colClasses = 'character')
  
  observeEvent(input$update,{
    #file_name<-paste0(getwd(),'/indexget.py')
    #print(file_name)
    #### issue error for ali cloud
    #python.load(file_name) 
    
    update_data()
    get_index(save=TRUE)
    insertUI("#update", "afterEnd",
             #h5('Finished updated index data' )
             HTML(paste("<b>Status</b>: <b><font color='green'>","Finished updated index data","</font></b>"))
             )
    }
  )
  
  ########## indexdata 指数数据###
  
  indexdata<-reactive({
    load('index.Rdata')
    symbol_env
  })
  
  output$status <- renderUI({
    symbol_env<-indexdata()
    #load('index.Rdata')
    HTML(paste("<b>Status</b>: <b><font color='red'>",index(xts::last(symbol_env$prices)),"</font></b>"))
  })
  
  retrun_cal<-reactive({
    symbol_env<-indexdata()
    #load('index.Rdata')
    x<-symbol_env$prices
    ret<-t(rbind(xts::last(x)/xts::last(mlag(x,1))-1,
                 xts::last(x)/xts::last(mlag(x,5))-1,
                 xts::last(x)/xts::last(mlag(x,20))-1,
                 xts::last(x)/xts::last(mlag(x,61))-1,
                 xts::last(x)/xts::last(mlag(x,122))-1,
                 xts::last(x)/xts::last(mlag(x,244))-1))
    ret<-as.data.frame(ret)
    colnames(ret)<-c('昨日','近1周','近1月','近3月','近6月','近1年')
    ret['指数代码']<-rownames(ret)
    ret
  })
  
  
  #######指数历史收益率###
  index_hist_info<-reactive({
    ret<-retrun_cal()
    #data<-read.csv('index.csv',row.names = 1,colClasses = 'character')
    #data<-get_index()
    data<-merge(index_info,ret)
    data<-data[,c('指数代码','名称','最新价','振幅','成交额','昨日','近1周','近1月','近3月','近6月','近1年')]
    data[,'成交额']<-sapply(data[,'成交额'],function(x){round(as.numeric(x)/10^8,2)})
    showindex<-data[!duplicated(data['名称']),]
    rownames(showindex)=1:dim(showindex)[1]
    showindex
    })
  
  selectindex<-reactive({
    showindex=index_hist_info()
    if(input$index.compare){
      rankindex=apply(showindex[,c('近1周','近1月','近3月','近6月','近1年')],2,rank,na.last=FALSE)
      for (i in input$index.rank){
        rankindex[,i]=rankindex[,i]>(dim(rankindex)[1]*(1-input[[i]][2]/100))
      }
      showindex=showindex[apply(rankindex,1,all),]
    }
    showindex
  })
  
  output$openfund<-renderDataTable({
    if(input$index.compare){
      showindex<-selectindex()
    }else{
      showindex<-index_hist_info()
    }
    # per<-function(x){
    #   round(x*100,1)
    # }
    # showindex[,c('昨日','近1周','近1月','近3月','近6月','近1年')]<-sapply(showindex[,c('昨日','近1周','近1月','近3月','近6月','近1年')],per)
    colnames(showindex)<-c('指数代码','名称','最新价','振幅',
                           '成交额/亿','昨日(%)','近1周(%)','近1月(%)','近3月(%)','近6月(%)','近1年(%)')
    showindex%>%showTable(pct_col = c(6:11))
  })
  
  
  #######获取指数历史价格序列###
  index_get<-reactive({
    symbol_env<-indexdata()
    index_hist=symbol_env[[spl(input$code,'-')[1]]]
    #index_hist[,'指数名称']=input$code

  })
  
  
  #############Rev & vol############
  index_mkv<-reactive({
    symbol_env<-indexdata()
    prices<-symbol_env$prices
    prices<-na.locf(prices) 
    prices<-na.fill(prices,'extend')
    
    index_info[,'类型']=sapply(index_info[,'名称'],function(x){
      x=as.character(x)
      
      if ( !is.na(str_match(x,'债')[1]) || !is.na(str_match(x,'信用')[1])){
        #print(x)
        return('债券')
      }else
        return('股票')
    })
    year_period=year(Sys.Date())
    last_year=year_period-1
    index_summary=data.frame(
    code=colnames(prices),
    Last_ret=apply(prices[paste0(last_year)]/mlag(prices[paste0(last_year)],1)-1,2,mean,na.rm=T)*250,
    Last_vol=apply(prices[paste0(last_year)]/mlag(prices[paste0(last_year)],1)-1,2,sd,na.rm=T)*sqrt(250),
    Hist_ret=apply(prices/mlag(prices,1)-1,2,mean,na.rm=T)*250,
    Hist_vol=apply(prices/mlag(prices,1)-1,2,sd,na.rm=T)*sqrt(250),
    YTD_ret=apply(prices[paste0(year_period)]/mlag(prices[paste0(year_period)],1)-1,2,mean,na.rm=T)*250,
    YTD_vol=apply(prices[paste0(year_period)]/mlag(prices[paste0(year_period)],1)-1,2,sd,na.rm=T)*sqrt(250),
    Last_total_ret=matrix(last(prices[paste0(last_year)],1))/matrix(first(prices[paste0(last_year)],1))-1,
    YTD_total_ret=matrix(last(prices[paste0(year_period)],1))/matrix(first(prices[paste0(year_period)],1))-1,
    Hist_total_ret=matrix(last(prices,1))/matrix(first(prices,1))-1
    )

  index_summary=merge(index_summary,index_info[,c('指数代码','名称','成交额','类型')],by.x = 'code',by.y = '指数代码')
  index_summary$成交额=sapply(index_summary$成交额,function(x){round(as.numeric(as.character(x))/1e8,1)})
  index_summary=index_summary[!duplicated(index_summary$名称),]
  index_summary=index_summary[!is.na(index_summary$成交额),]
  
  index_summary['规模']=discretize(index_summary$成交额,disc='equalfreq' ,5)
  temp_size=index_summary[,c('成交额','规模')]%>%dplyr::group_by(规模)%>%dplyr::summarise(
    size=paste0(min(成交额,na.rm=T),'-',max(成交额,na.rm=T)))
  index_summary=index_summary%>%dplyr::left_join(temp_size
    )
  
  index_summary$size=factor(index_summary$size,levels=temp_size$size)
  index_summary$规模=as.numeric(index_summary$规模)
  
  index_summary
  })
  
  output$rev_vol_bubble<-renderPlotly({
  index_summary<-index_mkv()  
    
  sindex=highlight_key(index_summary,~code)
  base=plot_ly(sindex,height=800)%>%group_by(code)
  
  g1=base%>%add_trace(x=~YTD_vol,y=~YTD_ret,
                      color=~size,size=~规模,sizes = c(5, 9),
                      type='scatter',
                      mode='markers',
                      marker=list(symbol = 'circle', sizemode = 'diameter',opacity = 0.75
                      ),
                      hoverinfo = 'text',
                      text=~paste0('指数代码: ',code,
                                   '<br>指数名称: ',名称,
                                   '<br>昨日成交额: ',成交额,'亿',
                                   '<br>          年化收益, 年化波动率',
                                   '<br>2020:    ',paste0(round(YTD_ret*100,0),'%'),',  ',
                                   paste0(round(YTD_vol*100,1),'%'),
                                   '<br>2019:    ',paste0(round(Last_ret*100,0),'%'),',  ',
                                   paste0(round(Last_vol*100,1),'%'),
                                   '<br>15-YTD: ', paste0(round(Hist_ret*100,0),'%'),',  ',
                                   paste0(round(Hist_vol*100,1),'%')
                      )
  )
  
 g2=base%>%add_trace(x=~Last_vol,y=~Last_ret,
                           color=~size,size=~规模,sizes = c(5, 9),
                           type='scatter',
                           mode='markers',
                           marker=list(symbol = 'circle', sizemode = 'diameter',opacity = 0.75
                           ),
                     hoverinfo = 'text',
                     text=~paste0('指数代码: ',code,
                                  '<br>指数名称: ',名称,
                                  '<br>昨日成交额: ',成交额,'亿',
                                  '<br>          年化收益, 年化波动率',
                                  '<br>2020:    ',paste0(round(YTD_ret*100,0),'%'),',  ',
                                  paste0(round(YTD_vol*100,1),'%'),
                                  '<br>2019:    ',paste0(round(Last_ret*100,0),'%'),',  ',
                                  paste0(round(Last_vol*100,1),'%'),
                                  '<br>15-YTD: ', paste0(round(Hist_ret*100,0),'%'),',  ',
                                  paste0(round(Hist_vol*100,1),'%')
                     ),
                     showlegend = FALSE
  )
  
 g3=base%>%add_trace(x=~Hist_vol,y=~Hist_ret,
                     color=~size,size=~规模,sizes = c(5, 9),
                     type='scatter',
                     mode='markers',
                     marker=list(symbol = 'circle', sizemode = 'diameter',opacity = 0.75
                     ),
                     hoverinfo = 'text',
                     text=~paste0('指数代码: ',code,
                                  '<br>指数名称: ',名称,
                                  '<br>昨日成交额: ',成交额,'亿',
                                  '<br>          年化收益, 年化波动率',
                                  '<br>2020:    ',paste0(round(YTD_ret*100,0),'%'),',  ',
                                  paste0(round(YTD_vol*100,1),'%'),
                                  '<br>2019:    ',paste0(round(Last_ret*100,0),'%'),',  ',
                                  paste0(round(Last_vol*100,1),'%'),
                                  '<br>15-YTD: ', paste0(round(Hist_ret*100,0),'%'),',  ',
                                  paste0(round(Hist_vol*100,1),'%')
                     ),
                     showlegend = FALSE
 )
  
 subplot(g1,g2,g3,titleX = TRUE,nrows = 3) %>% 
   #hide_legend() %>%
   highlight(on='plotly_hover')
 
  
  })
  
  output$index_summary<-renderDataTable({
    index_summary<-index_mkv()  
    index_summary[,2:10]=sapply(index_summary[,2:10],function(x){round(x,4)})
    index_summary=index_summary[,c(1,11:13,4,2,6,10,8,9)]
    colnames(index_summary)=c('指数代码','指数名称','成交额/亿','类型','历史年化收益','去年年化收益','YTD年化收益',
                              '历史累计收益','去年累计收益','YTD累计收益')
    index_summary%>%showTable(pct_col = c(5:10))%>%
      formatCurrency(3,'',digits = 1)
  }
  )
  
  
  #########################candle plot##########
  
  
  
  # df <- data.frame(Date=index(index_hist),coredata(index_hist))
  #
  # # create Bollinger Bands
  # bbands <- BBands(df[,c("high",'low','close')])
  #
  # # join and subset data
  # df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2015-02-14")
  #
  # # colors column for increasing and decreasing
  # for (i in 1:length(df[,1])) {
  #   if (df$close[i] >= df$open[i]) {
  #     df$direction[i] = 'Increasing'
  #   } else {
  #     df$direction[i] = 'Decreasing'
  #   }
  # }

candle_plotly<-function(df,titlename=''){
  namecode=spl(titlename,'-')[1]
  i <- list(line = list(color = '#FF6347'))
  d <- list(line = list(color = '#ADFF2F'))

  # plot candlestick chart
  p <- df %>%
    plot_ly(x = ~Date, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low, name = namecode,
            increasing = i, decreasing = d) %>%
    add_lines(x = ~Date, y = ~up , name = "B Bands",
              line = list(color = '#ccc', width = 0.9),
              legendgroup = "Bollinger Bands",
              hoverinfo = "none", inherit = F) %>%
    add_lines(x = ~Date, y = ~dn, name = "B Bands",
              line = list(color = '#ccc', width = 0.9),
              legendgroup = "Bollinger Bands", inherit = F,
              showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
              line = list(color = '#E377C2', width = 1),
              hoverinfo = "none", inherit = F) %>%
    layout(yaxis = list(title = "Price"),hovermode='closeset')

  # plot volume bar chart
  pp <- df %>%
    plot_ly(x=~Date, y=~volume, type='bar', name = paste0(namecode," Volume"),
            color = ~direction, colors = c('#FF6347','#ADFF2F')) %>%
    layout(yaxis = list(title = "Volume"))

  # create rangeselector buttons
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))

  # subplot with shared x axis
  p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste0(titlename,' ',as.character(first(df$Date)),'/',Sys.Date()),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'),
           hovermode='closeset')

  
  
}
  
  
  
  output$candleplot<-renderPlotly({
    index_hist<-index_get()
    df <- data.frame(Date=index(index_hist),coredata(index_hist))
    
    # create Bollinger Bands
    bbands <- BBands(df[,c("high",'low','close')])
    
    # join and subset data
    df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2016-01-01")
    
    # colors column for increasing and decreasing
    for (i in 1:length(df[,1])) {
      if (df$close[i] >= df$open[i]) {
        df$direction[i] = 'Increasing'
      } else {
        df$direction[i] = 'Decreasing'
      }
    }
    # chartSeries(index_hist,subset = paste0('last ',input$months,' months'),theme = 'white',name=input$code,
    #             TA=c(addMomentum(),addVo(),addEMA())
    #             )
    candle_plotly(df,title=input$code)
    
  })
  
  
  
  
  
  
  output$monthr<-renderPlot({
    index_hist<-index_get()
    plotbt.monthly.table(na.omit(Cl(index_hist)))
  })
  
  output$stock<-renderDataTable({
    index_list<-as.data.frame(index_con[[spl(input$code,'-')[1]]])
    showdata<-data.frame(matrix(0,1,11))
    colnames(showdata)<-c('股票代码','股票名称','最新价','涨跌幅','换手率','市盈率','主力成本','机构参与度','控盘','总市值/亿','流通市值/亿')
    showdata<-showdata[-1,]
    if (nrow(index_list)>0){
      index_list=data.frame(index_list[,c('成分券代码')])
      colnames(index_list)='股票代码'
      showdata<-merge(index_list,stock_info)
      showdata[,c(11,12)]=showdata[,c(11,12)]/100
    }
    showdata%>%showTable(pct_col=c(8,11,12))
  })
  
  
  

  indexplot_data<-reactive({
    symbol_env<-indexdata()
    codes<-spl(c(input$basic,input$codes),'-')
    select<-unique(codes[seq(1,len(codes),2)])
    prices<-symbol_env$prices[,select]
    prices[paste0(input$date_range[1],'::',input$date_range[2]),]
  })
  
  output$indexplot<-renderPlot({
    prices<-indexplot_data()
    colnames(prices)<-unique(c(input$basic,input$codes))
    #par(family='STKaiti')
    plota.matplot(scale.one(prices),main="历史指数收益曲线",family="SimSun")
  })
  
  output$iaplot<-renderPlot({
    prices<-indexplot_data()
    ret<-mlag(prices,1)/prices-1
    ia<-create.ia(ret)
    plot.ia(ia,layout=layout(matrix(c(1,2,2,2,2),nr=1)))
    
  })
  
  
  output$indexsummary<-renderPlot({
    prices<-indexplot_data()
    summary_index<-do.call(cbind,lapply(prices,indexsummary))
    plot.table(as.matrix(summary_index))
  })
  
  
  ########correlation##
  low_corr_list<-reactive({
    load('history.Rdata')
    prices<-symbol_env$prices
    index_info[,'类型']=sapply(index_info[,'名称'],function(x){
      x=as.character(x)
      
      if ( !is.na(str_match(x,'债')[1]) || !is.na(str_match(x,'信用')[1])){
        #print(x)
        return('债券')
      }else
        return('股票')
    })
    stock_index=index_info[index_info$类型!="债券" & !duplicated(index_info$名称),'指数代码']
    stock_index=setdiff(stock_index,c('399299','399298'))
    prices=prices[,intersect(stock_index,colnames(prices))]
    
    hist_return=prices/mlag(prices,1)-1
    #hist_volity=appl
    daterange=input$corperiod
    timeperiod=paste0(daterange[1],'::',daterange[2])
    num=input$index_num
    period_return=hist_return[timeperiod]
    period_sharp=apply(period_return,2,mean,na.rm=T)/apply(period_return,2,sd,na.rm=T)
    #sharp_list=names(which(rank(-period_sharp)<=200,TRUE))
    #endpoints(period_return,on='months')
    corr_period=cor(period_return,use='complete.obs')
    corr_rank=apply(corr_period,1,function(x){sum(abs(x))})
    #corr_list=names(which(rank(corr_rank)<=200,TRUE))
    combine_list=cbind(rank(-period_sharp),rank(corr_rank))
    index_list=names(which(rank(apply(combine_list,1,sum))<=num,TRUE))
    #index_list=intersect(corr_list,sharp_list)
    #index_list=index_list[1:ifelse(length(index_list)>num,num,length(index_list))]
    corr_profile=list('index'=index_list,'prices'=prices[timeperiod,index_list],'corr'=corr_period[index_list,index_list])

  })

  output$matplot<-renderPlot({
    corr_profile=low_corr_list()
    prices=corr_profile[['prices']]
    index_name=as.character(index_info[,'名称'])
    
    colnames(prices)=paste0(colnames(prices)," - ",index_name[which(index_info[,'指数代码']%in% colnames(prices),TRUE)])
    plota.matplot(scale.one(prices))
  })

  output$corrplot<-renderPlot({
    corr_profile=low_corr_list()
    prices=corr_profile[['prices']]
    ret=prices/mlag(prices,1)-1
    ia<-create.ia(ret)
    plot.ia(ia,layout=layout(matrix(c(1,2,2,2,2),nr=1)))
    #corrplot.mixed(corr_profile[['corr']],order = "hclust", number.cex = .9,tl.pos='lt'
    #)
  })
  # 
  output$index<-renderPlot({
    corr_profile=low_corr_list()
    prices=corr_profile[['prices']]
    summary_index<-do.call(cbind,lapply(prices,indexsummary))
    plot.table(as.matrix(summary_index))
  })
  
  get_corr_index<-function(period_return,num=10){
    period_sharp=apply(period_return,2,mean,na.rm=T)/apply(period_return,2,sd,na.rm=T)
    corr_period=cor(period_return,use='pairwise.complete.obs')
    corr_rank=apply(corr_period,1,function(x){sum(abs(x))})
    combine_list=cbind(rank(-period_sharp),rank(corr_rank))
    index_list=names(which(rank(apply(combine_list,1,sum))<=(num+0.5),TRUE))[1:num]
    index_list
  }
  
  monthly_corr_list<-function(prices,lookback=252){
    endmonth=endpoints(prices,on ='months')
    endmonth=endmonth[endmonth>lookback]
    hist_return=prices/mlag(prices,1)-1
    #corr_index_list<-data.frame(matrix(data=NA,ncol=10,nrow=length(endmonth),byrow=T),row.names = index(prices[endmonth]))
    corr_index_list<-list()
    for (monthindex in endmonth){
      #print(monthindex)
      period_return=hist_return[monthindex-lookback:monthindex,]
      temp_index=get_corr_index(period_return)
      #print(temp_index)
      corr_index_list=c(corr_index_list,temp_index)
      
    }
    corr_index_list=data.frame(matrix(corr_index_list,ncol=10,byrow = TRUE),row.names = index(prices[endmonth]))

    
  }
  
  
  
}
)
