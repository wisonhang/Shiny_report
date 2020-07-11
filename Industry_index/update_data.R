setwd('/home/dwhang/project/dwhang/Industry_index/')
source('global.R')
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
history_data<-function(){
  con=dbConnect(dbDriver("SQLite"),dbname='index_database.db')
  code_list=dbListTables(con)
  symbol_env=new.env()
  #timeDate=Sys.Date()-365
  timeDate='2006-01-01'
  for (code in code_list){
    query=paste0(sub('code',code,'select * from "code"'),sub('Date',timeDate,' where date>="Date 00:00:00"'))
    temp=read.xts(dbGetQuery(con,query))
    if (dim(temp)[1]<(252*5))
      print(code)
    else
      symbol_env[[code]]=temp
  }
  bt.prep(symbol_env, align='keep.all', fill.gaps = T)
  save(symbol_env,file='history.Rdata')
}

get_mktcap<-function(){

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



update_data()
index_info=get_index(save=TRUE)
stock_info=get_mktcap()
history_data()