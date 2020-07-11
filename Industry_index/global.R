#require(quantmod)
library(RCurl)
library(rvest)
library(rjson)
library(stringr)
library(xml2)
library(SIT)
library(data.table)
library(readr)



get_index<-function(save=FALSE){
url='http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?type=CT&cmd=C.1&sty=FCOIATA&sortType=(ChangePercent)&sortRule=-1&page=1&pageSize=3000&js=var%20fWclhjZJ={rank:[(x)],pages:(pc),total:(tot)}&token=7bc05d0d4c3c22ef9fca8c2a912d779c&jsName=quote_123&_g=0.628606915911589&_=1523194042600'
url2='http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?type=CT&cmd=C.5&sty=FCOIATA&sortType=(ChangePercent)&sortRule=-1&page=1&pageSize=3000&js=var%20xguHTOUQ={rank:[(x)],pages:(pc),total:(tot)}&token=7bc05d0d4c3c22ef9fca8c2a912d779c&jsName=quote_123&_g=0.628606915911589&_=1523197989755'
get_hsindex<-function(url){
tryCatch(
  #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
  web<-read_html(url),
  error=function(e){cat("ERROR :","","\n")
    Sys.sleep(1)
  }
)
txt<-web%>% html_text(trim=TRUE)
txt<-sub('var .*rank:[[]',"",txt)
txt<-sub('[]].*',"",txt)
data<-sapply(strsplit(txt,'",')[[1]],strsplit,',')
data=do.call(rbind,data)

data=as.data.frame(data[,c(2:13,22)],row.names = 1:nrow(data))

col=c('指数代码','名称','最新价','涨跌额','涨跌幅','振幅','成交量','成交额','昨收','今开','最高','最低','5分钟涨跌')
colnames(data)=col
#data['市值']=as.numeric(data['成交额'])/as.numeric(data['换手率'])*100
data[,1]=sapply(data[,1],as.character)
data
}
data=rbind(get_hsindex(url),get_hsindex(url2))
if(save==TRUE)
  write.csv(data,file='index.csv',fileEncoding = 'UTF-8')
data
}

# update_data<-function(){
# files=list.files('data')
# symbol_env=new.env()
# for (ff in files){
#   temp=read.xts(paste0('data/',ff))
#   symbol_env[[substring(ff,1,6)]]=temp
# }
# bt.prep(symbol_env, align='keep.all', fill.gaps = T)
# save(symbol_env,file='index.Rdata')
# }

#update_data()


update_content<-function(){
  files=list.files('content')
  index_con=new.env()
  for (ff in files){
    temp=read_csv(paste0('content/',ff),col_types = list('成分券代码'=col_character()))
    index_con[[substring(ff,1,6)]]=temp
  }
  #bt.prep(symbol_env, align='keep.all', fill.gaps = T)
  save(index_con,file='indexcontent.Rdata')
}



get_mktcap<-function(){
  url='http://xuanguapi.eastmoney.com/Stock/JS.aspx?type=xgq&sty=xgq&token=eastmoney&c=[cz20(4|0w)][cz19(4|0w)]&p=1&jn=yJmWnAha&ps=4000&s=cz19(4|0w)&st=1&r=1523285116734'
  tryCatch(
    #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
    web<-read_html(url),
    error=function(e){cat("ERROR :","","\n")
      Sys.sleep(1)
    }
  )
  txt<-web%>% html_text(trim=TRUE)
  txt<-sub('var .*rank:[[]',"",txt)
  txt<-sub('["]].*',"",txt)
  data<-sapply(strsplit(txt,'",')[[1]],strsplit,',')
  data=do.call(rbind,data)
  
  data=as.data.frame(data[,c(2:5)],row.names = 1:nrow(data))
  
  col=c('股票代码','股票名称','流通市值','总市值')
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
  data0<-matrix(unlist(data0),ncol=17,byrow=TRUE)
  data0<-as.data.frame(data0[,c(2:10)])
  rownames(data0)<-1:nrow(data0)
  colnames(data0)<-c('股票代码','股票名称','最新价','涨跌幅','市盈率','换手率','主力成本','机构参与度','控盘')
  stock_info<-merge(data0,data)
  stock_info[,c('主力成本','机构参与度')]<-sapply(stock_info[,c('主力成本','机构参与度')],function(x){round(as.numeric(as.character(x)),2)})
  stock_info[,c('总市值/亿','流通市值/亿')]<-sapply(stock_info[,c('总市值','流通市值')],function(x){round(as.numeric(as.character(x))/1e8,2)})
  stock_info[,c('最新价','涨跌幅','市盈率','换手率')]<-sapply(stock_info[,c('最新价','涨跌幅','市盈率','换手率')],function(x){as.numeric(as.character(x))})
  stock_info<-stock_info[,c(-10,-11)]
  save(stock_info,file='stock.Rdata')
  stock_info
  }

base_index<-c('000001-上证指数','000016-上证50','000300-沪深300','000905-中证500',
              '399001-深证成指','399005-中小板指','399006-创业板指')
index_info<-get_index(save=TRUE)
#stock_info<-get_mktcap()
indexes<-as.data.frame(gsub('.csv','',list.files('content')))
colnames(indexes)='指数代码'
indexname=merge(indexes,index_info[,c(1,2)])
indexname=paste(indexname[,1],indexname[,2],sep = '-')

