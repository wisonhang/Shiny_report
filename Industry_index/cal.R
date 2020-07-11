require(quantmod)
require(rPython)
require(rjson)
require(RSQLite)
library(showtext)
library(corrplot)
library(stringr)
library(xml2)
library(SIT)

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

prices=symbol_env$prices['2017::2018']


dayret=prices/mlag(prices,1)-1


weeks=endpoints(prices,'weeks')
weeks=weeks[weeks>0]
weekret=prices[weeks,]/mlag(prices[weeks,],1)-1

a=apply(dayret,2,sd,na.rm=T)*sqrt(252)
b=apply(weekret,2,sd,na.rm=T)*sqrt(52)

Vol=data.frame(dayVol=a,weekVol=b)
Vol['code']=rownames(Vol)

index_info=read.csv('index.csv',row.names = 1,colClasses = 'character')
Vol=merge(Vol,index_info[,c('指数代码','名称')],by.x='code',by.y='指数代码')


write_csv(Vol,'vol.csv')
