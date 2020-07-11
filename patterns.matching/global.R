library(shiny)
library(xtable)
library(SIT)
path=getwd()
source(sub("project/dwhang.*",'project/SIT/bt.test.r',path))
source(sub("project/dwhang.*",'project/SIT/utils.r',path))

library(RCurl)
library(RSQLite)

load('pool.Rdata')


dir<-getwd()
dir_index<-sub('patterns.matching','Industry_index',dir)
con_index<-dbConnect(dbDriver("SQLite"),dbname=paste0(dir_index,'/','index_database.db'))
index_pool<-dbListTables(con_index)
index_pool=paste(index_pool,'index',sep='-')

# dir<-getwd()
# dir_stock<-sub('patterns.matching','Stocks_historical',dir)
# con_stock<-dbConnect(dbDriver("SQLite"),dbname=paste0(dir_stock,'/','stocks_database.db'))
# stock_pool<-dbListTables(con_stock)
pools=c(index_pool)
# save(pools,file='pool.Rdata')