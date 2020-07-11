setwd("/home/dwhang/project/MutualFund")
library(RSQLite)
# library(xts)
library(SIT)
library(dplyr)
library(data.table)
library(lubridate)
library(PerformanceAnalytics)
library(gridExtra)
library(emayili)
library(curl)
library('ggpubr')
con=dbConnect(dbDriver("SQLite"),dbname='/home/dwhang/project/MutualFund/funds_database.db')
code_list=dbListTables(con)
data=dbGetQuery(con,'select * from MutualFundRank')
load('/home/dwhang/project/dwhang/Industry_index/index.Rdata')

index_base=read.csv('/home/dwhang/project/dwhang/Industry_index/index.csv',row.names = 1,colClasses = 'character')

x<-symbol_env$prices
index_ret<-t(rbind(xts::last(x)/xts::last(mlag(x,1))-1,
             xts::last(x)/xts::last(mlag(x,5))-1,
             xts::last(x)/xts::last(mlag(x,20))-1,
             xts::last(x)/xts::last(mlag(x,61))-1,
             xts::last(x)/xts::last(mlag(x,122))-1,
             xts::last(x)/xts::last(mlag(x,244))-1))
index_ret<-as.data.frame(index_ret)
colnames(index_ret)<-c('昨日','近1周','近1月','近3月','近6月','近1年')
index_ret['指数代码']<-rownames(index_ret)

index_info<-merge(index_base,index_ret)
index_info<-index_info[,c('指数代码','名称','昨收','最新价','涨跌幅','成交额','昨日','近1周','近1月','近3月','近6月','近1年')]
index_info[,'成交额']<-sapply(index_info[,'成交额'],function(x){round(as.numeric(x)/10^8,2)})
showindex<-index_info[!duplicated(index_info['名称']),]
rownames(showindex)=1:dim(showindex)[1]

INDEX_CNT=nrow(showindex)

long_index=showindex%>%mutate(
  rank_1=rank(desc(昨日)),
  rank_5=rank(desc(近1周)),
  rank_20=rank(desc(近1月)),
  rank_61=rank(desc(近3月)),
  rank_122=rank(desc(近6月)),
  rank_244=rank(desc(近1年))
)%>%filter(
  rank_244<0.2*INDEX_CNT,
  rank_122<0.2*INDEX_CNT,
  rank_61<0.3*INDEX_CNT,
  rank_20<0.3*INDEX_CNT,
  rank_5<0.5*INDEX_CNT
)%>%arrange(rank_1)

short_index=showindex%>%mutate(
  rank_1=rank(desc(昨日)),
  rank_5=rank(desc(近1周)),
  rank_20=rank(desc(近1月)),
  rank_61=rank(desc(近3月)),
  rank_122=rank(desc(近6月)),
  rank_244=rank(desc(近1年))
)%>%filter(
  rank_61<0.5*INDEX_CNT,
  rank_5<0.2*INDEX_CNT,
  rank_1<0.1*INDEX_CNT
)%>%arrange(rank_1)


funds_rank=dbGetQuery(con,'select * from MutualFundRank')%>%filter(资产规模>1)

funds_rank=funds_rank%>%mutate_at(vars(starts_with('近')),as.numeric)
funds_filter=funds_rank%>%filter(
  资产规模>5 & 资产规模<200 & (Sys.Date()-as.Date(起始期))>180&基金类型 %in% c('股票型','股票指数','混合型','债券型')
)%>%mutate(
  基金类别=case_when(基金类型 %in% c('债券型','股票指数')~基金类型,
                     grepl('生物',基金简称)|grepl('医药',基金简称)|grepl('医疗',基金简称)|grepl('健康',基金简称)~'医疗保健',
                     grepl('消费',基金简称)|grepl('白酒',基金简称)|grepl('食品',基金简称)|grepl('饮料',基金简称)~'消费',
                     grepl('科技',基金简称)|grepl('创新',基金简称)|grepl('互联网',基金简称)|grepl('金融',基金简称)|grepl('银行',基金简称)~'金融科技',
                     grepl('基建',基金简称)|grepl('一带一路',基金简称)|grepl('地产',基金简称)|grepl('能源',基金简称)|grepl('环保',基金简称)~'基建',
                     T~'其他'
                     )
)%>%group_by(基金类别)%>%mutate(
  rank_1=ntile(desc(近1月),10),
  rank_6=ntile(desc(近6月),10),
  rank_12=ntile(desc(近1年),10)
)%>%filter(rank_6<3&rank_12<2&rank_1<5 | 基金类别 %in% c('金融科技','消费','医疗保健','基建'))


funds_list=funds_filter%>%mutate(rank_6月=rank(desc(近6月)))%>%filter(rank_6月<=30)%>%ungroup()
funds_info=split(funds_list,funds_list$基金类别)


funds_code=split(funds_list$基金代码,funds_list$基金类别)
funds_ret=lapply(funds_code,function(x){
  Data=lapply(x,function(x) {
    temp=dbGetQuery(con,sprintf("select 净值日期,日增长率,单位净值,累计净值,分红送配 from MutualFundHistory where 基金代码='%s' order by 净值日期 desc limit 122",x))
    temp=temp%>%mutate(
      日增长率=gsub('%','',日增长率)%>%as.numeric()
    )
    ret=as.xts(temp[,2]/100,order.by =as.POSIXct( as.character(temp$净值日期),tz=''))
  })
  ret=do.call(cbind,Data)
  colnames(ret)=x
  ret
  }
)

fund.performance.chart<-function(type='股票指数'){
    tempinfo=funds_info[[type]]
    tempret=funds_ret[[type]]
    
    temp=tempinfo%>%select(.data$日期,.data$基金类别,.data$基金代码:.data$基金简称,.data$基金经理:.data$资产规模,
                      .data$日增长率:.data$成立来
                      )
    ret_eval=Return.cumulative(tempret
    )%>%rbind(maxDrawdown(tempret)
    )%>%rbind(AverageDrawdown(tempret)
    )%>%rbind(round(SharpeRatio.annualized(tempret,Rf=0.04/252),1))
    
    ret_eval=data.frame(t(ret_eval))
    colnames(ret_eval)=c('期间收益','最大回撤','平均回撤','夏普值')
    ret_eval$code=rownames(ret_eval)
    temp=temp%>%left_join(ret_eval,by=c('基金代码'='code'))
    
    result=temp%>%mutate_at(vars(starts_with('近')),
                          function(x) { scales::percent(x/100,accuracy = 0.1) }
                          )%>%
                mutate_at(c('任职回报','日增长率','今年来','成立来'),
                          function(x) { scales::percent(as.numeric(x)/100,accuracy = 0.1) }
                        )%>%
                mutate_at(c('期间收益','最大回撤','平均回撤'),
                          function(x) { scales::percent(x,accuracy = 0.1) }
                        )%>%
                mutate_at('资产规模',function(x)paste0(x,'亿'))
    result=result%>%filter(rank(desc(夏普值),ties.method = 'first')<6)
    
    return(list(result=result,ret=tempret))
  
}

fund.plot.data=lapply(names(funds_info),fund.performance.chart)

summary_table=do.call(rbind,lapply(fund.plot.data,function(x){
  temp=x$result
  kongbai=temp[1,]
  kongbai[,3:ncol(kongbai)]=''
  rbind(kongbai,temp)
}))

png("DailyReport.png", height = 1000, width = 1600)
grid.arrange(tableGrob(summary_table,row=NULL))
dev.off()
# 

