library(RCurl)
library(rvest)
library(parallel)
library(SIT)
library(shiny)
library(ggplot2)
library(foreach)
library(data.table)
library(rpivotTable)
library('leaflet')
#load('initial.Rdata')



# GET /chengjiao/d2 HTTP/1.1
# Host: sh.lianjia.com
# User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:57.0) Gecko/20100101 Firefox/57.0
# Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
# Accept-Language: zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2
# Accept-Encoding: gzip, deflate
# Referer: http://sh.lianjia.com/chengjiao/
# Cookie: aliyungf_tc=AQAAAJo7nEmlggUAkoX3OpHy7C2M1CV2; select_city=310000; cityCode=sh; lianjia_uuid=db942792-9bdb-408e-aaab-049f934323c2; gr_user_id=dfea307c-4602-4a48-aaad-af0a28a0becc; gr_session_id_970bc0baee7301fa=44013f8d-e2db-4ca1-b300-49b52eb37b4d; __xsptplusUT_696=1; lianjia_ssid=8d939e3e-d2d0-04db-e240-06e0d41088c6; ubt_load_interval_b=1516713045681; ubt_load_interval_c=1516713045681; _ga=GA1.2.134630505.1516712927; _gat=1; _gat_u=1; ubta=2299869246.2497280764.1516712927511.1516713046417.1516713055394.3; ubtb=2299869246.2497280764.1516713055395.9205699F988AE7035D5DB6BD3540B504; ubtc=2299869246.2497280764.1516713055395.9205699F988AE7035D5DB6BD3540B504; ubtd=3; __xsptplus696=696.1.1516712927.1516713045.3%234%7C%7C%7C%7C%7C%23%23SviAfL2hUD1rIRuzDgVca7ajZGqTzXuY%23; lianjia_token=2.005ce21b2425f96dbd4d4f32156907bb67; lianjia_userId=2000000008395637; gr_cs1_44013f8d-e2db-4ca1-b300-49b52eb37b4d=userid%3A2000000008395637
# Connection: keep-alive
# Upgrade-Insecure-Requests: 1

# myheader<-c("Host"= "sh.lianjia.com",
#             "User-Agent"=" Mozilla/5.0 (Windows NT 6.1; WOW64; rv:57.0) Gecko/20100101 Firefox/57.0",
#             "Accept"= "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
#             "Accept-Language"="zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2",
#             "Accept-Encoding"= "gzip, deflate",
#             "Referer"="http://sh.lianjia.com/chengjiao/",
#             "Cookie"="aliyungf_tc=AQAAAJo7nEmlggUAkoX3OpHy7C2M1CV2; select_city=310000; cityCode=sh; lianjia_uuid=db942792-9bdb-408e-aaab-049f934323c2; gr_user_id=dfea307c-4602-4a48-aaad-af0a28a0becc; gr_session_id_970bc0baee7301fa=44013f8d-e2db-4ca1-b300-49b52eb37b4d; __xsptplusUT_696=1; lianjia_ssid=8d939e3e-d2d0-04db-e240-06e0d41088c6; ubt_load_interval_b=1516713045681; ubt_load_interval_c=1516713045681; _ga=GA1.2.134630505.1516712927; _gat=1; _gat_u=1; ubta=2299869246.2497280764.1516712927511.1516713046417.1516713055394.3; ubtb=2299869246.2497280764.1516713055395.9205699F988AE7035D5DB6BD3540B504; ubtc=2299869246.2497280764.1516713055395.9205699F988AE7035D5DB6BD3540B504; ubtd=3; __xsptplus696=696.1.1516712927.1516713045.3%234%7C%7C%7C%7C%7C%23%23SviAfL2hUD1rIRuzDgVca7ajZGqTzXuY%23; lianjia_token=2.005ce21b2425f96dbd4d4f32156907bb67; lianjia_userId=2000000008395637; gr_cs1_44013f8d-e2db-4ca1-b300-49b52eb37b4d=userid%3A2000000008395637",
#             "Connection"="keep-alive",
#             "Upgrade-Insecure-Requests"="1")
myheader<-c(
'Host'= 'sh.lianjia.com',
'User-Agent'= 'Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0',
'Accept'= '*/*',
'Accept-Language'= 'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2',
'Accept-Encoding'= 'gzip, deflate, br',
'Referer'= 'https://sh.lianjia.com/chengjiao/pg2/',
'Cookie'= '_ga=GA1.3.1956483913.1473815618; lianjia_uuid=99949991-e9c7-4df8-b8bc-7d427ccf98ad; _ga=GA1.2.1956483913.1473815618; gr_user_id=84a5ae61-9280-4505-b0a2-ebdca807fe32; ubta=2299869246.1101253263.1473815618645.1514905713083.1514905850541.685; pt_393d1d99=uid=El7-zd3kPbVUnu-qmjjIng&nid=0&vid=XXZP-lAL0HshXLVn1LRwnw&vn=8&pvn=1&sact=1490063182175&to_flag=0&pl=B9AAHlexPUe/888zzCpAYw*pt*1490063143964; _smt_uid=57f5eb50.334841a2; _jzqa=1.1393651109478892300.1475901567.1476577009.1522833633.5; hjstat_uv=3232933115583413487|679544; __xsptplus696=696.3.1514905571.1514905582.2%231%7Cbaidu%7Cpinzhuan%7CMain%7C%25E9%2593%25BE%25E5%25AE%25B6%7CTitle%23%23z1NPFZXjx6TCz3-IfObGsx5bEOQGZX1N%23; select_city=310000; all-lj=eae2e4b99b3cdec6662e8d55df89179a; lianjia_ssid=fce08e4f-c0ce-43c4-9d1f-589b14d96dc7; Hm_lvt_9152f8221cb6243a53c83b956842be8a=1522833632; Hm_lpvt_9152f8221cb6243a53c83b956842be8a=1522833739; UM_distinctid=1628ff52f0434b-09e6615794f1568-17347a40-e1000-1628ff52f06fc; CNZZDATA1253492439=1895444057-1522828834-%7C1522828834; CNZZDATA1254525948=1544018049-1522828377-%7C1522833777; CNZZDATA1255633284=315402030-1522833120-%7C1522833120; CNZZDATA1255604082=1734163783-1522833282-%7C1522833282; _jzqb=1.6.10.1522833633.1; _jzqc=1; _jzqckmp=1; _qzja=1.172512131.1522833633464.1522833633464.1522833633464.1522833729768.1522833739366.0.0.0.6.1; _qzjb=1.1522833633464.6.0.0.0; _qzjc=1; _qzjto=6.1.0; _gid=GA1.2.1094543758.1522833639; lianjia_token=2.0067172f621e0c59fb76ba065392ed71c4',
'Connection'= 'keep-alive'
)
#url='http://sh.lianjia.com/xiaoqu/d2'

#url='http://sh.lianjia.com/ershoufang/d7'


initial.sell<-function(){
  url="http://sh.lianjia.com/ershoufang/d1"
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  temp<-txt %>% html_nodes('div.level1')
  link<-paste0("http://sh.lianjia.com",temp[[1]]%>% html_nodes('a') %>%html_attr('href'))
  link.name<-temp[[1]]%>% html_nodes('a') %>%html_text()
  data<-as.data.frame(cbind(link.name,link)[-1,])
  detail.data<-lapply(data[,2],spider.url)
  dt<-do.call(rbind,detail.data)
  
  spider.info<-function(url){
    d <- debugGatherer()
    web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
    txt<-read_html(web)
    temp <- txt%>% html_nodes('ul.content')%>% html_nodes('span.num') %>% html_text()
    temp <- gsub('\n','',gsub('\t','',temp))
    return(temp)
  }
  
  data.num<-lapply(data[,2],spider.info)
  dt.num<-lapply(dt[,2],spider.info)
  
  initial.info<-list()
  temp<-cbind(dt,do.call(rbind,dt.num))
  colnames(temp)<-c('街道','链接','出售中','90天成交','昨日带看')
  initial.info$small<-temp
  temp<-cbind(data,do.call(rbind,data.num))
  colnames(temp)<-c('区域','链接','出售中','90天成交','昨日带看')
  initial.info$big<-temp
  return(initial.info)
}

initial.sold<-function(){
  url="http://sh.lianjia.com/chengjiao/d1"
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  temp<-txt %>% html_nodes('div.level1')
  link<-paste0("http://sh.lianjia.com",temp[[1]]%>% html_nodes('a') %>%html_attr('href'))
  link.name<-temp[[1]]%>% html_nodes('a') %>%html_text()
  data<-as.data.frame(cbind(link.name,link)[-1,])
  detail.data<-lapply(data[,2],spider.url)
  dt<-do.call(rbind,detail.data)
  spider.info<-function(url){
    d <- debugGatherer()
    web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
    txt<-read_html(web)
    temp <- txt%>% html_nodes('span.result-count') %>% html_text()
    temp <- gsub('\n','',gsub('\t','',temp))
    return(temp)
  }
  
  data.num<-lapply(data[,2],spider.info)
  dt.num<-lapply(dt[,2],spider.info)
  
  initial.info<-list()
  temp<-cbind(dt,do.call(rbind,dt.num))
  colnames(temp)<-c('街道','链接','数量')
  initial.info$small<-temp
  temp<-cbind(data,do.call(rbind,data.num))
  colnames(temp)<-c('区域','链接','数量')
  initial.info$big<-temp
  return(initial.info)
}


spider.url<-function(url){
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  temp<-txt %>% html_nodes('div.level2-item')
  link<-paste0("http://sh.lianjia.com",temp%>% html_nodes('a') %>%html_attr('href'))
  link.name<-temp%>% html_nodes('a') %>%html_text()
  data<-as.data.frame(cbind(link.name,link)[-1,])
  return(data)
}




lianjia.onsite<-function(url){
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  
  temp<-txt %>% html_nodes('div.info-col') %>% html_text()
  price1<-gsub('|','',gsub('\n','',gsub('\t','',temp)))
  
  temp<-txt %>% html_nodes('span.price-item') %>% html_text()
  price2<-gsub('|','',gsub('\n','',gsub('\t','',temp)))
  
  # temp<-txt %>% html_nodes('span.row1-text') %>% html_text()
  # temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')
  # temp1<-do.call(rbind,lapply(temp,function(x){iif(len(x)==5,x,c(x,rep('',5-len(x))))}))
  
  temp<-txt %>% html_nodes('div.row1-text') %>% html_text()
  temp<-strsplit(gsub(' ','',gsub('\n','',gsub('\t','',temp))),'\\|')
  temp1<-do.call(rbind,lapply(temp,function(x){
    if(len(x)==3)
      return(x)
    else{
      temp=c('','','')
      if(any(grepl('层',x)))
        temp[1]=x[grepl('层',x)]
      if(any(grepl('朝',x)))
        temp[2]=x[grepl('朝',x)]
      if(any(grepl('装',x) | grepl('毛坯',x)))
        temp[3]=x[grepl('装',x) | grepl('毛坯',x)]
    }
    return(temp)
  }
  ))
  
  temp<-txt %>% html_nodes('span.row2-text') %>% html_text()
  temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')
  temp2<-do.call(rbind,lapply(temp,function(x){iif(len(x)==4,x,c(x,rep('',4-len(x))))}))
  
  link<-txt %>% html_nodes('a.js_fanglist_title') %>% html_attr('href')
  
  temp<-txt %>% html_nodes('div.property-tag-container')%>% html_text()
  temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')

  temp3<-do.call(rbind,temp)
  
  data<-as.data.frame(cbind(temp2,temp1,price1,price2,temp3,link))[,-6]
  colnames(data)=c('小区','区域','街道','建造时间','房型','大小',
                   '楼层','朝向','总价','单价','备注','链接')
  
  return(data)
}


#url='http://sh.lianjia.com/chengjiao/jingan/d10'
url='https://sh.lianjia.com/chengjiao/jingan/pg2/'
lianjia.chengjiao<-function(url){
  Sys.sleep(1)
  url<-gsub(' ','',url)
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  
  
  temp<-txt %>% html_nodes('div.info-col') %>% html_text()
  price1<-gsub('|','',gsub('\n','',gsub('\t','',temp)))
  price<-rbind(NULL,matrix(price1,ncol=6,byrow = T)[,c(3,4,6)])
  
  temp<-txt %>% html_nodes('div.info-row') %>%html_nodes('a.link-hover-green') %>% html_text()
  link<-txt %>% html_nodes('div.info-row') %>% html_nodes('a.link-hover-green') %>% html_attr('href')
  temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')
  temp0<-do.call(rbind,lapply(temp,function(x){iif(len(x)==3,x,c(x,rep('',3-len(x))))}))
  
  
  temp<-txt %>% html_nodes('div.row1-text') %>% html_text()
  temp<-strsplit(gsub(' ','',gsub('\n','',gsub('\t','',temp))),'\\|')
  temp1<-do.call(rbind,lapply(temp,function(x){
    if(len(x)==3)
      return(x)
    else{
     temp=c('','','')
     if(any(grepl('层',x)))
       temp[1]=x[grepl('层',x)]
     if(any(grepl('朝',x)))
       temp[2]=x[grepl('朝',x)]
     if(any(grepl('装',x) | grepl('毛坯',x)))
       temp[3]=x[grepl('装',x) | grepl('毛坯',x)]
    }
    return(temp)
  }
  ))
  
  temp<-txt %>% html_nodes('span.row2-text') %>% html_text()
  temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')
  temp2<-do.call(rbind,lapply(temp,function(x){iif(len(x)==2,x,c(x,rep('',2-len(x))))}))
  
  temp<-txt %>% html_nodes('div.property-tag-container')%>% html_text()
  temp<-strsplit(gsub('\\|','',gsub('\n','',gsub('\t','',temp))),' ')
  temp3<-do.call(rbind,lapply(temp,function(x){
    if(length(x)!=0 & any(grepl('距离',x)))
      return(x[grep('距',x)])
    else
      return('')
  }))
  #temp3<-do.call(rbind,temp)
  
  data<-as.data.frame(cbind(temp0,temp2,temp1,price,temp3,link))
  colnames(data)<-c('小区','房型','大小','区域','街道','楼层',
                    '朝向','装修','成交日期','总价','单价','备注','链接')
  return(data)
}

do.chengjiao.spider<-function(url,num){
  if(as.numeric(num)==0)
    return(NULL)
  pages<-ceiling(as.numeric(num)/20)
  Data<-NULL
  for(page in 1:pages){
    new_url<-paste0(url,'d',page)
    data<-tryCatch(lianjia.chengjiao(new_url),
                   error=function(e) {
                     cat(new_url,'error','\n')
                   })
    Data<-rbind(Data,data)
  }
  cat('finish spidering',url,' ',num,'\n')
  return(Data)
}

fun<-function(x){
  do.call(rbind,apply(x,1,function(y){
    do.chengjiao.spider(as.character(y[2]),as.numeric(y[3]))
  }))
}


updatedata<-function(){
  load('historysold.Rdata')
  initial<-initial.sold()
  x=split.data.frame(initial$small,1:100)
  newdata<-foreach(i=1:100,.combine=rbind) %do% fun(x[[i]])
  m=levels(newdata[,'房型'])
  map<-function(x,m){
    if(any(grepl(x,m)))
      return(m[grep(x,m)])
    else
      return(paste0(' ',x))
  }
  data[,'房型']<-sapply(data[,'房型'],map,m)
  newdata['成交日期']=sapply(newdata['成交日期'],as.Date,"%Y.%m.%d")
  newdata['成交日期']=as.Date(newdata[,'成交日期'],origin='1970-01-01')
  dt<-rbind(data,newdata)
  data<-dt[!duplicated(dt[,13]),]
  return(data)
}



#data=updatedata()
# save(data,file='historysold.Rdata')
# 
# 
# 
# data=read.csv("historysold.csv")


