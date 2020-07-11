
library(RCurl)
library(rvest)
library(parallel)
library(SIT)
library(shiny)
library(ggplot2)
library(foreach)
myheader<-c(
  'Host'='sh.lianjia.com',
  'User-Agent'= 'Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:60.0) Gecko/20100101 Firefox/60.0',
  'Accept'= 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
  'Accept-Language'= 'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2',
  'Accept-Encoding'= 'gzip, deflate, br',
  'Cookie'= '_ga=GA1.3.1956483913.1473815618; lianjia_uuid=99949991-e9c7-4df8-b8bc-7d427ccf98ad; _ga=GA1.2.1956483913.1473815618; gr_user_id=84a5ae61-9280-4505-b0a2-ebdca807fe32; ubta=2299869246.1101253263.1473815618645.1514905713083.1514905850541.685; pt_393d1d99=uid=El7-zd3kPbVUnu-qmjjIng&nid=0&vid=XXZP-lAL0HshXLVn1LRwnw&vn=8&pvn=1&sact=1490063182175&to_flag=0&pl=B9AAHlexPUe/888zzCpAYw*pt*1490063143964; _smt_uid=57f5eb50.334841a2; _jzqa=1.1393651109478892300.1475901567.1522833633.1527400665.6; hjstat_uv=3232933115583413487|679544; __xsptplus696=696.3.1514905571.1514905582.2%231%7Cbaidu%7Cpinzhuan%7CMain%7C%25E9%2593%25BE%25E5%25AE%25B6%7CTitle%23%23z1NPFZXjx6TCz3-IfObGsx5bEOQGZX1N%23; Hm_lvt_9152f8221cb6243a53c83b956842be8a=1527400663,1527400684; UM_distinctid=1628ff52f0434b-09e6615794f1568-17347a40-e1000-1628ff52f06fc; CNZZDATA1253492439=1895444057-1522828834-%7C1527398622; CNZZDATA1254525948=1544018049-1522828377-%7C1527398324; CNZZDATA1255633284=315402030-1522833120-%7C1527399327; CNZZDATA1255604082=1734163783-1522833282-%7C1527396536; _qzja=1.172512131.1522833633464.1522833633464.1527400669958.1527401001562.1527401009120.0.0.0.18.2; lianjia_token=2.001783a1ba6e98d723062e888b5028a2e6; ljref=pc_sem_baidu_ppzq_x; select_city=310000; all-lj=ed5a77c9e9ec3809d0c1321ec78803ae; lianjia_ssid=7a360fce-aca6-4cdd-9ac3-a7062dde852a; Hm_lpvt_9152f8221cb6243a53c83b956842be8a=1527401008; _jzqb=1.9.10.1527400665.1; _jzqc=1; _jzqy=1.1527400665.1527400665.1.jzqsr=baidu|jzqct=%E9%93%BE%E5%AE%B6.-; _jzqckmp=1; _qzjb=1.1527400669957.9.0.0.0; _qzjc=1; _qzjto=9.1.0; _gid=GA1.2.765752956.1527400672',
  'Connection'= 'keep-alive',
  'Upgrade-Insecure-Requests'='1',
  'Cache-Control'= 'max-age=0'
)
url='https://sh.lianjia.com/ershoufang/putuo/'
lianjia.onsite<-function(url){
  d <- debugGatherer()
  web<-getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  web<-read_html(web)
  textlist=web %>% html_nodes('li.clear')
  # for (txt in textlist)
  #   get_info(txt)
  get_info=function(txt){
    houseInfo=txt %>% html_nodes('div.houseInfo')%>% html_text()
    name=spl(houseInfo,'[|]')[1]
    huxing=spl(houseInfo,'[|]')[2]
    space=spl(houseInfo,'[|]')[3]
    chaoxiang=spl(houseInfo,'[|]')[4]
    zhuangxiu=spl(houseInfo,'[|]')[5]
    lift=ifelse(length(spl(houseInfo,'[|]'))==6,spl(houseInfo,'[|]')[6],"")
    positionInfo=txt%>% html_nodes('div.positionInfo')%>% html_text(trim=TRUE)
    position=spl(positionInfo,'-')[1]
    region=spl(positionInfo,'-')[2]
    followInfo= txt %>% html_nodes('div.followInfo') %>% html_text(trim=TRUE)
    subway= txt%>%html_nodes('div.tag span.subway')%>%html_text(trim=TRUE)
    if (len(subway)==0)
      subway=''
    taxfee=txt%>%html_nodes('div.tag span.taxfree')%>%html_text(trim=TRUE)
    if (len(taxfee)==0)
      taxfee=''
    haskey= txt%>% html_nodes('div.tag span.haskey')%>%html_text(trim=TRUE)
    if (len(haskey)==0)
      haskey=''
    totalprice= txt%>% html_nodes('div.totalPrice')%>%html_text(trim=TRUE)
    unitprice= txt%>% html_nodes('div.unitPrice')%>%html_text(trim=TRUE)
    link=txt%>%html_nodes('div.title a')%>%html_attr('href')
    tempdata=data.frame(name,huxing,space,chaoxiang,zhuangxiu,lift,position,region,subway,taxfee,
                        haskey,totalprice,unitprice,link,followInfo)
    return(tempdata)
  }
  data=do.call(rbind,lapply(textlist,get_info))
  data
}
url='https://sh.lianjia.com/chengjiao/songjiang/'
lianjia.chengjiao<-function(url){
  #d <- debugGatherer()
  #web<-getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  web<-read_html(url)
  textlist=web %>% html_nodes('ul.listContent li')
  get_detail<-function(txt){
    region=''
    district=''
    houseInfo=txt %>% html_nodes('div.title')%>% html_text(trim=TRUE)
    name=spl(houseInfo,' ')[1]
    huxing=spl(houseInfo,' ')[2]
    space=spl(houseInfo,' ')[3]
    address=txt %>% html_nodes('div.houseInfo')%>% html_text(trim=TRUE)
    chaoxiang=spl(address,'[|]')[1]
    zhuangxiu=spl(address,'[|]')[2]
    lift=ifelse(length(spl(address,'[|]'))==3,spl(address,'[|]')[3],"")
    position=txt%>% html_nodes('div.positionInfo')%>% html_text(trim=TRUE)
    HouseInfo= txt %>% html_nodes('span.dealHouseTxt') %>% html_text(trim=TRUE)
    if (len (HouseInfo)==0)
      HouseInfo=""
    dealInfo= txt %>% html_nodes('span.dealCycleTxt') %>% html_text(trim=TRUE)
    dealdate=txt%>%html_nodes('div.dealDate')%>%html_text(trim=TRUE)
    totalprice= txt%>% html_nodes('div.totalPrice')%>%html_text(trim=TRUE)
    unitprice= txt%>% html_nodes('div.unitPrice')%>%html_text(trim=TRUE)
    link=txt%>%html_nodes('div.title a')%>%html_attr('href')
    tempdata=data.frame(name,district,region,huxing,space,chaoxiang,zhuangxiu,lift,position,dealInfo,dealdate,
                        HouseInfo,totalprice,unitprice,link)
    return(tempdata)
  }
  data=do.call(rbind,lapply(textlist,get_detail))
  if (!is.null(data)){
    region=web%>%html_nodes('dl dd div div a.selected')%>% html_text(trim=TRUE)
    #region=spl(url,'pg')[1]
    data['district']=region[1]
    data['region']=region[2]
  }
  data
}
do.chengjiao.spider<-function(url,num){
  if(as.numeric(num)==0)
    return(NULL)
  pages<-ceiling(as.numeric(num)/20)
  Data<-NULL
  for(page in 1:pages){
    new_url<-paste0(url,'pg',page,'/')
    data<-tryCatch(lianjia.chengjiao(new_url),
                   error=function(e) {
                     cat(new_url,'error','\n')
                   })
    Data<-rbind(Data,data)
  }
  cat('finish spidering',url,' ',num,'\n')
  return(Data)
}
spider.url<-function(url){
  # url<-'http://sh.lianjia.com/chengjiao/minhang/'
  # d <- debugGatherer()
  # web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  url<-as.character(url)
  txt<-read_html(url)
  temp<-txt %>% html_nodes('dl dd div div')
  link<-paste0("http://sh.lianjia.com",temp[[2]]%>% html_nodes('a') %>%html_attr('href'))
  link.name<-temp[[2]]%>% html_nodes('a') %>%html_text()
  data<-as.data.frame(cbind(link.name,link)[-1,])
  return(data)
}
initial.sold<-function(){
  url="https://sh.lianjia.com/chengjiao/"
  d <- debugGatherer()
  web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  txt<-read_html(web)
  temp<-txt %>% html_nodes('dl dd div div')
  link<-paste0("http://sh.lianjia.com",temp[[1]]%>% html_nodes('a') %>%html_attr('href'))
  link.name<-temp[[1]]%>% html_nodes('a') %>%html_text()
  data<-as.data.frame(cbind(link.name,link))
  detail.data<-lapply(data[,2],spider.url)
  dt<-unique.data.frame(do.call(rbind,detail.data))
  spider.info<-function(url){
    #d <- debugGatherer()
    #web=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
    url<-as.character(url)
    txt<-read_html(url)
    temp <- txt%>% html_nodes('div.total span') %>% html_text(trim=TRUE)
    #temp <- gsub('\n','',gsub('\t','',temp))
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
#
initial.info=initial.sold()
save(initial.info,file='initial.Rdata')

fun<-function(x){
  do.call(rbind,apply(x,1,function(y){
    do.chengjiao.spider(as.character(y[2]),as.numeric(y[3]))
  }))
}
# x=split.data.frame(initial$small,1:100)
# newdata<-foreach(i=1:100,.combine=rbind) %do% fun(x[[i]])
#

# rownames(newdata)=seq(1:nrow(newdata))
# write.csv(newdata,file='sold.csv',row.names = FALSE)
gun<-function(x){
  do.call(rbind,apply(x,1,function(y){
    do.chengjiao.spider(as.character(y[2]),as.numeric(y[4]))
  }))
}
update.chengjiao<-function(){
  initial=initial.sold()
  initial$small[,'数']=round(as.numeric(as.character(initial$small[,'数量']))*0.1,0)
  newdata=data.frame()
  for(i in 1:nrow(initial$small)){
    ul=initial$small[i,'链接']
    num=initial$small[i,'数']
    new=try(do.chengjiao.spider(as.character(ul),as.numeric(num)),silent=T)
    if('try-error' %in% class(new)){
      next
    }else{
      newdata=rbind(newdata,new)
    }
  }
  write.csv(newdata,file='new.csv',row.names =FALSE)
}
#write.csv(newdata,file='new.csv',row.names =FALSE)
# data=read.csv('formatsold.csv')
# col=c('小区','区域','街道','户型','面积','朝向','装修','电梯','楼层','地铁','交易信息','总价','单价','成交日期','楼龄','楼高','建筑年代','经度','维度')
# colnames(data)=col
# data[,c('总价','单价','经度','维度')]=sapply(data[,c('总价','单价','经度','维度')],as.numeric)
# save(data,file='historysold.Rdata')

update.chengjiao()