library(RCurl)
library(rvest)
library(rjson)
library(stringr)
library(xml2)
library(SIT)
library(data.table)
load('group.Rdata')
getStock.report<-function(time){
  url='http://datainterface.eastmoney.com/EM_DataCenter/JS.aspx?type=SR&sty=YJBB&fd=time&st=13&sr=-1&p=1&ps=4000'
  #times=paste0(year,c('-03-31','-06-30','-09-30','-12-31'))
  #time=times[season]
  url=sub('time',time,url)
  curl=getCurlHandle()
  tryCatch(
    txt<-getURL(url=url, curl = curl,encoding = "UTF-8"),
    error=function(e){cat("ERROR :",year,"\n")
      Sys.sleep(1)
    }
  )
  txt<-sub('\\(','',txt)
  txt<-sub('\\)','',txt)
  temp<-fromJSON(txt)
  temp<-do.call(rbind,strsplit(temp,','))
  data<-as.data.frame(temp)
  colnames(data)<-c('股票代码','股票简称','每股收益','扣非每股收益','营业收入','同比增长','环比增长',
                    '净利润','同比','环比','每股净资产','净资产收益率','每股经营现金流量',
                    '销售毛利率','利润分配','股息率','公告日期','报告期','类别')
  return(data)
  
}




industry_code<-list(
  '有色金属'='BK0478',
  '钢铁行业'='BK0479',
  '电子信息'='BK0447',
  '港口水运'='BK0450',
  '水建泥材'='BK0424',
  '文化传媒'='BK0486',
  '园林工程'='BK0726',
  '珠宝首饰'='BK0734',
  '交运物流'='BK0422',
  '化纤行业'='BK0471',
  '电信运营'='BK0736',
  '船舶制造'='BK0729',
  '装修装饰'='BK0725',
  '造纸印刷'='BK0470',
  '贵金属'='BK0732',
  '医疗行业'='BK0727',
  '工艺商品'='BK0440',
  '旅游酒店'='BK0485',
  '仪器仪表'='BK0458',
  '木业家具'='BK0476',
  '综合行业'='BK0539',
  '专用设备'='BK0910',
  '交运设备'='BK0429',
  '农药兽药'='BK0730',
  '包装材料'='BK0733',
  '航天航空'='BK0480',
  '文教休闲'='BK0740',
  '食品饮料'='BK0438',
  '环保工程'='BK0728',
  '化肥行业'='BK0731',
  '商业百货'='BK0482',
  '高速公路'='BK0421',
  '塑胶制品'='BK0454',
  '电力行业'='BK0428',
  '公用事业'='BK0427',
  '输配电气'='BK0457',
  '国际贸易'='BK0484',
  '金属制品'='BK0739',
  '纺织服装'='BK0436',
  '石油行业'='BK0464',
  '多元金融'='BK0738',
  '机械行业'='BK0545',
  '煤炭采选'='BK0437',
  '安防设备'='BK0735',
  '农牧饲鱼'='BK0433',
  '玻璃陶瓷'='BK0546',
  '软件服务'='BK0737',
  '通讯行业'='BK0448',
  '汽车行业'='BK0481',
  '材料行业'='BK0537',
  '酿酒行业'='BK0477',
  '家电行业'='BK0456',
  '医药制造'='BK0465',
  '保险'='BK0474',
  '化工行业'='BK0538',
  '工程建设'='BK0425',
  '银行'='BK0475',
  '券商信托'='BK0473',
  '电子元件'='BK0459',
  '房地产'='BK0451',
  '全行业'=''

  )

get_industry<-function(industry){
  code<-paste0(industry_code[industry],'1')
  url<-'http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?type=CT&cmd=C.code&sty=FCOIATA&sortType=C&sortRule=-1&page=1&pageSize=300&js=var%20quote_123%3d{rank:[(x)],pages:(pc)}&token=7bc05d0d4c3c22ef9fca8c2a912d779c&jsName=quote_123&_g=0.47742817551160877'
  url<-sub("code",code,url)
  curl=getCurlHandle()
  tryCatch(
    txt<-getURL(url=url, curl = curl,encoding = "UTF-8"),
    error=function(e){cat("ERROR :",year,"\n")
      Sys.sleep(1)
    }
  )
  txt<-sub('var.*rank:','',txt)
  txt<-sub(',pages.x','',txt)
  temp<-fromJSON(txt)
  temp<-do.call(rbind,strsplit(temp,','))
  temp<-as.data.table(temp[,c(2,3)])
  colnames(temp)<-c('股票代码','股票简称')
  return(temp)
}

#http://finance.sina.com.cn/stock/sl/#qmxindustry_1
get_sina_group<-function(){
  type=c('地域','行业','概念','新浪行业')
  url_set=list('地域'='http://money.finance.sina.com.cn/q/view/newFLJK.php?param=area',
  '行业'='http://money.finance.sina.com.cn/q/view/newFLJK.php?param=industry',
  '概念'='http://money.finance.sina.com.cn/q/view/newFLJK.php?param=class',
  '新浪行业'="http://vip.stock.finance.sina.com.cn/q/view/newSinaHy.php"
  )
  f<-function(url){
  tryCatch(
    #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
    web<-read_html(url,encoding = 'GBK'),
    error=function(e){cat("ERROR :","","\n")
      Sys.sleep(1)
    }
  )
  txt<-web%>% html_text()
  txt<-sub('var.*=','',txt)
  temp<-fromJSON(txt)
  temp<-temp[-len(temp)]
  temp<-do.call(rbind,sapply(temp,strsplit,','))
  temp<-as.data.frame(temp[,2])
  colnames(temp)='类别'
  return(temp)
  }
  return(lapply(url_set,f))
}

#group<-get_sina_group()








get_industry_market<-function(){
  
}

get_area_market<-function(){
  
}

get_gainian_market<-function(){
  
}

get_forecast<-function(){
  url='http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?type=CT&cmd=C._A&sty=GEMCPF&st=(AllNum)&sr=-1&p=1&ps=4000&cb=&js=var%20NUPsMPsr={%22data%22:[(x)],%22pages%22:%22(pc)%22}&token=3a965a43f705cf1d9ad7e1a3e429d622&rt=50709553'
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
  
  data=as.data.frame(data[,-1],row.names = 1:nrow(data))
  col=c('股票代码','名称','最新价','涨跌额','涨跌幅','振幅','成交量(手)','成交额','昨收','今开','最高','最低','五分钟涨跌','量比','换手率','市盈率','上市日期')
  colnames(data)=col
  #data['市值']=as.numeric(data['成交额'])/as.numeric(data['换手率'])*100
  data
  
}