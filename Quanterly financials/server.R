library(data.table)
library(formattable)
library(DT)
library(shinymanager)
server<-function(input, output,session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = "/home/dwhang/project/dwhang/Shiny_module/database.sqlite",
      passphrase = 'z00dawei'
    ), timeout = 0
  )
  
  load('data.Rdata')
  get_stock<-function(code,page=1){
    url='http://vip.stock.finance.sina.com.cn/quotes_service/api/json_v2.php/Market_Center.getHQNodeData?page=pg&num=150&sort=symbol&asc=1&node=code&symbol=&_s_r_a=page'
    url=sub('code',code,url)
    url=sub('pg',page,url)
    tryCatch(
      #txt<-getURL(url=url, curl = curl,encoding = "gbk"),
      web<-read_html(url,encoding = 'GBK'),
      error=function(e){cat("ERROR :","","\n")
        Sys.sleep(1)
      }
    )
    #print(txt)
    txt<-web%>% html_text(trim=TRUE)
    if(!is.null(txt)){
      temp<-strsplit(txt,',')[[1]]
      id<-temp[grepl('code',temp)]
      name<-temp[grepl('name',temp)]
      id<-gsub('\"','',sub('code:','',id))
      name<-gsub('\"','',sub('name:','',name))
      id<-as.data.table(cbind(id,name))
      colnames(id)<-c('股票代码','股票简称')
      id
    }
  }
  
  get_stocks<-function(code){
    temp=get_stock(code)
    Data=temp
    page=1
    while(nrow(temp)==100){
      page=page+1
      temp=get_stock(code,page)
      Data=rbind(Data,temp)
    }
    Data
  }
  
  
  get_data<-reactive({
   store<-ls(data)
   if(input$time %in% store & input$time!=ls(data)[len(ls(data))]){  
     return(data[[input$time]])
   }else{
     data[[input$time]]=getStock.report(input$time)
     save(data,file='data.Rdata')
     return(data[[input$time]])
   }
  })
  
  get_equity_info<-function(){
    url='http://nufm.dfcfw.com/EM_Finance2014NumericApplication/JS.aspx?type=CT&cmd=C._A&sty=FCOIATA&sortType=(ChangePercent)&sortRule=-1&page=1&pageSize=4000&js=var%20hxfkDIfQ={rank:[(x)],pages:(pc),total:(tot)}&token=7bc05d0d4c3c22ef9fca8c2a912d779c&jsName=quote_123&_g=0.628606915911589&_=1520759989849'
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
    data=as.data.frame(data[,c(2:13,22:26)],row.names = 1:nrow(data))
    col=c('股票代码','名称','最新价','涨跌额','涨跌幅','振幅','成交量(手)','成交额','昨收','今开','最高','最低','五分钟涨跌','量比','换手率','市盈率','上市日期')
    colnames(data)=col
    #data['市值']=as.numeric(data['成交额'])/as.numeric(data['换手率'])*100
    data
    
  }
  
  observeEvent(input$group,{
    temp<-group[[input$group]]
    updateSelectInput(session,
                      'industry',strong('分类选择'),c(as.character(temp[,1]),'全部'),'全部')
    
  })
  
  
  
  get_industry_set<-function(){
      temp=group[[input$group]]
      if(input$industry!='全部'){
      code<-rownames(temp)[which(as.character(temp[,1])==input$industry)]
      id<-get_stocks(code)
      }else{
      id=NULL
      }
      return(id)
  }
  
  get_marketdata<-reactive({
    marketdata<-get_equity_info()
    marketdata
  })
  
  formatdata<-reactive({
    dt<-get_data()
    marketdata<-get_marketdata()
    dt<-merge(dt,marketdata[,c("股票代码","市盈率")],all.x=TRUE)
    
    dt[,c(3:14,16,20)]=sapply(dt[,c(3:14,16,20)],function(x){as.numeric(as.character(x))})
    
    dt[,c(6,7,9,10,12,14,16)]=dt[,c(6,7,9,10,12,14,16)]/100
    dt
  })
  
  
  
  
  
  
  
  output$report<-renderDataTable({
    dt<-formatdata()
    id<-get_industry_set()
    if(!is.null(id)){
      dt<-merge(id,dt)
    }
    
    col=which((colnames(dt) %in% input$columns )==FALSE)-1
    
    dt=as.data.frame(dt)
    dt[,c('营业收入','净利润')]<-sapply(dt[,c('营业收入','净利润')],
                                 function(x){
                                   round(x/1e8,3)
                                 })
    dt%>%datatable(
    # container = sketch,
    extensions = 'Buttons',
    rownames = F,
    filter = 'top',
    class='cell-border stripe',
    caption=htmltools::tags$caption(
      style = 'caption-side: top;text-align:center;
      font-size:larger;color:black;font-weight:bold;'
      ,htmltools::withTags(
        h3('Company quarter report',style='font-weight:bold',align='center')
      )),
       options=list(
        buttons = c('copy', 'print','colvis'),
        initComplete = htmlwidgets::JS("function(settings, json) 
                          {$(this.api().table().header()).css({'font-size' : '15px'});}"),
        pageLength = 50, autoWidth = F,
        lengthMenu = c(15,25,50,100),
        dom='Blfrtip',
        autoWidth=T,
        columnDefs = list(list(className = 'dt-center',
                               targets = 0:(ncol(dt)-1)),
                          list(width=100,targets=0:(ncol(dt)-1)),
                          list(visible=FALSE, targets=col)
                          
        )
        )
        )%>% formatRound(c(3,4,11,13,20))%>%
             formatPercentage(c(6,7,9,10,12,14,16),2)%>%
             formatCurrency(c(5,8),'￥亿 ')
    
    
    
    }
  )
  
  
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      dt<-formatdata()
      id<-get_industry_set()
      if(!is.null(id)){
        dt<-merge(id,dt)
      }
      write.csv(dt, sep=',', col.names=NA, quote=F, file=file, append=T,fileEncoding ='GBK')      		
      
    }
  )	

}