library(rPython)
python.load('test.py')
path=getwd()
library(jiebaR)
library(wordcloud2)
library(stringr)
library(plyr)
library(readr)
library(RCurl)
library(rvest)
library(rjson)
library(readr)

#url='https://s.taobao.com/search?q=%E9%B1%BC%E6%B2%B9&imgfile=&commend=all&ssid=s5-e&search_type=item&sourceId=tb.index&spm=a21bo.2017.201856-taobao-item.1&ie=utf8&initiative_id=tbindexz_20170306&bcoffset=15&ntoffset=15&p4ppushleft=1%2C48&style=grid&s=0'

shinyServer(function(input, output){
  
  ##########Tmall############
  commentspider<-eventReactive(input$run,{
    url=input$url
    pages=input$pages
    
    web=read_html(url)%>%html_text()
    itemid=sub('itemId=','',str_match(web,'itemId=[0-9]*'))[1]
    sellerid=sub('sellerId=','',str_match(web,'sellerId=[0-9]*'))[1]
    file=paste0(itemid,'.csv')
    filelist=list.files(paste0(path,'/Data'))
    # if (is.na(itemid) | is.na(sellerid)){
    #   web=read_html(url)%>%as.character()
    #   itemid=sub('itemId=','',str_match(web,'itemId=[0-9]*'))[1]
    #   sellerid=sub('sellerId=','',str_match(web,'sellerId=[0-9]*'))[1]
    # }
    if (!file %in% filelist){
      a=try(python.call('get_TM_item_pinglun',itemid,sellerid,pages,file))
      print(a)
      data=try(read.csv(paste0(path,'/Data/',file),encoding = 'utf-8'),silent = T)
    }else{
      data=read.csv(paste0(path,'/Data/',file),encoding = 'utf-8')
    }
   return(data)
  })
  
  get_comment<-reactive({
    url=input$url
    pages=input$pages
    itemid=sub('itemId=','',str_match(url,'itemId=[0-9]*'))[1]
    if(is.na(itemid)){
      itemid=sub('id=','',str_match(url,'id=[0-9]*'))[1]
    }
    if(!is.na(itemid)){
      file=paste0(itemid,'.csv')
      filelist=list.files(paste0(path,'/Data'))
      data=try(read.csv(paste0(path,'/Data/',file),encoding = 'utf-8'),silent = T)
    }

    data
  })
  
  output$status<-renderUI({
    out = tryCatch(commentspider(), error=function( err ) paste(err))	    				
    if( class(out)=='try-error' ) 
      HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",'wrong urllink'))
    else
      HTML("<b>Status</b>: <b><font color='green'>Data gathering is Ok</font></b>")		
  })
  
  comment_summary<-eventReactive(input$recloud,{
    data=get_comment()
    if (class(data)!='try-errof'){
      cutter=worker()
      test=sapply(data[,'rateContent']%>% as.character(),segment,cutter)
      names(test)=seq(1,length(test))
      test=lapply(test,unique)
      stopwords=unlist(strsplit(input$stopwords,'、'))
      print(stopwords)
      filter_word=lapply(test,filter_segment,stopwords)
      filter_word=lapply(filter_word,str_trim)
      tableword=plyr::count(unlist(filter_word))
    }
  })
  
  output$cloud<-renderWordcloud2({
    tableword=comment_summary()
    wordcloud2(tableword, size = 2, fontFamily = "微软雅黑",
                 color = "random-light", backgroundColor = "grey")
    

  })
  
  output$data<-downloadHandler(
    filename = function() {
      url=input$url
      file=paste0(sub('itemId=','',str_match(url,'itemId=[0-9]*'))[1],'.csv')
    },
    content = function(file) {
      data=get_comment()
      write_excel_csv(data,file)


    }
  )
  
  ##########JD############
  commentspider_JD<-eventReactive(input$JDrun,{
    url=input$JDurl
    pages=input$JDpages
    file=paste0(sub('.html','',str_match(url,'[0-9]*.html')),'.csv')
    filelist=list.files(paste0(path,'/Data'))
    if (!file %in% filelist){
      a=try(python.call('get_JD_item_pinglun',url,pages,file))
      print(a)
      data=try(read.csv(paste0(path,'/Data/',file),encoding = 'utf-8'),silent = T)
    }else{
      data=read.csv(paste0(path,'/Data/',file),encoding = 'utf-8')
    }
    return(data)
  })
  
  get_comment_JD<-reactive({
    url=input$JDurl
    pages=input$JDpages
    file=paste0(sub('.html','',str_match(url,'[0-9]*.html')),'.csv')
    filelist=list.files(paste0(path,'/Data'))
    data=try(read.csv(paste0(path,'/Data/',file),encoding = 'utf-8'),silent = T)
    
  })
  
  output$JDstatus<-renderUI({
    out = tryCatch(commentspider_JD(), error=function( err ) paste(err))	    				
    if( class(out)=='try-error' ) 
      HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",'wrong urllink'))
    else
      HTML("<b>Status</b>: <b><font color='green'>Data gathering is Ok</font></b>")		
  })
  
  comment_summary_JD<-eventReactive(input$JDrecloud,{
    data=get_comment_JD()
    if (class(data)!='try-errof'){
      cutter=worker()
      content=data[data[,'content']!='此用户未填写评价内容','content']
      test=sapply(content%>% as.character(),segment,cutter)
      names(test)=seq(1,length(test))
      test=lapply(test,unique)
      stopwords=unlist(strsplit(input$JDstopwords,'、'))
      print(stopwords)
      filter_word=lapply(test,filter_segment,stopwords)
      filter_word=lapply(filter_word,str_trim)
      tableword=plyr::count(unlist(filter_word))
    }
  })
  
  output$JDcloud<-renderWordcloud2({
    tableword=comment_summary_JD()
    wordcloud2(tableword, size = 2, fontFamily = "微软雅黑",
               color = "random-light", backgroundColor = "grey")
    
    
  })
  
  output$JDdata<-downloadHandler(
    filename = function() {
      url=input$JDurl
      file=paste0(sub('.html','',str_match(url,'[0-9]*.html')),'.csv')
    },
    content = function(file) {
      data=get_comment_JD()
      write_excel_csv(data,file)
      
      
    }
  )
############key word search Tmall&Taobal

  
  output$Taobaostatus<-renderDataTable({
    
    pages=isolate(input$Taobaopages)
    keyword=isolate(input$Taobaokw)
    filename=paste0(keyword,'.csv')
    file_exist=filename %in% list.files(paste0(path,'/Data'))
    rerun=isolate(input$rerunkw)
    input$Taobaorun
    if((!file_exist) | (rerun=='TRUE')){
      num=0
      Data=data.frame()
      withProgress(message = 'Web spider in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (page in 1:pages) {
                       url=sub('num',num,sub('keyword',keyword,base_url))
                       print(url)
                       data=readTaobao(url)
                       Data=rbind(Data,data)
                       num=num+44
                       Sys.sleep(5)
                       incProgress(1/pages)
                     
                     }
                  write.csv(Data,file=paste0(path,'/Data/',filename))
                 })
    

    }else{
        #print(filename)
        Data=read.csv(paste0(path,'/Data/',filename))
      }
    Data
  })
  
  get_keyword_result<-reactive({
    pages=isolate(input$Taobaopages)
    filename=isolate(paste0(input$Taobaokw,'.csv'))
    Data=read.csv(paste0(path,'/Data/',filename))
  })
  
  output$Taobaodata<-downloadHandler(
    filename = function() {
      url=input$Taobaokw
      file=paste0(url,'.csv')
    },
    content = function(file) {
      data=get_keyword_result()
      write_excel_csv(data,file)
      
      
    }
  )
  
})