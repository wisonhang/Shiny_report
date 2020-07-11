library(readr)
library(leaflet)
library(leafletCN)
library(ggplot2)
library(glmnet)
library(cluster)
library(SIT)
library(data.table)
library(rpart)
library(rpart.plot)
library(tidyr)
library(showtext)
library(dplyr)
library(lubridate)
library(DT)
library(purrr)
library(progress)
showtext_auto()
font_add("SimSun", "/home/dwhang/project/simsun.ttc")

shinyServer(function(input, output) {
  #load('historysold.Rdata')
  data=read_csv('formatsold.csv')%>%as.data.frame()
  # data[,'地铁']=as.character(data[,'地铁'])
  # data[data[,'地铁']=="",'地铁']="无地铁"
  # data[,'地铁']=as.factor(data[,'地铁'])
  data=data%>%mutate(
    地铁=ifelse(is.na(地铁),'无地铁',地铁),
    地铁=as.factor(地铁),
    成交日期=date(成交日期),
    年=year(成交日期),
    月=month(成交日期)
  )
  xiaoqu=read_csv('xiaoqu.csv')%>%as.data.frame()
  temp=data%>%mutate(
    电梯=ifelse(电梯=='有电梯',1,0),
    地铁=ifelse(地铁=='无地铁',0,1),
    年份=year(成交日期)
    )
  
  xiaoqu=temp%>%group_by(小区,区域)%>%summarise(
    已售=n(),
    楼龄=mean(楼龄,na.rm=T),
    电梯=mean(电梯,na.rm=T),
    地铁=mean(地铁,na.rm=T),
    面积=mean(面积,na.rm=T)
  )%>%inner_join(
   temp%>%filter(year(Sys.Date())-年份<3)%>%group_by(小区,区域,年份)%>%summarise(
      总价=mean(总价,na.rm=T),
      单价=mean(单价,na.rm=T)/10000,
      已售=n()
    )%>%data.table()%>%dcast(小区+区域~年份,value.var=c('总价','单价','已售')),
   by=c('小区'='小区','区域'='区域')
  )%>%filter(已售>2)
  
  formatdata<-eventReactive(ifelse(input$set,TRUE,TRUE),{
    p <- Progress$new()
    p$set(value = 1, message = "Updating data...")
    data[,c('单价','总价')]<-lapply(data[,c('单价','总价')],function(x){round(x,0)})
    # data[,'成交日期']<-date(data[,'成交日期'])
    # data[,'月']<-date.month(data[,'成交日期'])
    # data[,'年']<-date.year(data[,'成交日期'])
    # 
    # data=
    # data[,c('经度','纬度')]<-data[,c('经度','纬度')]
    #data[,'地铁']=
    #data[,'维度']<-data[,'维度']-0.006
    data=as.data.table(data)
    if (input$district!="全部")
      data<-data[区域==input$district]
    if (input$year!='全部')
      data<-data[年>=input$year]
    
    p$close()
    data
    
  })
  
  output$boxplot<-renderPlot({
    tempdata<-formatdata()
    tempdata<-na.omit(tempdata)
    x=input$x
    y=input$y
    wrap=input$wrap
    col=input$col
    lel=names(sort(tapply(tempdata[[y]],tempdata[[x]],mean)))
    tempdata[[x]]=factor(tempdata[[x]],levels=lel)
    'null'=''
    if (col!='null'){
      lel1=names(sort(tapply(tempdata[[y]],tempdata[[col]],mean)))
      tempdata[[col]]=factor(tempdata[[col]],levels=lel1)
    }
    attach(tempdata)
    g<-ggplot(tempdata,aes(x=get(x),y=get(y),col=get(col)))+geom_boxplot()+
      labs(x=x,y=y)+ theme(plot.title = element_text(hjust = 0.5))+
      guides(col=guide_legend(title=input$col))
    if(wrap!='null'){
      lel2=names(sort(tapply(tempdata[[y]],tempdata[[wrap]],mean)))
      tempdata[[wrap]]=factor(tempdata[[wrap]],levels=lel2)
      attach(tempdata)
      g<-g+facet_wrap(~get(wrap),ncol=1)
    }
    g
  })
  
  output$sold<-renderDataTable({
    tempdata<-formatdata()

    tempdata<-tempdata[,-c('电梯','经度','纬度','年','月','楼龄','楼高','建筑年代','朝向')]
    setnames(tempdata,c('面积','总价','单价'),
             c('面积/平','总价/万','单价/元'))
    tempdata%>%datatable(rownames =F,
                         filter = 'top',extensions = 'Buttons',
                         caption=htmltools::tags$caption(
                           style = 'caption-side: top;text-align:center;
                           font-size:larger;color:black;font-weight:bold;'
                         ),
                         options=list(
                           buttons = c('copy', 'excel', 'pdf', 'print','colvis'),
                           initComplete = JS("function(settings, json) 
                                             {$(this.api().table().header()).css({'font-size' : '15px'});}"),
                           pageLength = 25, autoWidth = F,
                           dom='Bfrtip',
                           columnDefs = list(list(className = 'dt-center',
                                                  targets=0:11
                           ),
                           list(width='100px',targets=0)
                           )
                           )
    )
    
  })
  
  output$table1<-renderRpivotTable({
    p <- Progress$new()
    p$set(value = 1, message = "calculate pivot table...")
    
    data<-formatdata()
    tempdata<-data[,c('区域','街道','年','月','装修','户型','面积','总价','单价')]
    setDT(tempdata)
    tempdata[,月份:=paste(年,月,sep = ' ')]
    tempdata[,c('年','月'):=NULL]
    setnames(tempdata,c('面积','总价','单价'),
             c('面积/平','总价/万','单价/元'))
    #tempdata
    
    rpv=rpivotTable(tempdata,rows=input$index,cols=input$colum,vals='单价/元',
                aggregatorName = "Average", 
                rendererName = "Heatmap")
    p$close()
    rpv
    
  })
  
  output$map<-renderLeaflet({
    data<-formatdata()
    #data<-data[年==input$year]
    if (input$size=='街道')
      region=data[,.(mag=mean(单价),lat=mean(纬度),long=mean(经度)),by=.(街道)]
    else
      region=data[,.(mag=mean(单价),lat=mean(纬度),long=mean(经度)),by=.(小区)]
    colnames(region)=c('类别','mag','lat','long')
    map=data.frame(region)
    outline=map[chull(map$long, map$lat),]
    cPal<-colorBin(palette = c('green',"blue",'red'),domain = map[,'mag'])
    Shanghai <- leaflet(map) %>% 
      amap(group='高德')%>%
      # addProviderTiles('OpenStreetMap.BlackAndWhite',group='灰度')%>%
      # addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "黑底") %>%   
      # 第三层底图，白底层
      addProviderTiles(providers$Stamen.TonerLite, group = "白底") %>% 
      setView(lng = map[1,'long'], lat = map[1,'lat'], zoom = 10)%>%
      # addMarkers(~long, ~lat, popup = ~paste0(as.character(round(mag,0)),'元/平'), label = ~as.character(类别),
      #            group='标注')%>%
      addMarkers(~long, ~lat,
                 clusterOptions = markerClusterOptions(),
                 clusterId = "quakesCluster",
                 popup = ~paste0(as.character(round(mag,0)),'元/平'), label = ~as.character(类别),
                 group='标注') %>%
      addEasyButton(easyButton(
        states = list(
          easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))%>%
      
      addCircleMarkers(~long,~lat,fillColor = ~cPal(mag),stroke =F, group = "点",fillOpacity = 0.8,radius=3) %>%
      addLegend("bottomleft", pal = cPal, values = ~mag,title = "每平米均价",
                labFormat = labelFormat(suffix = " 元/平"),
                opacity = 1,group='图例')%>%
      addMiniMap()%>%
      addCircles(~long, ~lat, ~mag/100, stroke =F, group = "圈",color="#9A32CD",fillOpacity = 0.7) %>%
      addPolygons(data = outline, lng = ~long, lat = ~lat,
                  fill = FALSE, weight = 2, color = "#FFFFCC", group = "轮廓") %>%
      addMeasure()%>%
      # 复选框       
      addLayersControl( 
        #显示层
        baseGroups = c("标准","灰度", "黑底", "白底"),   
        overlayGroups = c("圈",'点', "轮廓",'标注','图例'),
        options = layersControlOptions(collapsed = FALSE)
      )%>% hideGroup(c('圈','轮廓'))
  })
  
  class.ind <- function(cl) {
    n <- length(cl)
    cl <- factor(cl,levels = unique(cl))
    x <- matrix( 0,  n ,  length(levels(cl)) )
    # unclass 返回每个字符在level表中的位置
    # 然后按照列计算在向量中的位置
    x[n*(unclass(cl)-1) +(1:n)] <- 1
    x<-data.frame(x)
    colnames(x)=levels(cl)
    x[,-1]
  }
  modeldata<-reactive({
    trans=formatdata()
    trans=na.omit(trans)
    trans=trans[,c('区域','户型','地铁','电梯','面积','单价','楼龄','楼高')]%>% as.data.frame()
    #trans[is.na(trans['地铁']),'地铁']='无地铁'
    
    trans
  })
  
  lm.modes<-function(){
    trans=modeldata()
    #trans=data.frame(trans)
    #trans[,'地铁']=na.fill(trans[,'地铁'],'无地铁')
    #trans[is.na(trans[,'地铁']),'地铁']='无地铁'
    x=cbind(trans[,c('单价','电梯','楼龄','楼高','面积')],class.ind(trans[,'区域']),class.ind(trans[,'户型']),class.ind(trans[,'地铁'])
            )
    x[,'电梯']=factor(x[,'电梯'],levels=c('无电梯','有电梯'))
    x[,'电梯']<-unclass(x[,'电梯'])-1
    y=x[,'单价']
    lm.fit<-lm(单价~.,x)
    
    x=x[,-1]
    x=sapply(x,as.numeric)
    cv.fit=cv.glmnet(scale(x),scale(y),intercept = FALSE,alpha=1)

    return(list('lm'=lm.fit,'lasso'=cv.fit))
  }
  

  output$regression<-renderPrint(
    summary(modeldata(),maxsum=10)
  )
  
  
  output$lm<-renderPrint({
    model=lm.modes()
    summary(model$lm)
  })
  
  
  output$lasso<-renderPlot({
    model=lm.modes()
    cv.fit=model$lasso
    coef=coef(cv.fit,s='lambda.min')
    coeff=data.frame(cbind(rownames(coef),coef[,1]))[-1,]
    colnames(coeff)=c('var','coeff')
    coeff[,'coeff']=sapply(coeff[,'coeff'],function(x){round(as.numeric(as.character(x)),2)})
    #coeff[,'var']=sapply(coeff[,'var'],function(x){sub('X','',x)})
    coeff[,'var']=factor(coeff[,'var'],levels=coeff[,'var'])
    g=ggplot(coeff,aes(var,coeff))+geom_bar(stat='identity',fill='red')
    g
  })
  
  clustmodel<-reactive({
    test=xiaoqu[,-c(1,2)]
    test[is.na(test)] <- 0
    
    wssplot<-function(data,nc=15,seed=1234){
      wss<-(nrow(data)-1)*sum(apply(data,2,var))
      for (i in 2:nc){
        set.seed(seed)
        wss[i]<-sum(kmeans(data,centers=i)$withiness)
      }
      plot(1:nc,wss,type='b',xlab='Number of Clusters',
           ylab='Within groups sum of squres')
    }
    
    #wssplot(test)
    
    fit.pa=pam(test,k=input$kmean,stand=TRUE)
    fit.pa
  })
  
  
  output$clust<-renderPlot({
    fit.pa<-clustmodel()
    clusplot(fit.pa, col.p = fit.pa$clustering,main='小区聚类-K中心算法')
  })
  
  output$tree<-renderPlot({
    xq=xiaoqu
    fit.pa=clustmodel()
    xq['质心']=fit.pa$clustering
    # colnames(xq)=c('小区名称','区域','已售套数','楼龄/年','电梯指数','地铁指数','面积','16总价/万','17总价/万','18总价/万',
    #                '16单价/万','17单价/万','18单价/万','质心')
    attach(xq)
    fit=rpart(质心~.,data=xq[,-c(1,2)],method='class'
    )
    rpart.plot(fit, branch=1, branch.type=5, type=1, extra=102,  
               shadow.col="gray", box.col="green",  
               border.col="blue", split.col="red",  
               split.cex=1.2, main="Kyphosis decision tree for k中心")
    
   })
  
  output$clustsummary<-renderDataTable({
    xq=xiaoqu
    fit.pa=clustmodel()
    xq['质心']=fit.pa$clustering
    
    xqinfo<-xq%>%group_by(质心)%>%nest()%>%mutate(
      数量=map(data,function(x)length(x$小区)),
      已售=map(data,function(x)sum(x$已售)),
      summary=map(data,function(x) tail(rbind(x[-c(1:3)],colMeans(x[,-c(1:3)],na.rm=T)),1))
    )%>%select(-data)%>%unnest()
    
    # xqtable=data.table(xq)
    # xqinfo=xqtable[,.(length(小区名称),sum(已售),mean(面积),
    #                mean(楼龄,na.rm=T),mean(电梯),mean(地铁),
    #                mean(总价1,na.rm=T),mean(总价2,na.rm=T),mean(总价3,na.rm=T),
    #                mean(单价1,na.rm=T),mean(单价2,na.rm=T),mean(单价3,na.rm=T)),by=质心]
    
    colnames(xqinfo)[1:7]=c('分类','小区数','售出数/套','楼龄/年',
                            '电梯指数','地铁指数','平均面积/m2')
    
    # xqinfo=setDF(xqinfo)
    # xqinfo[,4:13]=sapply(xqinfo[,4:13],round,2)

    xqinfo%>%
      datatable(rownames =F,
                caption=htmltools::tags$caption(
                  style = 'caption-side: top;text-align:center;
                  font-size:larger;color:black;font-weight:bold;'
                ),
                options=list(
                  initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '15px'});}"),
                  pageLength = 5, autoWidth = F,
                  dom='Bt',
                  columnDefs = list(list(className = 'dt-center',
                                         targets=0:15
                  ),
                  # list(width='100px',targets=0),
                  list(visible=FALSE, targets=c(13:15))
                  )
                )
      )%>%formatCurrency(c(2,3,8:10),'',digits = 0)%>%
      formatCurrency(c(4:7,11:16),'',digits = 2)

      
  })
  
  output$xiaoquinfo<-renderDataTable({
    #xiaoqu=read.csv('xiaoqu.csv')
    fit.pa=clustmodel()
    
    #col=c('楼龄','电梯','地铁','面积','总价1','总价2','总价3','单价1','单价2','单价3')
    xiaoqu['分类']=fit.pa$clustering
    #xiaoqu[,c('单价2016','单价2017','单价2018')]=xiaoqu[,c('单价2016','单价2017','单价2018')]/10000
    # xiaoqu[,col]=sapply(xiaoqu[,col],round,2)
    # colnames(xiaoqu)=c('小区名称','区域','已售','楼龄/年','电梯','地铁',
    #                    '面积/m2','16总价/万','17总价/万','18总价/万','16单价/万','17单价/万','18单价/万','分类')
    # xiaoqu
    
    xiaoqu%>%
      datatable(rownames =F,
                filter = 'top',extensions = 'Buttons',
                caption=htmltools::tags$caption(
                  style = 'caption-side: top;text-align:center;
                  font-size:larger;color:black;font-weight:bold;'
                ),
                options=list(
                  buttons = c('copy', 'excel', 'pdf', 'print','colvis'),
                  initComplete = JS("function(settings, json) 
                                    {$(this.api().table().header()).css({'font-size' : '15px'});}"),
                  pageLength = 100, autoWidth = F,
                  dom='Bfrtip',
                  columnDefs = list(list(className = 'dt-center',
                                         targets=0:16
                  ),
                  list(width='100px',targets=0),
                  list(visible=FALSE, targets=c(13:15))
                  )
                )
                )%>%formatCurrency(c(2,3,8:10),'',digits = 0)%>%
      formatCurrency(c(4:7,11:16),'',digits = 2)
  })
  
  
  output$clustcount<-renderTable({
    xq=xiaoqu
    fit.pa=clustmodel()
    xq['质心']=fit.pa$clustering
    a=xq%>%group_by(质心,区域)%>%summarise(num=n())%>%as.data.frame()
    spread(a,区域,num)
  })
  
})
