library(shiny)
library(xtable)
library(SIT)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
library(RCurl)
#library(nloptr)
library(ggplot2)
library(corrplot)
library(fGarch)

getSymbols.mine<-function(symbol,from='2000-01-01',to=Sys.time(),auto.assign=T,env=parent.frame(),
                          ...){
  d <- debugGatherer()
  myheader=c("Accep"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
             "Accept-Encoding"="gzip, deflate,br",
             "Accept-Language"="zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
             "Connection"="keep-alive",
             #"Cookie"="B=02p4t99bsgjbq&b=3&s=ai; PRF=%3Dundefined%26t%3DQRAAX%252BBIL%252B%255ENQCASH%252BVFITX%252BIEF%252BHYG%252BVTSMX%252BVFINX%252BVTI%252BVGTSX%252BVDMIX%252BEFA%252B%255ENDX%252BQQQ%252BEEM",
             "Cookie"="B=02p4t99bsgjbq&b=3&s=ai; PRF=%3Dundefined%26t%3DSPY%252BVTI%252BACWX%252BAGG%252BLQD%252BSHY%252BUUP%252BGSG%252BIYR%252BIWM%252BQRAAX%252BBIL%252B%255ENQCASH%252BVFITX%252BIEF",
             "Host"="query1.finance.yahoo.com",
             "Upgrade-Insecure-Requests"="1",
             "User-Agent"="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0")
  from=as.numeric(as.POSIXct(1, origin = from))
  if (is.timeBased(to)){
    to=round(as.numeric(to),0)
  }else{
    to=as.numeric(as.POSIXct(1,origin =to))
  }
  url=paste('https://query1.finance.yahoo.com/v7/finance/download/',symbol,'?period1=',
            from,'&period2=',to,'&interval=1d&events=history&crumb=uUu.HgX1UH8',sep='')
  txt=getURL(url=url,httpheader = myheader,debugfunction = d$update, verbose = T,encoding='utf-8')
  if(grepl('error',txt))
    return(NULL)
  fr <- read.csv(text=txt,na.strings =c('null','NA',''),skipNul=TRUE)
  fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1]), src = "yahoo", 
            updated = Sys.time())
  fr<-fr[,c(1,2,3,4,6,5)]
  colnames(fr) <- paste(toupper(gsub("\\^", "", symbol)), 
                        c("Open", "High", "Low", "Close", 'Volume',"Adjusted"), 
                        sep = ".")
  if (auto.assign) 
    assign(symbol, fr, env)
  if (auto.assign) 
    return(symbol)
  return(fr)
  
}

##################HRP##########

getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}

getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}

getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}

recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <- w[cItems0] * alpha
  w[cItems1] <- w[cItems1] * (1-alpha)
  
  if(length(cItems0) > 1) {
    w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}




min.var.portfolio.TF<-function(ia,constrants){
  #risk.index=get.risky.asset.index(ia)
  moms <- Return.cumulative(ia$hist.returns)
  highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0){
    weights<-rep(0, ia$n)
    names(weights)<-names(ia$index)
    weights
  }else if (sum(selectedAssets)==1){
    weights<-rep(0,ia$n)
    weights[which(selectedAssets==TRUE)]=1
    names(weights)<-names(ia$index)
    weights
  }else{
    selectia<-create.ia(selectedSubset)
    selconstrants =create.basic.constraints(sum(selectedAssets),constrants$lb[selectedAssets], 
                                         constrants$ub[selectedAssets], 1)
    weights<-min.var.portfolio(selectia,selconstrants)
    #weights<-min.var.portfolio(selectia,constrants)
    # cors <- cor(selectedSubset) # correlation
    # volSubset <- selectedSubset # 20 day volatility
    # vols <- t(apply(volSubset,2,sd))
    # covs <- t(vols) %*% vols * cors
    # minVolRets <- t(matrix(rep(1, sum(selectedAssets))))
    # weights <- portfolio.optim(x=minVolRets, covmat = cov(selectedSubset[-1,]))$pw
    set.risky.asset(weights,selectedAssets)
  }
  
}


HRP.portfolio.TF<-function(ia,constrants){
  #risk.index=get.risky.asset.index(ia)
  moms <- Return.cumulative(ia$hist.returns)
  highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0){
    weights<-rep(0, ia$n)
    names(weights)<-names(ia$index)
    weights
  }else if (sum(selectedAssets)==1){
    weights<-rep(0,ia$n)
    weights[which(selectedAssets==TRUE)]=1
    names(weights)<-names(ia$index)
    weights
  }else{
    clustOrder <- hclust(dist(ia$corr[selectedAssets,selectedAssets]), method = 'single')$order
    weights<-getRecBipart(ia$cov[selectedAssets,selectedAssets], clustOrder)
    set.risky.asset(weights,selectedAssets)
  }
}


HRP.portfolio<-function(ia,constrants){
  risk.index = get.risky.asset.index(ia)
  clustOrder <- hclust(dist(ia$correlation), method = 'single')$order
  weights<-getRecBipart(ia$cov, clustOrder)
  #weight=target.risk/as.numeric(sqrt(t(weight)%*%ia$cov%*%weight))*weight
  set.risky.asset(weights, risk.index)
}

risk.parity.portfolio.TF<-function(ia,constrants){
  moms <- Return.cumulative(ia$hist.returns)
  highRankAssets <- rank(moms) >= ia$n/2 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- ia$hist.returns[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0){
    weights<-rep(0, ia$n)
    names(weights)<-names(ia$index)
    weights
  }else if (sum(selectedAssets)==1){
    weights<-rep(0,ia$n)
    weights[which(selectedAssets==TRUE)]=1
    names(weights)<-names(ia$index)
    weights
  }else{
    risk=ia$risk[selectedAssets]
    weights=1/risk/(sum(1/risk))
    set.risky.asset(weights,selectedAssets)
  }
}

files=list.files('data')
symbol_env=new.env()
for (ff in files){
  temp=read.xts(paste0('data/',ff))
  #temp=as.xts(as.data.frame(temp[,-c(1,2,8)]),order.by = as.POSIXct(temp[,2],'%Y-%m-%d'))
  symbol_env[[substring(ff,1,6)]]=temp
}

save(symbol_env,file='Adata.Rdata')







ret.estimate<-function(ret){
  sample=na.omit(ret)
  garch=garchFit(~garch(1,1),sample,trace = F)
  garch.arma15=garchFit(~arma(1,5)+garch(1,1),sample,trace = F)
  garch.arma11=garchFit(~arma(1,1)+garch(1,1),sample,trace = F)
  exp.ret=cbind(runMean(sample,60),runMean(sample,252),garch@fitted,garch.arma11@fitted,garch.arma15@fitted)
  colnames(exp.ret)=c('60m','252m','garch','garch-arma11','garch-arma15')
  plota.matplot(exp.ret)
  return(exp.ret)
}

vol.estimate<-function(ret){
  sample=na.omit(ret)
  garch=garchFit(~garch(1,1),sample,trace = F)
  garch.arma15=garchFit(~arma(1,5)+garch(1,1),sample,trace = F)
  garch.arma11=garchFit(~arma(1,1)+garch(1,1),sample,trace = F)
  vol=cbind(runSD(sample,60),runSD(sample,252),garch@sigma.t,garch.arma11@sigma.t,garch.arma15@sigma.t)
  colnames(vol)=c('60m','252m','garch','garch-arma11','garch-arma15')
  plota.matplot(vol)
}

vol.garch<-function(ret,method=c('ret','vol'),forecast=20){
  method=method[1]
  sample=na.omit(ret)
  if(length(sample)<forecast)
    return(NA)
  #garch=garchFit(~arma(1,1)+garch(1,1),sample,trace=F)
  garch=garchFit(~garch(1,1),sample,trace=F)
  if(method=='ret'){
    exp.ret=try(mean(predict(garch,forecast)[,1]),silent=T)
  if(class(exp.ret)=='try-error')
    exp.ret=mean(garch@fitted)
  return(exp.ret)
  }else if(method=='vol'){
    exp.vol=try(mean(predict(garch,forecast)[,3]),silent=T)
    if(class(exp.vol)=='try-error')
      exp.vol=mean(garch@sigma.t)
    return(exp.vol)
  }
}

create.adjust.ia<-function(hist.returns, index = 1:ncol(hist.returns), nperiod = nrow(hist.returns)){
  ia = list()
  ia$hist.returns = hist.returns
  ia$nperiod = nperiod
  ia$index = index
  ia$n = ncol(ia$hist.returns)
  ia$symbols = colnames(ia$hist.returns)
  ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
  ia$garch.risk = apply(ia$hist.returns,2,vol.garch,'vol')
  ia$correlation = cor(ia$hist.returns, use = "pairwise.complete.obs", 
                       method = "pearson")
  ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
  ia$covadj=ia$correlation * (ia$garch.risk %*% t(ia$garch.risk))
  ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
  ia$garch.return = apply(ia$hist.returns,2,vol.garch,'ret')
  return(ia)
}

# end=endpoints(ret,on='weeks')
# end=end[end>=250]
# dt=list(risk=NA,vol=NA,exp.ret=NA,garch.ret=NA)
# for (i in end){
#   hist.returns=ret[(i-250):i,]
#   ia=create.adjust.ia(hist.returns)
#   dt$risk=rbind(dt$risk,ia$risk)
#   dt$vol=rbind(dt$vol,ia$garch.risk)
#   dt$exp.ret=rbind(dt$exp.ret,ia$expected.return)
#   dt$garch.ret=rbind(dt$garch.ret,ia$garch.return)
#   
# }
# dt=lapply(dt,function(x){return(as.xts(x,order.by=index(ret[c(1,end),])))})
# colnames(dt$risk)=colnames(dt$vol)=colnames(dt$exp.ret)=colnames(dt$garch.ret)=index(ret[c(1,end),])
# #############################
# input=list()
# input$files$name=''
# input$Periodicity='weeks'
# input$lookback=50
# input$estimate.len='days'
# input$yr=c(2013,2018)
# input$symbols="Equity:510050,510300,159915,000006,159923,159938,159939,159940,512880;
# Commodity:000819,399979,518880,;
# Bond:000832,511010,120601,120602"
# txt<-unlist(strsplit(gsub('\n','',input$symbols),';'))
# symbols_asset<-list()
# for (subname in c('Equity','Bond','Commodity','Cash')){
#   symbols_asset[[subname]]=spl(gsub(paste0(subname,':'),'',txt[grep(subname,txt)]))
# }
# data<-new.env()
# upload_symbol=gsub('.csv','',input$files$name)
# now=as.Date(Sys.Date())
# upload_symbol=gsub('.csv','',input$files$name)
# data_symbol=gsub('.csv','',list.files('data'))
# input$Equity=symbols_asset$Equity
# input$Commodity=symbols_asset$Commodity
# input$Bond=symbols_asset$Bond
# asset_pool=list(input$Equity,input$Commodity,input$Bond,input$Cash)
# 
# for(symbol in unlist(asset_pool)) {
#   #print(symbol)
#   if(!is.null(symbol_env[[symbol]])){
#     if(!symbol %in% c(upload_symbol,data_symbol)){
#       start=index(last(symbol_env[[symbol]]))+1
#       if(start<now){
#         #tempdata<-getSymbols.mine(symbol,from=as.character(start),auto.assign=FALSE)
#         tryCatch({
#           #tempdata<-getSymbols.mine(symbol,from=as.character(start),auto.assign=FALSE)
#           tempdata<-getSymbols(symbol,from=as.character(start),auto.assign=FALSE)
#         }, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
#         if(!is.null(tempdata))
#           symbol_env[[symbol]]=rbind(symbol_env[[symbol]],tempdata)
#       }
#     }
#   }else if (symbol %in% data_symbol){
#     #print(paste0(symbol,'data_symbol'))
#     temp=read.xts(paste0('data/',symbol,'.csv'))
#     colnames(temp)<-paste(symbol,
#                           c("Open", "High", "Low", "Close","Adjusted",'Volume'),
#                           sep = ".")
#     symbol_env[[symbol]]=temp
#   }else if (symbol %in% upload_symbol){
#     temp=read.xts(input$files$datapath[which(grepl(symbol,upload_symbol))])
#     colnames(temp)<-paste(symbol,
#                           c("Open", "High", "Low", "Close","Adjusted",'Volume'),
#                           sep = ".")
#     symbol_env[[symbol]]=temp
#   }else{
#     tryCatch(
#       #symbol_env[[symbol]]<-getSymbols.mine(symbol,auto.assign=FALSE)
#       symbol_env[[symbol]]<-getSymbols(symbol,auto.assign=FALSE)
#       , error = function(e) { stop(paste('Problem getting prices for',symbol)) }
#     )
#   }
#   data[[symbol]] = symbol_env[[symbol]][paste0(input$yr[1],'::',input$yr[2])]
# }
