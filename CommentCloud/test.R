files=list.files(paste0(getwd(),'/Data'))
library(jiebaR)
library(wordcloud2)
library(stringr)
library(plyr)
library(readr)
library(RCurl)
library(rvest)
library(rjson)
library(readr)
boss=rbind(read.csv('bossmen546989649589.csv'),read.csv("bossmen558956737405.csv" ))
boss=rbind(boss,read.csv('bossmen546837721238.csv'))
lampo=rbind(read.csv("lampo547501969350.csv" ),read.csv("lampo549996315235.csv"))
mdmen=rbind(read.csv("MDmen567845689758.csv"),read.csv("MDmen571753728812.csv"))
mdwomen=rbind(read.csv("MDwomen568502424733.csv"),read.csv("MDwomen568627837202.csv"))
supply=rbind(read.csv("supplymen565378640342.csv"),read.csv("supplymen565489877473.csv"))

stopwords='了、也、好、很、的、不、我、会、说、和、吧'
comment_summary<-function(data){
cutter=worker()
test=sapply(data[,'rateContent']%>% as.character(),segment,cutter)
names(test)=seq(1,length(test))
test=lapply(test,unique)
stopwords=unlist(strsplit(stopwords,'、'))
print(stopwords)
filter_word=lapply(test,filter_segment,stopwords)
filter_word=lapply(filter_word,str_trim)
tableword=plyr::count(unlist(filter_word))
}


wordcloud2(comment_summary(boss), size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")

wordcloud2(comment_summary(lampo), size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")

wordcloud2(comment_summary(mdmen), size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")

wordcloud2(comment_summary(mdwomen), size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")

wordcloud2(comment_summary(supply), size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")


