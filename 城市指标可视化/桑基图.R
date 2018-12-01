#桑基图 （用于网络流量的分析也适用于企业各区域产品的流动）
library(readxl)
library(dplyr)
library(mapdata)
library(maptools)
library(maps)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(REmap)
library(networkD3)

#桑基图
data<-read_xlsx('E:/R WORK SPACE/Data Files/企业各个区域的流量情况.xlsx')
data1<-aggregate(data[,4],by=list(data$state,data$address),FUN = sum)
data<-data[,-1]
names(data1)<-c("source","target","value")
names(data)<-c("source","target","value")
dat<-bind_rows(data1,data)
sankeylink<-dat
sankeynode<-data.frame(name=unique(c(sankeylink$source,sankeylink$target)))
sankeynode$index<-0:(nrow(sankeynode)-1)
sankeylink<-merge(sankeylink,sankeynode,by.x="source",by.y="name")
sankeylink<-merge(sankeylink,sankeynode,by.x="target",by.y="name")
dat1<-sankeylink[,c(4,5,3)]
names(dat1)<-c("Source","Target","Value")
datname<-sankeynode[,1,drop=FALSE]
sankeyNetwork(Links=dat1,Nodes=datname, Source ="Source",Target = "Target", Value = "Value", NodeID = "name",units = "TWh",fontSize = 13, nodeWidth = 50) 

#文字云（反应了各个省的流量分布）
library(wordcloud2)
mydata<-read_xlsx('E:/R WORK SPACE/Data Files/企业各个区域的流量情况.xlsx')
clouddata<-select(mydata,provice,vlaue)
wordcloud2(clouddata,size = 0.5,shape = 'star')

#树状图
library(treemap)
treedata<-mydata[,-1]
treemap(treedata,index = c('address','provice'),vSize = 'vlaue',palette='RdBu')

#环比图
dat2<-mydata
dat2<-mutate(dat2,sale2016=runif(34,500,1000),sale2017=runif(34,600,1000),ratio=round((sale2017-sale2016)/sale2016,3))
bardat<-group_by(dat2,address)
bardatsum<-summarise(bardat,sum2016=sum(sale2016),sum2017=sum(sale2017))
bardatsum<-gather(bardatsum,year,sumsale,-address)
p<-ggplot(bardatsum,aes(address,sumsale,fill=year))+geom_bar(stat = 'identity',position = dodge)+coord_polar(theta = 'x',start = 0)+facet_wrap(year~.)
p+theme(axis.title = element_blank(),axis.line = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank())

#在地图上标出在某个省的增长率












