#子弹图（绩效与目标关系）

library(ggplot2)
library(reshape2)
library(ggmap)
library(dplyr)
library(tidyr)
KPI<-c("KPI1","KPI2","KPI3","KPI4","KPI5")
INDEX<-1:5
good<-rep(0.2,5)
excellent<-good
pass<-rep(0.6,5)
target<-c(0.84,0.85,0.7,0.92,0.78)
fact<-c(0.91,0.8,0.68,0.91,0.8)
mydata<-data.frame(KPI,INDEX,excellent,good,pass,target,fact)
dat<-gather(mydata,perform,scope,-KPI,-target,-INDEX,-fact)
color<-c("#43546C","#8695B2","#D9DBDF")


p<-ggplot()+geom_bar(data=dat,aes(KPI,scope,fill=perform),stat = 'identity',position = 'stack',width = 0.7)+ylim(-0.15,1.2)

p1<-p+geom_linerange(data=dat,aes(x=KPI,ymin=0,ymax=fact),col="#000002",size=5)+scale_fill_manual(values=sort(color,decreasing=T) )

p2<-p1+geom_text(data = mydata,aes(x=KPI,y=fact+0.5),label=fact)+theme(
  legend.direction="horizontal",
  legend.position=c(0.5,.88),
  legend.text=element_text(size=12)
)
