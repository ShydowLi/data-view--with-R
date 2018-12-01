#连环图（反映一个企业每一年的各个指标情况）
library(ggplot2)
library(scatterpie)
library(dplyr)
mydata<-c(1,1,1,1,1,1,1,1,1,2,3,2,3,5,5,1,1,1,1,1,2,2,4,5,1,3,2,3,5,5,4,2,4,2,1,2,1,1,0.5,0.5)
Dummy<-5*seq(1:8)
year<-c(2010,2011,2012,2013,2014,2015,2016,2017)
mydata1<-matrix(mydata,nrow=8,ncol=5,byrow = T)
mydata1<-as.data.frame(mydata1)
colnames(mydata1)<-c('s1','s2','s3','s4','s5')
mydata1<-cbind(year,Dummy,mydata1)
color2<-c("#17375E","#23538D","#558ED5","#8EB4E3","#C6D9F1")
Data<-c(5,8,15,12,9,19,16,9)
mydata1<-cbind(mydata1,Data)


ggplot()+geom_line(data = mydata1,aes(year,Data,group=1),color='#085264',size=0.8)+geom_scatterpie(data = mydata1,aes(year,Data,r=1.5),cols = colnames(mydata1)[3:7],color=NA)+ylim(0,25)+scale_fill_manual(values = color2)+guides( fill=guide_legend(label.position ="top"))+
  theme(
    axis.title=element_blank(),
    legend.title=element_blank(),
    panel.background=element_blank(),
    axis.line=element_line(),
    axis.ticks=element_line(),
    legend.direction="horizontal",
    legend.position=c(0.15,0.9),
  )