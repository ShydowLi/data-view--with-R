#漏斗图以漏斗的形式直观的表现100%的数据，每一部分都
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
scpoe<-c(0.9,0.8,0.6,0.4,0.2)
part<-paste0('part',c(1:5))
order<-1:5
help<-(1-scpoe)/2
dat<-data.frame(scpoe,part,order,help)
dat1<-melt(dat,id.vars = c('part','order'),variable.name = 'perform',value.name = 'scpoe')
#dat1<-gather(data=dat,perform,scope,-part,-order)
color<-c("#FFFFFF","#088158")
p<-ggplot()+geom_bar(data=dat1,aes(order,scpoe,fill=perform),stat = 'identity',position = 'stack',width = 0.7)+scale_fill_manual(values = sort(color))

#修饰
p1<-p+geom_text(data = dat1,aes(x=order,y=help+scpoe/2-.025,label=factor(part)),col="white",size=4)+theme_nothing()

