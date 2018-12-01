#瀑布图（解释每一个季度的业绩啊，涨幅等）
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
dat<-read_xlsx('E:/R WORK SPACE/Data Files/瀑布图.xlsx')
Color<-c("#A6442A","#015313","#131F37","#FFFFFF")
dat1<-gather(dat,class,scope,-Item)
Item<-c('Before','A','B','C','D','E','F','G','After')
dat1$class<-factor(dat1$class,levels=c("Add","BA","Reduc","Dummy"),order=T)
p<-ggplot()+geom_bar(data = dat1,aes(Item,scope,fill=class),stat = 'identity',position = 'stack',width = 0.7)+scale_x_discrete(limits=Item)+scale_fill_manual(values=Color)

#向上面标注数值
p1<-p+geom_text(data = dat,aes(Item,BA/2),label=ifelse(dat$BA==0,'',dat$BA),col='white')
