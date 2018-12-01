#不等宽柱形图（用柱形图的横坐标值代表的销量多少，纵坐标代表销售额，面积代表价值也就是瘦高的越好）
library(ggplot2)
library(scales)
library(ggmap)
mydata<-data.frame(Name=paste0("项目",1:5),Scale=c(35,30,20,10,5),ARPU=c(56,37,63,57,59))

#找出每个变量的横坐标的值
#构造矩形X轴的起点（最小点） 也就是每一个柱的起始点（累计和）
mydata$xmin<-0
for (i in 2:5){
  mydata$xmin[i]<-sum(mydata$Scale[1:i-1])
}
#构造矩形X轴的终点（最大点） 也就是每一个柱的终止点
for (i in 1:5){
  mydata$xmax[i]<-sum(mydata$Scale[1:i])
}
#构造数据标签的横坐标：
for (i in 1:5){
  mydata$label[i]<-sum(mydata$Scale[1:i])-mydata$Scale[i]/2
}

#画图
windowsFonts(myFont = windowsFont("微软雅黑"))

ggplot(mydata)+
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=0,ymax=ARPU,fill=Name))+
  scale_fill_manual(values=c("#54576B","#BD1F12","#E8BA11","#62962A","#9B56AF"))+
  geom_text(aes(x=label,y=ARPU-3,label=ARPU),size=6,col="white",family="myFont")+
  geom_text(aes(x=label,y=-2.5,label=Scale),size=4,col="black",family="myFont")+
  geom_text(aes(x=label,y=-5.5,label=Name),size=4,col="black",family="myFont")+
  annotate("text",x=16,y=70,label="不等宽柱形图",size=8,family="myFont")+  
  annotate("text",x=14,y=64,label="这是一幅很用心的图表",size=4,family="myFont")+ 
  annotate("text",x=11,y=-9.8,label="Source:EasyCharts",size=4,family="myFont")+ 
  ylim(-10,80)+
  theme_nothing()


