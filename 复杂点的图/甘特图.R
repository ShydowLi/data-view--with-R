library("lubridate")
library("ggplot2")
library("ggmap")
library(showtext)
library(grid)
library(scales)
Item<-paste("Step"," ",1:8,sep="")
Planned_Start_Date<-c("2016/03/03","2016/03/16","2016/03/28","2016/04/02","2016/04/12","2016/04/22","2016/05/16","2016/05/22")
Planned_Finish_Date<-c("2016/03/15","2016/03/31","2016/04/04","2016/04/15","2016/04/26","2016/05/20","2016/05/28","2016/06/12")
Actual_Start_Date<-c("2016/03/03","2016/03/16","2016/03/27","2016/04/05","2016/04/13","2016/04/22","2016/05/16","2016/05/22")
Actual_Finish_Date<-c("2016/03/18","2016/03/28","2016/04/05","2016/04/16","2016/04/27","2016/05/15","2016/05/16","2016/05/22")

mydata<-data.frame(Item,Planned_Start_Date,Planned_Finish_Date,Actual_Start_Date,Actual_Finish_Date,stringsAsFactors = FALSE)


mydata$Planned_Start_Date<-ymd(mydata$Planned_Start_Date)
mydata$Planned_Finish_Date<-ymd(mydata$Planned_Finish_Date)
mydata$Actual_Start_Date<-ymd(mydata$Actual_Start_Date)
mydata$Actual_Finish_Date<-ymd(mydata$Actual_Finish_Date)


datebreaks<-seq(as.Date("2015-03-01"),as.Date("2015-06-01"),by="1 month")
time<-as.Date("2016-05-15")

p<-ggplot()+
  geom_linerange(data=mydata,aes(x=Item,ymin=Planned_Start_Date,ymax=Planned_Finish_Date),size=10,color="#BFBFBF",alpha=0.8)+
  geom_linerange(data=mydata,aes(x=Item,ymin=Actual_Start_Date,ymax=Actual_Finish_Date),size=7,color="#085264",alpha=0.8)+
  scale_x_discrete(limits=sort(Item,decreasing=T))+
  scale_y_date(position ="top")+
  #scale_y_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  #geom_hline(data=NULL,aes(hintercept=time))+
  coord_flip()+
  theme(
    axis.title=element_blank(),
    axis.text.x=element_text(margin=margin(5,0,0,0,"pt")),   #调整x轴的变量间距
    axis.text.y=element_text(margin=margin(0,10,0,0,"pt")),  #调整y轴的间距
    axis.ticks.y=element_blank(),    #令y轴消失
    panel.grid.major.y=element_line(color="#FFB666",linetype=5),  #添加辅助线，设为红色5号
    panel.background=element_rect(fill="white"),
    axis.text=element_text(colour ="black",size=10,face="italic",family="myFont"),
    axis.line.x=element_line(),
    panel.spacing=unit(-0.3,"cm")
  )