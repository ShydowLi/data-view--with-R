#stack overflow问卷分析情况（可视化）

#目标
#1、各国开发者对编程语言是否有偏好
#2、开发者最密集的区域分布在世界上的那个角落
#3、编程语言与编程工作的一些相关联系

#假设问题   1、so平台的自画像（也就是用户描述） 2、os平台可以在哪些平台上进行推广




--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

#读取数据  （在这里取出原始数据的前10000行）
  
  library(tidyverse)
  library(ggmap)
  library(mapdata)
  library(ggthemes)
  library(ggtech)
  library(ggthemr)
  library(wordcloud2)
  library(maps)
  library(Cairo)
  library(showtext)
  library(grid)


data<-read.csv('F:\\kaggle数据集\\survey_results_public.csv',header=TRUE)
data1<-data[c(1:10000),]



#基础数据
Basedata<-select(data1,'Gender','Age','Hobby','OpenSource','Country','Student','YearsCoding')

#1、
#性别数据处理
Basedata$Gender<-as.character(Basedata$Gender)
Basedata$Gender<-if_else(str_detect(Basedata$Gender,c('Male','Female')),Basedata$Gender,'Unknow')
Basedata$Gender<-if_else(str_length(Basedata$Gender)>6,'Unknow',Basedata$Gender)

#分析可视化
Sexdata<-data.frame(Basedata$Gender)
colnames(Sexdata)<-'Gender'
Sexdata<-Sexdata%>%na.omit()%>%group_by(Gender)%>%summarise(count=n())
Sexdata<-Sexdata%>%mutate(Fre=count/sum(count))
Sexdata$Fre<-round(Sexdata$Fre,2)

color<-c("#43546C","#8695B2","#D9DBDF")
p<-ggplot(Sexdata,aes(x=Gender,y=count,fill=factor(Gender)))+geom_bar(stat = 'identity')+scale_fill_manual(values = color)+geom_text(aes(label=count),vjust=-0.2)

#饼图
label<-paste0(Sexdata$Gender,'-',Sexdata$Fre*100,'%','-',Sexdata$count)
pie.sales<-Sexdata$Fre
names(pie.sales)<-label
pie(pie.sales,col = color)



#2、年龄分布图
Agedata<-data.frame(Basedata$Age)
colnames(Agedata)<-'Age'
Agedata<-Agedata%>%na.omit()%>%group_by(Age)%>%summarise(agecount=n())

p1<-ggplot(data = Agedata,aes(x=Age,y=agecount))+geom_bar(stat = 'identity')+mytheme+ylim(c(0,4000))+ggtitle('年龄分布情况')+geom_text(aes(label=agecount),vjust=-0.3)



#3、是否对编程感兴趣，是否对开源做过贡献

HobbySourceData<-select(Basedata,Hobby,OpenSource)
Hobbydata<-HobbySourceData%>%count(Hobby)
Opensourcedata<-HobbySourceData%>%count(OpenSource)


color7<-c("#c72e29","#016392","#be9c2e","#098154","#fb832d")
Hobbydata.pie<-Hobbydata$n
names(Hobbydata.pie)<-paste0(c('Hobby_No','Hobby_Yes'),'-',round(Hobbydata$n/sum(Hobbydata$n),3)*100,'%')
pie(Hobbydata.pie,col =c('#be9c2e','#fb832d'),radius = 1,border=F,clockwise=T,init.angle=90)



Opensourcedata.pie<-Opensourcedata$n
names(Opensourcedata.pie)<-paste0(c('Opensource_No','Opensource_Yes'),round(Opensourcedata$n/sum(Opensourcedata$n),3)*100,'%')
pie(Opensourcedata.pie,col = c('#c72e29','#016392'),radius = 1,border=F,clockwise=T,init.angle=90)

  

#4、编程年龄分布情况

YearscodingData<-select(Basedata,YearsCoding)%>%na.omit()%>%count(YearsCoding)

YearscodingData$YearsCoding<-factor(YearscodingData$YearsCoding,levels = c('0-2 years','3-5 years',
'6-8 years','9-11 years','12-14 years','15-17 years','18-20 years','21-23 years','24-26 years','27-29 years','30 or more years'))

p2<-ggplot(data = YearscodingData,aes(x=YearsCoding,y=n))+geom_bar(stat = 'identity',fill='#014d64')+mytheme+ggtitle('编程年龄分布图')+geom_text(aes(label=YearscodingData$n),vjust=-0.3)+xlab('编程年龄')+ylab('记录数')+theme(axis.text.x = element_text(angle = 90))




  
#不同国家刷os的分布情况
options(baidumap.key ='AGn0F7tFeMHn8hCBZ55F88PuqzNxyXzs')

countrydata<-select(Basedata,Country)%>%na.omit()%>%count(Country)

countrydata.top30<-countrydata[order(countrydata$n,decreasing = T),][c(1:10),]

p3<-ggplot(data = countrydata.top30,aes(x=reorder(Country,n),y=n))+geom_bar(stat = 'identity',fill='#014d64')+geom_text(aes(label=n),hjust=-0.3)+mytheme+ggtitle('不同国家刷OS情况（前10）')+ylab('记录数')+coord_flip()




#使用编程语言和想要学习编程语言数据（蝴蝶图）
#数据提取
Language<-data1%>%select(LanguageWorkedWith,LanguageDesireNextYear)%>%na.omit()


LanWork<-Language$LanguageWorkedWith%>%str_split(';')%>%unlist()%>%table()%>%as.data.frame()
colnames(LanWork)<-c('Name','Count')

Nextwork<-Language$LanguageDesireNextYear%>%str_split(';')%>%unlist()%>%table()%>%as.data.frame()
colnames(Nextwork)<-c('Name','Count')


p4<-ggplot(data = LanWork,aes(x=reorder(Name,Count),y=Count))+geom_bar(stat = 'identity',fill='#014d64')+geom_text(aes(label=paste0(round(LanWork$Count/sum(LanWork$Count,3)*100),'%')),hjust=-0.3)+mytheme+coord_flip()+theme(axis.title = element_blank())

p5<-ggplot(data = Nextwork,aes(x=reorder(Name,Count),y=-Count))+geom_bar(stat = 'identity',fill='#014d64')+geom_text(aes(label=paste0(round(LanWork$Count/sum(LanWork$Count,3)*100),'%')),hjust=1)+mytheme+coord_flip()+theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())


CairoPNG(file="butterfly.png",width=1200,height=696)
showtext_begin()
grid.newpage()
pushViewport(viewport(layout=grid.layout(7,11)))
vplayout<-function(x,y){viewport(layout.pos.row =x,layout.pos.col=y)}
print(p5,vp=vplayout(2:7,1:5))
print(p4,vp=vplayout(2:7,6:11))
grid.text(label="Opportunity-to-Deal\nConversion Rate",x=.80,y=.88,gp=gpar(col="black",fontsize=15,fontfamily="myfzhzh",draw=TRUE,just="centre"))
grid.text(label="Lead-to-Opportunity\nConversion Rate",x=.20,y=.88,gp=gpar(col="black",fontsize=15,fontfamily="myfzhzh",draw=TRUE,just="centre"))
grid.text(label="Webinars convert opportunities,but don't close",x=.50,y=.95,gp=gpar(col="black",fontsize=20,fontfamily="myfzhzh",draw=TRUE,just="centre"))
showtext.end()
dev.off()


