#电信客户流失：描述性统计
library(tidyverse)
library(ggthemes)
library(ggthemr)
library(ggtech)
library(reshape2)
library(wordcloud2)
library(easyGgplot2)  #该包用于合并图片
library(Rmisc)



#导入数据集
data<-read.csv('F:\\R\\电信客户流失.csv',header = T,stringsAsFactors = F)

#删除缺失值
data<-na.omit(data)

#可视化操作
#1、是否有家属，是否有伙伴
DependentData<-data$Dependents%>%table()%>%data.frame()%>%mutate(fre=Freq/sum(Freq))
colnames(DependentData)<-c('是否家属','数量','频率')
ggplot(data = DependentData,aes(x=是否家属,y=数量,fill=是否家属))+geom_bar(stat = 'identity')+coord_polar(theta = 'x',start = 0)
#ggplot2画出的饼图并不是很好看
Dpendent.pie<-DependentData$频率
names(Dpendent.pie)<-paste0(c('Depend_no','Depend_yes'),'_',round(DependentData$频率,3)*100,'%')
pie(Dpendent.pie,col = c("#be9c2e","#098154"),radius = 1,border = F,init.angle = 90)

PatnerData<-data%>%count(Partner)

PatnerData.pie<-PatnerData$n
names(PatnerData.pie)<-paste0(c('partner_no','partner_yes'),'_',round(PatnerData$n/sum(PatnerData$n),3)*100,'%')
pie(PatnerData.pie,col = c("#098154","#fb832d"),radius = 1,border = T,init.angle = 90)


#2、客户留在公司的月数
#这里将连续型数据转变为离散数据可以使用cut()函数自定义区间，或者根据fivenum()函数分割，或者进行快速聚类
#cut()自定义分割
Tenuredat<-cut(data$tenure,breaks = c(0,12,24,36,48,60,72),right = F)
TenureData<-Tenuredat%>%table()%>%data.frame()
colnames(TenureData)<-c('Section','Freq')

ggplot(data = TenureData,aes(x=Section,y=Freq))+geom_bar(stat = 'identity',fill='#014d64',width = 0.8)+mytheme+ggtitle('客户留存区间图')+geom_text(aes(label=TenureData$Freq),vjust=-0.3)+xlab('区间分布')+ylab('客户留存数量')+theme(axis.text.x = element_text(angle = 90))


#fivenum()函数分割：
Tenuredat1<-cut(data$tenure,breaks = fivenum(data$tenure),right = F)  #这里可以添加lables标签，类似因子

#kmeans离散化
v<-kmeans(data$tenure,5)
cut(data$tenure,breaks = v$centers,right = F)


#3、对搅动人群进行分组，对比分析
BigData<-data%>%select(gender,Partner,Dependents,PhoneService,Contract,PaymentMethod,Churn)

Gender_data<-BigData%>%select(gender,Churn)%>%group_by(Churn,gender)%>%summarise(count=n())%>%mutate(fre=count/sum(count))
Nochurn_Genderdata<-Gender_data[c(1:2),]
Yeschurn_Genderdata<-Gender_data[c(3:4),]

#对没有churn的客户进行分析：
NoMylabel<-paste0(Nochurn_Genderdata$gender,'-',Nochurn_Genderdata$Churn,'-',round(Nochurn_Genderdata$fre,3)*100,"%")
p1<-ggplot(data = Nochurn_Genderdata,aes(x=gender,y=count,fill=gender))+geom_bar(stat = 'identity')+coord_polar(theta = 'x',start = 0)+theme(axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),legend.position = 'top',panel.border = element_blank(),panel.grid = element_blank(),panel.background = element_blank(),plot.title = element_text(hjust = 0.5))+scale_fill_economist(labels=NoMylabel)+guides(fill=guide_legend(title = NULL))+ggtitle('NO-tunrn 用户对比')

#对有churn的客户进行分析：
YesMylabel<-paste0(Yeschurn_Genderdata$gender,'-',Yeschurn_Genderdata$Churn,'-',round(Yeschurn_Genderdata$fre,3)*100,'%')
p2<-ggplot(data=Yeschurn_Genderdata,aes(x=gender,y=count,fill=gender))+geom_bar(stat = 'identity')+coord_polar(theta = 'x',start = 0)+HX_mytheme+scale_fill_economist(labels=YesMylabel)+guides(fill=guide_legend(title = NULL))+ggtitle('Yes-tunrn用户对比')

ggplot2.multiplot(p1,p2,cols = 2)


#4\按Churn分类
Churndata<-filter(data,Churn=='Yes')
NoChurndata<-filter(data,Churn=='No')


#internetservice使用形式
YesInternetData<-select(Churndata,InternetService)%>%count(InternetService)%>%data.frame()
colnames(YesInternetData)<-c('InternetService','Yes_churn')
NoInternetData<-select(NoChurndata,InternetService)%>%count(InternetService)%>%data.frame()
colnames(NoInternetData)<-c('InternetService','no_churn')
#进行数据框合并,并且长转宽
InternetData<-right_join(YesInternetData,NoInternetData,by="InternetService")%>%gather(tyoe,count,-InternetService)
InternetPlot<-ggplot(data=InternetData,aes(x=InternetService,y=count,fill=tyoe))+geom_bar(stat = 'identity',position = 'dodge',width = 0.8)+theme_economist()+scale_fill_economist()+geom_text(aes(label=count),vjust=-0.3,position =position_dodge(0.8))+guides(fill=guide_legend(title = NULL))


#5、根据合同contract对比
ContractData<-select(data,Churn,Contract)%>%group_by(Churn,Contract)%>%summarise(Count=n())

Contractplot<-ggplot(data = ContractData,aes(x=Contract,y=Count,fill=Churn))+geom_bar(stat = 'identity',position = 'dodge',width = 0.8)+theme_tech(theme = 'facebook')+scale_fill_tech(theme = 'facebook')+geom_text(aes(label=Count),vjust=-.3,position = position_dodge(0.8))+ggtitle('客户合同分布情况')


#paymentmethod分布情况

PaymentData<-select(data,Churn,PaymentMethod)%>%group_by(Churn,PaymentMethod)%>%summarise(count=n())

Noachurn_Paydata<-PaymentData[c(1:4),]
Yeschurn_paydata<-PaymentData[c(5:8),]

pctno<-round(Noachurn_Paydata$count/sum(Noachurn_Paydata$count)*100,1)
libs<-paste0(Noachurn_Paydata$PaymentMethod,'-',pctno,'%')
Nopaymentplot<-pie(Noachurn_Paydata$count,labels = libs,col = c("#3b5998","#6d84b4", "#afbdd4", "#d8dfea"),radius = 1,cex=1.2,main = 'Churn-no-paymentmethod')

pctno1<-round(Yeschurn_paydata$count/sum(Yeschurn_paydata$count)*100,1)
libs1<-paste0(Yeschurn_paydata$PaymentMethod,'-',pctno1,'%')
Yespaymentplot<-pie(Yeschurn_paydata$count,labels = libs1,col = c("#3b5998","#6d84b4", "#afbdd4", "#d8dfea"),radius = 1,cex=1.2,main = 'Churn-Yes-paymentmethod')




