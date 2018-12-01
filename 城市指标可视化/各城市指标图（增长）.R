#标出各省指标（或者在企业在每个城市的业绩等）在中国地图上的显示
library(maptools)
library(ggplot2)
library(plyr)
library(ggmap)
library(dplyr)

#导入中国地图和各省会之间的经纬度等数据
china_shp<-readShapePoly('E:/R WORK SPACE/ggplot2包绘图地图/中国地图/bou2_4p.shp')
china_map<-fortify(china_shp)

#各城市城市经纬度数据
provice<-mapNames('china')
provicev_list<-get_geo_position(provice)

#模拟企业的指标或各省的指标，这里随机取出10个撑死
procvice<-provicev_list[sample(nrow(provicev_list),10),]
procvice<-mutate(procvice,N15=runif(10,min=500,max=1000),N16=runif(10,600,1100),Ratio=round((N16-N15)/N15,3))
colnames(procvice)<-c('long','lat','city','N15','N16','Ratio')

#将经纬度转化为字符型变量
procvice$long<-as.numeric(procvice$long)
procvice$lat<-as.numeric(procvice$lat)

#画底图也就是地图
p1<-ggplot()+geom_polygon(aes(x=long, y=lat, group=group),data=china_map, fill="white", colour="grey60")+ylim(c(15,60))

#添加图层
p2<-p1+geom_linerange(data=procvice,aes(x=long-0.5,ymin=lat,ymax=lat+N15/max(N15,N16)*5),size=3,color="orange",alpha=0.8)+geom_linerange(data=procvice,aes(long+0.5,ymin=lat,ymax=lat+N16/max(N16,N15)*5),size=3,color='blue')

#在p2的基础上，美化柱形图，添加文字说明
p3<-p2+geom_text(data = procvice,aes(x=long,y=lat-0.6),label=paste0(procvice$city,ifelse(procvice$Ratio>0,'▲','▼'),procvice$Ratio*100,'%'),size=3)

#在p3的基础上添加图例
p4<-p3+annotate("text", x=105, y=52, label="● 2015", color= "orange", size=8)+ annotate("text", x=105, y=49, label="● 2016", color= "blue", size=8)

#在p4的基础上去除坐标轴
p5<-p4+theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank())


